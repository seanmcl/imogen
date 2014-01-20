
structure Command :> Command = struct
   structure C = CommandLine
   structure O = OS.Process
   open General
   open PP.Ops

   exception Command of string

   structure Flag = struct
      datatype kind =
         Noarg of unit -> unit
       | String of string -> unit
       | Int of int -> unit
       | Set of bool ref
       | Unset of bool ref
       | SetString of string ref
       | SetInt of int ref

      datatype t = T of
         { name : string
         , kind : kind
         , doc : string }

      fun compare (T { name, ... }, T {name = name', ...}) =
         String.compare (name, name')

      fun splitDoc (T { doc, ... }) =
         case String1.index doc #" " of
            SOME n =>
            (String.substring (doc, 0, n),
             String.substring (doc, n+1, String.size doc - n - 1))
          | NONE => ("", doc)

      fun name (T { name, ... }) = name

      type 'a create =
         { name : string
         , process : 'a
         , doc : string }
         -> t

      fun noarg { name, process, doc } =
         T { name = name, kind = Noarg process, doc = doc }

      fun string { name, process, doc } =
         T { name = name, kind = String process, doc = doc }

      fun int { name, process, doc } =
         T { name = name, kind = Int process, doc = doc }

      fun set { name, process, doc } =
         T { name = name, kind = Set process, doc = doc }

      fun unset { name, process, doc } =
         T { name = name, kind = Unset process, doc = doc }

      fun setString { name, process, doc } =
         T { name = name, kind = SetString process, doc = doc }

      fun setInt { name, process, doc } =
         T { name = name, kind = SetInt process, doc = doc }
   end

   structure F = Flag

   structure Alias = struct
      datatype 'a t = Ok of 'a | None | Ambiguous

      fun find f ts s =
         case List.find (fn t => f t = s) ts of
            SOME t => Ok t
          | NONE =>
            case List.filter (fn t => String.isPrefix s (f t)) ts of
               [] => None
             | [t] => Ok t
             | _ :: _ :: _ => Ambiguous
   end

   structure A = Alias

   datatype t =
      Base of
      { readme : unit -> PP.t
      , summary : string
      , usageArg : string
      , flags : Flag.t list
      , run : { anons : string list } -> OS.Process.status }
    | Group of
      { readme : unit -> PP.t
      , summary : string
      , subs : (string * t) list }

   val create = Base
   val group = Group

   val summary = fn
      Base { summary, ... } => summary
    | Group { summary, ... } => summary

   val path : string list ref = ref []

   fun name () = $(List.last (String1.chop [#"/"] (CommandLine.name ())))

   fun ppPath () =
      %[ name (), \, %(map (fn s => %[$s, \ ]) (rev (!path))) ]

   fun pad col s = (StringCvt.padRight #" " col s)

   val help = fn
      Base { readme, flags, usageArg, ... } =>
      let
         val flags = List1.sort Flag.compare flags
         fun len flag =
            String.size (F.name flag)
               + 1 + String.size (fst (Flag.splitDoc flag))
         val padlen = 2 + Int1.maxList (List.map len flags)
         fun flag (t as Flag.T { name, ... }) =
            let val (arg, doc) = F.splitDoc t in
               %[$(pad padlen ("-" ^ name ^ " " ^ arg)), $doc]
            end
      in
         &[ ~
          , readme ()
          , ~
          , %[$"Usage: ", ppPath (), $usageArg]
          , ~
          , $"=== flags ==="
          , ~
          , %[PP.spaces 2, &(List.map flag flags)]
          , ~ ]
      end
    | Group { readme, subs, ... } =>
      let
         val padlen = 2 + Int1.maxList (List.map (String.size o #1) subs)
         val summary = fn (name, t) => %[$(pad padlen name), $(summary t)]
         val subs =
            List1.sort (fn ((x, _), (y, _)) => String.compare (x, y)) subs
      in
         &[ ~
          , readme ()
          , ~
          , $"=== subcommands ==="
          , ~
          , %[PP.spaces 2, &(List.map summary subs )]
          , ~
          , $"For help on a subcommand, use imogen [subcmd ...] -help"
          , ~ ]
      end

   fun fail (_, msg) = raise Command ("Error: " ^ msg)

   fun run (t, args) = case (t, args) of
      (Group _, []) => (PP.ppl (help t); O.success)
    | (t, "-help" :: _) => (PP.ppl (help t); O.success)
    | (t, "--help" :: _) => (PP.ppl (help t); O.success)
    | (Group { subs, ... }, arg :: args) =>
      let in
         case A.find fst subs arg of
            A.None => fail (t, "There is no subcommand named " ^ arg)
          | A.Ambiguous => fail (t, "Ambiguous subcommand")
          | A.Ok (_, t) =>
            let in
               path := arg :: (!path)
             ; run (t, args)
            end
      end
    | (Base { flags, run, ... }, args) =>
      let
         fun match s = case A.find (fn t => "-" ^ Flag.name t) flags s of
            A.Ambiguous => fail (t, "Ambiguous flag")
          | A.None => NONE
          | A.Ok t => SOME t
         val anons = ref []
         fun process args =
            let
               val p : string * (string -> unit) * string list -> unit =
                  fn (name, f, args) =>
                     case args of
                        [] => fail (t, name ^ "expects an argument")
                      | arg :: args => (f arg; process args)
            in
               case args of
                  [] => ()
                | arg :: args =>
                  case match arg of
                     NONE =>
                     let in
                        anons := arg :: (!anons)
                      ; process args
                     end
                   | SOME (Flag.T { name, kind, ... }) =>
                     let in
                        case kind of
                           F.Noarg f => (f (); process args)
                         | F.Set b => (b := true; process args)
                         | F.Unset b => (b := false; process args)
                         | F.String f => p (name, f, args)
                         | F.Int f => p (name, f o valOf o Int1.ofString, args)
                         | F.SetString s => p (name, (fn s' => s := s'), args)
                         | F.SetInt s =>
                           p ( name,
                              (fn s' => s := valOf (Int1.ofString s')),
                              args )
                     end
            end
      in
         process args
       ; run { anons = rev (!anons) }
      end

   fun runArgs t = O.exit (run t)

   fun run t =
      O.exit (runArgs (t, C.arguments ()))
      handle Command msg =>
         let in
            PP.ppl (&[ $msg
                     , ~
                     , help t ])
          ; O.exit O.failure
         end
end

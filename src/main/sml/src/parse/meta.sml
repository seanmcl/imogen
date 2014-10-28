
structure Meta :> Meta = struct
   open General
   open PP.Ops

   fun fail s = failwith ("Meta.parse: " ^ s)

   structure Pre = struct
      datatype t = Theorem | NonTheorem | Open | Unknown

      fun ofString s = case String.downcase s of
         "theorem" => Theorem
       | "nontheorem" => NonTheorem
       | "non-theorem" => NonTheorem
       | "open" => Open
       | "unknown" => Unknown
       | _ => Unknown

      val toString = fn
         Theorem => "Theorem"
       | NonTheorem => "NonTheorem"
       | Open => "Open"
       | Unknown => "Unknown"
      val pp = PP.string o toString
   end

   structure Mode = struct
      datatype t = K | T | B | K4 | S4 | S5 | P
      fun ofString s = case String.downcase s of
         "k" => K
       | "t" => T
       | "b" => B
       | "k4" => K4
       | "s4" => S4
       | "s5" => S5
       | "p" => P
       | _ => fail ("unknown mode: " ^ s)
      val toString = fn
         K => "K"
       | T => "T"
       | B => "B"
       | K4 => "K4"
       | S4 => "S4"
       | S5 => "S5"
       | P => "P"
      val pp = $ o toString
   end

   structure Status = struct
      type t = Pre.t * Mode.t list

      fun pre m l = SOME (Pre.ofString (String.concat l), m)

      fun mods l = case List.index (fn s => s = "]") l of
         NONE => fail "expected closing bracket"
       | SOME n =>
         let
            val r = List.drop (l, n+1)
            val l = List.take (l, n)
            val r = case r of
               ":" :: r => r
             | s :: _ => fail ("expected : found " ^ s)
             | []  => fail ("expected : found nothing")
            val ms =
               let
                  fun loop acc = fn
                     [] => acc
                   | [m] => Mode.ofString m :: acc
                   | m :: "," :: ms =>
                     loop (Mode.ofString m :: acc) ms
                   | _ => fail "malformed list"
               in
                  loop [] l
               end
         in
            pre ms r
         end

      fun parse line =
         let
            val toks = Tokens.lex line
         in
            case map String.downcase toks of
               "%" :: "status" :: ":" :: t => pre [] t
             | "%" :: "status" :: "(" :: "intuit" :: "." :: ")" :: ":" :: t =>
               pre [] t
             | "%" :: "status" :: "[" :: t => mods t
             | _ => NONE
         end
      fun pp (p, ms) = %%[Pre.pp p, PP.list (map Mode.pp ms)]
   end

   structure Rating = struct
      type t = real

      fun parse line = case Tokens.lex line of
         "%" :: "Rating" :: ":" :: r => Real.fromString (String.concat r)
       | "%" :: "Rating" :: "(" :: "intuit" :: "." :: ")" :: ":" :: r =>
         Real.fromString (String.concat r)
       | _ => NONE

      val pp = PP.real
   end

   datatype one =
      Status of Status.t
    | Rating of Rating.t

   fun parse line = case Status.parse line of
      SOME s => SOME (Status s)
    | NONE =>
      case Rating.parse line of
         SOME r => SOME (Rating r)
       | NONE => NONE

   type t = one list
   type printable = t

   (* Grab the last status/rating data.  They are stored in reverse order. *)
   fun rating l = List.findMap (fn Rating s => SOME s | _ => NONE) l
   fun status l = List.findMap (fn Status s => SOME s | _ => NONE) l

   fun pp (l:printable) =
      let
         fun option pp f x = case f x of
            NONE => $"None"
          | SOME y => pp y
      in
         &[ $"Metadata: ",
           %[\\, &[ %[$"Status: ", option Status.pp status l]
                  , %[$"Rating: ", option Rating.pp rating l]]]]
      end

   val nonempty : t -> bool = not o null

   (* the header if the lines of the file up to the first
      non-comment line *)
   fun header s =
      let
         fun getHeaderLine () =
            case Option.map String.strip (TextIO.inputLine s) of
               NONE => NONE
             | SOME "" => SOME ""
             | SOME s =>
               if String.sub (s, 0) = #"%" then SOME s else NONE
         fun loop acc = case getHeaderLine () of
            NONE => acc
          | SOME s => loop (s :: acc)
      in
         loop []
      end

   fun ofString s = List.mapPartial parse (header (TextIO.openString s))
   fun ofFile f = List.mapPartial parse (header (TextIO.openIn f))
   fun ofStdin () = List.mapPartial parse (header TextIO.stdIn)

   val _ = (rating, ofStdin)
end



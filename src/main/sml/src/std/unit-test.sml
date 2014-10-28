
structure UnitTest :> UnitTest = struct

   structure List = List1
   structure S = String1

   open PP.Ops

   exception Fail of string

   datatype t =
      Unit of unit -> unit
    | List of t list
    | Label of string * t

   (* Nodes are for determining the location of a failure. *)
   structure Node = struct
      datatype t  =
         Index of int
       | Label of string

      val toString = fn
         Index n => Int.toString n
       | Label s => s
   end

   structure Path = struct
      (* A path is simply a list of nodes, from test case to root. *)
      type t = Node.t list

      fun toString (l:t) =
         S.concat (List.separate ":" (map Node.toString (rev l)))
   end

   fun fail msg =
      let in
         print (msg ^ "\n")
       ; raise Fail msg
      end

   fun assertMsg f = Unit (fn () =>
      case f () of
         (true, _) => ()
       | (false, msg) => fail msg)

   fun assert f = assertMsg (fn () => (f (), "assertion failure"))

   fun raises f =
      Unit (fn () =>
         let in
            Debug.print_error_on_failure := false
          ; if (ignore (f ()); false) handle _ => true then
               ()
            else
               fail "expected exception was not raised"
          ; Debug.print_error_on_failure := true
         end)

   fun ok f = Unit (fn () => ignore (f ()))

   val rec count = fn
      Unit _ => 1
    | List l => foldr (fn (t, acc) => count t + acc) 0 l
    | Label (_,t) => count t

   local
      fun time f x =
         let
            val timer = Timer.startRealTimer ()
            val result = f x
            val time = Timer.checkRealTimer timer
         in
            (result,time)
         end

      fun limit timeOut f x = case timeOut of
         SOME n => time (TimeLimit.timeLimit n f) x
       | NONE => time f x
   in
      fun doit (opts as {verbose, timeout}) path = fn
         Unit f =>
         let in
            let
               val (_, time) = limit timeout f ()
            in
               ((1,0,0), time)
            end
            handle Fail msg =>
               let in
                  PP.ppl (&[ %[$"### Failure in: ", $(Path.toString path)]
                           , $msg ])
                ; ((1,1,0), Time.zeroTime)
               end
             | exn =>
               let in
                  PP.ppl (&[%[$"### Error in: ", $(Path.toString path)],
                            %[$"exception = ",$(exnName exn)]])
                ; ((1,0,1), Time.zeroTime)
               end
         end
       | List ts =>
         let
            fun foldFn (i, test, ((total, failures, errors), time)) =
               let
                  val ((t, f, e), m) = doit opts (Node.Index i :: path) test
               in
                  ((total + t, failures + f, errors + e), Time.+(time, m))
               end
         in
            List.foldli foldFn ((0,0,0), Time.zeroTime) ts
         end
       | Label (name,t) =>
         let
            val indent = S.space ((length path div 2) * 3)
            val _ =
               if not verbose then () else
               print (indent ^ "[Running: " ^ name ^ "]\n")
            val res as (_, time) = doit opts (Node.Label name :: path) t
         in
            if not verbose then () else
            print
               (String.concat
                   [ indent,   "[Done   : ", name, " (", Time.fmt 3 time, ")]\n"])
          ; res
         end
   end

   fun runGen opts test =
      let
         val ((t,f,e), time) = doit opts [] test
         val fmt = &[ ~
                    , %[$"Cases    : ",PP.int (count test)]
                    , %[$"Tried    : ",PP.int t]
                    , %[$"Failures : ",PP.int f]
                    , %[$"Errors   : ",PP.int e]
                    , %[$"Time     : ",PP.time time]
                    , ~]
      in
         PP.ppl fmt
       ; (t, f, e)
      end

   fun run' opts t = ignore (runGen opts t)
   fun run t = run' {verbose = true, timeout = NONE} t

   structure Ops = struct
      val $ = Label
      val % = assert
      val & = List
   end

   fun makeCommand t =
      let
         val summary = "Run unit tests."
         fun readme () = $summary
         fun run _ =
            let
               val (_, f, e) = runGen {verbose = true, timeout = NONE} t
            in
               if f + e > 0 then OS.Process.failure else OS.Process.success
            end
      in
         Command.create
            { readme = readme
            , summary = summary
            , usageArg = ""
            , flags = []
            , run = run }
      end

end

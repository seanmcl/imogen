
structure Debug :> Debug = struct
   open PP.Ops

   val assertP = ref true
   val traceP = ref false

   fun printl s = print (s ^ "\n")

   structure Fail = struct
      val print_error_on_failure = ref true

      fun failwith' p =
         let open PP.Ops in
            if !print_error_on_failure then PP.ppl (%[$"Failure: ", p])
            else ()
          ; raise Fail "failwith'"
         end

      fun failwith s = failwith' (PP.text s)
   end
   open Fail

   structure Assert = struct
      fun assert (f, g) =
         if not (!assertP) then () else
         if f () then () else
         let in
            PP.ppl (g ())
          ; raise Fail "Assert.assert"
         end

      fun asserts (f, s) = assert (f, fn () => $s)

      fun assert' f =
         if not (!assertP) orelse f ()
         then ()
         else raise Fail "Assert.assert'"

      fun uassert f =
         assert' (fn () => ((f () : unit; true) handle _ => false))
   end

   structure Trace : sig
      val trace
         : string -> ('a -> 'b) -> ('a -> 'b)

      val traceArgs
         :  string * ('a -> PP.t) * ('b -> PP.t)
         -> ('a -> 'b)
         -> ('a -> 'b)
   end = struct
      val _ = if !traceP
              then printl "Warning: Tracing is on."
              else ()
      local
         val depth = ref 0
      in
         fun indent () = Ref.incr depth
         fun outdent () = Ref.decr depth
         fun printCol () = !depth
      end (* local *)

      fun handleExn (name, exn) =
         let in
            outdent ()
          ; PP.pp (%[PP.spaces (2 + printCol()), $name, $" raises ", $(exnName exn)])
          ; raise exn
         end

      fun trace name f x =
         if not (!traceP) then f x else
         let
            val col = printCol()
            val inp = %[PP.spaces col, $name, $" <-- "]
            val _ = PP.pp inp
            val _ = indent()
            val res = f x
            val outp = %[PP.spaces col, $name, $" --> "]
         in
            outdent()
          ; PP.pp outp
          ; res
         end
         handle exn => handleExn(name, exn)

      fun traceArgs (name, ppA, ppB) f x =
         if not (!traceP) then f x else
         let
            val col = printCol()
            val top = %[PP.spaces col, $name, $" <-- ", ppA x]
            val _ = PP.pp top
            val _ = indent()
            val res = f x
            val bot = %[PP.spaces col, $name, $" --> ", ppB res]
         in
            outdent()
          ; PP.pp bot
          ; res
         end
         handle exn => handleExn(name, exn)
   end

   open Assert
   open Trace

end

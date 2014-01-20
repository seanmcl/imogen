
structure WithReturn :> WithReturn = struct
   type ('a, 'b) return = 'a -> 'b

   val f : (('a, 'b) return -> 'a) -> 'a =
      fn g => let
         exception Return
         val r = ref NONE
         fun return x = (r := SOME x; raise Return)
      in
         let
            val rval = g return
         in
            case !r of
               NONE => rval
             | SOME _ =>
               raise
                  (Fail "withReturn exited normally despite return \
                        \ being called")
         end
         handle
            Return =>
            case !r of
               NONE => raise (Fail "Impossible")
             | SOME x => x
      end
end

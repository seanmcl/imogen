
structure Clock :> Clock = struct

   type t = int * int ref

   (* It's important to start with a value >= 1.  We use a negation encoding of
    fixing variables, and we shouldn't have a variable that can't be
    fixed.  *)
   fun new n =
      if n >= 1 then (n, ref n)
      else raise Fail "Invariant:  start time must be >= 1"

   fun tick (_, t) = Ref.incr t
      handle Overflow =>
         let in
            print "The clock out of ticks!\n"
          ; raise Overflow
         end

   fun time (_, t) = !t
   fun compare ((_, ref x), (_, ref y)) = Int.compare (x, y)
   fun reset (n, t) = t := n

end

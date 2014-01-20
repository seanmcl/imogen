
structure Timing :> Timing = struct
   type center = unit
   type sum = unit
   fun init _ = ()
   fun newCenter _ = ()
   fun reset _ = ()
   fun time _ f x = f x
   fun sumCenter _ = ()
   fun pp _ = PP.text "Timing is off"
   val ppSum = pp
end

structure Counting :> Timing = Timing

structure TimeLimit : sig
   exception TimeOut
   val timeLimit : Time.time -> ('a -> 'b) -> 'a -> 'b
end = struct
   exception TimeOut
   val _ = TimeOut
   fun timeLimit _ f x = f x
end

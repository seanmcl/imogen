
structure IntHashSet = HashSetFn (struct
   type t = int
   val eq = op=
   val hash = Word1.ofInt
   val pp = PP.int
end)

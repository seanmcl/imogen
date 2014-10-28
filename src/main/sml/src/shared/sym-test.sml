
structure SymTest : Test = struct
   structure P = Param
   structure T = UnitTest

   open General
   open UnitTest.Ops

   val t0 = T.assert (fn () => P.eq (P.ofString "@c", P.ofString "@c"))
   val t1 = T.assert (fn () => P.eq (P.ofString "@c#", P.ofString "@c#"))
   val t2 = T.assert (fn () => P.eq (P.ofString "@c#", P.fix (P.ofString "@c")))
   val t3 = T.assert (fn () => P.eq (P.ofString "@c", P.unfix (P.ofString "@c#")))

   val test = &[ t0, t1, t2, t3 ]
end

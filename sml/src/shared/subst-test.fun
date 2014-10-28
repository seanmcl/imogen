
functor SubstTestFn(Subst : Subst) : Test = struct
   structure S = Subst
   structure T = UnitTest
   structure VSet = Var.Set
   structure PSet = Param.Set

   open General
   open UnitTest.Ops

   infix == ===

   val s = Subst.ofString
   val v = Var.ofString
   val t = Term.ofString
   val a = Param.ofString
   fun exV (th, x, t) = Subst.extend (th, Left (x, t))
   fun exP (th, a, b) = Subst.extend (th, Right (a, b))
   fun op* (t1, t2) = Subst.compose (t1, t2)

   fun s == t = Subst.eq (s, t)
   fun s === t = Atoms.eq (s, Atoms.make t)

   val t0 = %(fn () => s "{X -> X, Y -> Y, @a -> @a}" == S.id)
   val t1 = T.raises (fn () => exV (s "{X -> c}", v "X", t "d"))
   val t2 = %(fn () =>
      S.compose (s "{X -> c}", s "{Y -> c}") == s "{X -> c, Y -> c}")
   val t3 = %(fn () =>
      S.compose (s "{X -> Y}", s "{Y -> Z}") == s "{X -> Z, Y -> Z}")
   val t4 = T.raises (fn () => exP (s "{@a -> @b}", a "@a", a "@c"))
   val t5 = T.ok (fn () => exP (s "{@a -> @b}", a "@a", a "@b"))
   val t6 = T.raises (fn () => exV (s "{}", Var.fix (v "X"), t "c"))
   val t7 = T.raises (fn () => s "{X -> f(X)}")
   val t8 = %(fn () => s "{X -> c}" * s "{X -> d}" == s "{X -> c}")
   val t9 = T.raises (fn () => s "{@a -> X}")
   val t10 = %(fn () => Subst.img (s "{@a -> @b}") === (VSet.empty, PSet.ofList [a "@b"]))
   val t11 = %(fn () => Subst.img (s "{X -> @b}") === (VSet.empty, PSet.ofList [a "@b"]))
   val t12 = %(fn () => Subst.img (s "{@a -> @b, X -> f(@b, c, Y)}") === (VSet.ofList [v "Y"], PSet.ofList [a "@b"]))

   val test = &[ t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12 ]
end

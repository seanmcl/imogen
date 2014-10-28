
functor CUnifTestFn (CUnif : CUnif) : Test = struct
   structure T = UnitTest
   structure U = CUnif

   open General
   open UnitTest.Ops

   infix == << !<<

   fun s == t = Subst.eq (s, t)

   val s = Subst.ofString
   val c = CFormula.parse
   (* val v = Term.Var o Var.ofString *)
   (* val t = Term.ofString *)
   (* val a = Term.Param o Param.ofString *)

   val vars =
      map Var.ofString
         ["X", "Y", "Z", "W", "X0", "X1", "X2", "X3", "X4", "X5", "X6"
         , "X7", "X8", "X9", "X10", "X11"]

   val params =
      map Param.ofString
         ["@a", "@b"]

   val atoms = Atoms.make (Var.Set.ofList vars, Param.Set.ofList params)

   fun t1 << t2 = U.unify (s t1, s t2, atoms)

   fun t1 !<< t2 = not (t1 << t2)

   fun l eqs = valOf (Unif.unify eqs)

   val t0 = %(fn () => s "{X -> c}" + s "{X -> c}" == s "{X -> c}" )

   val test = &[ t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13
               , t14, t15, t16, t17, t18, t19, t20, t21, t22, t23  ]
end

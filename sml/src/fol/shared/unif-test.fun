
functor UnifTestFn (Unif : Unif) : Test = struct
   structure T = UnitTest
   structure U = Unif

   open General
   open UnitTest.Ops

   infix == << !<<

   val s = Subst.ofString
   val v = Term.Var o Var.ofString
   val t = Term.ofString
   val a = Term.Param o Param.ofString
   fun op+ (t1, t2) = valOf (U.plus (t1, t2))
   (* fun op* (t1, t2) = S.compose (t1, t2) *)

   fun s == t = Subst.eq (s, t)

   val vars =
      map Var.ofString
         ["X", "Y", "Z", "W", "X0", "X1", "X2", "X3", "X4", "X5", "X6"
         , "X7", "X8", "X9", "X10", "X11"]

   val params =
      map Param.ofString
         ["@a", "@b"]

   val atoms = Atoms.make (Var.Set.ofList vars, Param.Set.ofList params)

   fun t1 << t2 = Unif.subsumes (s t1, s t2, atoms)

   fun t1 !<< t2 = not (t1 << t2)

   fun l eqs = valOf (Unif.unify eqs)

   val t0 = %(fn () => s "{X -> c}" + s "{X -> c}" == s "{X -> c}" )
   val t1 = T.raises (fn () => s "{X -> c}" + s "{X -> d}")
   (* fixed variables can't be in the domain *)
   val t2 = T.raises (fn () => s "{X# -> c}")
   val t3 = %(fn () => l [(v "X", a "@a")] == s "{X -> @a}")
   val t4 = %(fn () => l [(a "@a", a "@b")] == s "{@a -> @b}")
   (* fixed params don't unify with anything but themselves*)
   val t5 = %(fn () => l [(a "@a#", a "@a#")] == s "{}")
   val t6 = %(fn () => l [(a "@a#", a "@b")] == s "{@b -> @a#}")
   val t7 = %(fn () => s "{X -> f(Y), Z -> c}" + s "{Y -> Z}"
                          == s "{X -> f(c), Y -> c, Z -> c}")
   val t8 = %(fn () => "{}" <<  "{}")
   val t9 = %(fn () => "{}" <<  "{X -> c, @a -> @b}")
   val t10 = %(fn () => "{X -> c}" !<< "{}")
   val t11 = %(fn () => "{X -> c}" !<< "{X -> d}")
   val t12 = %(fn () => "{X -> Z}" !<< "{Y -> Z}")
   val t13 = %(fn () => "{X -> Y}" !<< "{X -> Z}")
   val t14 = %(fn () => "{X -> Y}" << "{X -> Y}")
   val t15 = %(fn () =>
      "{X -> X1,X2 -> X3,X4 -> X1,X5 -> X1,X6 -> X8,X7 -> X9,X10 -> X11}"
      << "{X -> X1,X2 -> X3,X4 -> X1,X5 -> X1,X6 -> X8,X7 -> X9,X10 -> X11}")
   val t16 = T.assert (fn () => Unif.variant (t "f(X)", t "f(Y)"))
   val t17 = T.assert (fn () => Unif.unifiable (t "f(X, @a)", t "f(Y, @b)"))
   val t18 = T.assert (fn () => Unif.variant (t "f(X, @a)", t "f(Y, @b)"))
   val t19 = %(fn () => "{X2 -> X1,X3 -> X}" << "{X -> X1,X2 -> X1,X3 -> X1}")
   val t20 = %(fn () => "{X -> Z}"  << "{X -> Y, Z -> Y}")
   val t21 = %(fn () => "{X -> Y}" !<< "{X -> Z, Y -> W}")
   val t22 = %(fn () => "{X -> @a}" << "{X -> @a}")
   val t23 = %(fn () => "{X -> @a#}" << "{X -> @a#}")

   val test = &[ t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13
               , t14, t15, t16, t17, t18, t19, t20, t21, t22, t23  ]
end

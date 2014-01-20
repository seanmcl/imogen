
functor ParseTestFn(Parse : Parse) : Test = struct
   open Parse
   structure T = UnitTest
   open UnitTest.Ops
   infix ==

   val v = Var.ofString
   val a = Param.ofString
   val p = Pred.ofString
   val f = Func.ofString

   val t0 = T.ok (fn () => v "X")
   val t1 = T.raises (fn () => v "x")
   val t2 = T.assert (fn () => not (Var.isFixed (v "X")))
   val t3 = T.assert (fn () => Var.isFixed (v "X#"))

   val t4 = T.ok (fn () => a "@a")
   val t5 = T.raises (fn () => a "a")
   val t6 = T.raises (fn () => a "A")
   val t7 = T.assert (fn () => not (Param.isFixed (a "@a")))
   val t8 = T.assert (fn () => Param.isFixed (a "@a#"))

   val t9 = T.ok (fn () => p "p")
   val t10 = T.ok (fn () => p "P")
   val t11 = T.raises (fn () => p "@p")
   val t12 = T.raises (fn () => p "p#")
   val t13 = T.assert (fn () => Pred.pos (p "p"))
   val t14 = T.assert (fn () => Pred.neg (p "P"))

   val t15 = T.ok (fn () => f "c")
   val t16 = T.ok (fn () => f "c'")
   val t17 = T.raises (fn () => f "F")
   val t18 = T.raises (fn () => f "c#")
   val t19 = T.raises (fn () => f "@c")

   val test = &[ t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10
               , t11, t12, t13, t14, t15, t16, t17, t18, t19 ]
end

structure Test : Test = struct
   open UnitTest.Ops
   structure Parse = ParseTestFn (Parse1)
   val test = $("Parse", Parse.test)
end

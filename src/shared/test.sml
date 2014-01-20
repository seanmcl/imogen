
structure Test : Test = struct
   open UnitTest.Ops

   structure Subst = SubstTestFn(Subst)

   val test =
      $("Shared", &[ $("Sym", SymTest.test)
                   , $("Subst", Subst.test)
                   ])
end

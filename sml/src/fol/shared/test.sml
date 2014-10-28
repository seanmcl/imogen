
structure SharedTest : Test = struct
   open General
   open UnitTest.Ops

   structure Unif = UnifTestFn(Unif)
   structure ListTermIndex = TermIndexTestFn(ListIndex)
   structure PathTermIndex = TermIndexTestFn(PathIndex)
   structure ListIndex = IndexFn(ListIndex)
   structure ListIndex = IndexTestFn(ListIndex)
   structure PathIndex = IndexFn(PathIndex)
   structure PathIndex = IndexTestFn(PathIndex)

   val test =
      $("Shared", &[ $("Unif", Unif.test)
                   , $("ListTermIndex", ListTermIndex.test)
                   , $("PathTermIndex", PathTermIndex.test)
                   , $("ListIndex", ListIndex.test)
                   , $("PathIndex", PathIndex.test)
                   ])

   val () = noWarnUnused (fn _ : RuleListIndex.t => ())
end

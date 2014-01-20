
structure Test : Test = struct
   open UnitTest.Ops

   val test =
      $("Fol", &[ SharedTest.test
                , Cascade.Test.test
                , Partial.Test.test
                ])
end

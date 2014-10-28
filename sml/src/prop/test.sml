
structure Test : Test = struct
   open UnitTest.Ops

   val test =
      $("Prop", &[ Cascade.Test.test
                 , Partial.Test.test
                 ])
end


structure Test : Test = struct
   open UnitTest.Ops

   val test =
      $("Partial", &[])
end

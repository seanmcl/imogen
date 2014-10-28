
structure Test : Test = struct
   open UnitTest.Ops

   val test =
      $("Cascade", &[])
end

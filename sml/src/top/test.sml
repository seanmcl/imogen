
structure Test = struct
   val test =
      let open UnitTest.Ops in
         &[ Parse.Test.test
          , Shared.Test.test
          , Prop.Test.test
          , Fol.Test.test
          , Constr.Test.test
          ]
      end
end

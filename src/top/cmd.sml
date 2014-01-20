
structure Cmd :> Cmd = struct
   structure C = Command
   open PP.Ops

   fun readme () =
      &[ $"Imogen: a polarized inverse method theorem prover." ]

   val summary = "The Imogen theorem prover"

   val subs = [ ("prop", CmdProp.cmd)
              , ("fol", CmdFol.cmd)
              , ("modal", CmdModal.cmd)
              , ("linear", CmdLinear.cmd)
              , ("ordered", CmdOrdered.cmd)
              , ("test", UnitTest.makeCommand Test.test)
              ]

   val cmd = C.group
                { readme = readme
                , summary = summary
                , subs = subs }
end

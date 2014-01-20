
structure CmdLinear :> Cmd = struct
   structure C = Command

   open PP.Ops

   structure Show =
      CmdShowFn (val summary = "Show a linear logic formula"
                 structure Frontend = Constr.Provers.Linear.Frontend)

   structure Prove =
      CmdProveFn (val mode = NONE
                  val summary = "Prove a linear logic formula"
                  structure Prover = Constr.Provers.Linear)

   val summary = "Intuitionistic linear logic"

   fun readme () = $summary

   val cmd = C.group
                { readme = readme
                , summary = summary
                , subs =
                  [ ("show", Show.cmd)
                  , ("prove", Prove.cmd)
                  ] }
end

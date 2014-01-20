
structure CmdFol :> Cmd = struct
   open PP.Ops

   structure CmdShow =
      CmdShowFn (val summary = "Show a fol formula"
                 structure Frontend = Fol.Provers.Partial.Frontend)

   structure CCmd =
      CmdProveFn (val summary = "Prove a formula using the cascading prover"
                  val mode = NONE
                  structure Prover = Fol.Provers.Cascade)

   structure PCmd =
      CmdProveFn (val summary = "Prove a formula using the partial-application prover"
                  val mode = NONE
                  structure Prover = Fol.Provers.Partial)

   val summary = "Intuitionistic propositional logic"

   fun readme () = $summary

   val cmd = Command.group
                { readme = readme
                , summary = summary
                , subs =
                  [ ("show", CmdShow.cmd)
                  , ("prove", PCmd.cmd)
                  , ("cascade", CCmd.cmd)
                  ] }
end

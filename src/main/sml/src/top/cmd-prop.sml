
structure CmdProp :> Cmd = struct
   open PP.Ops

   structure CmdShow =
      CmdShowFn (val summary = "Show a propositional formula"
                 structure Frontend = Prop.Provers.Partial.Frontend)

   structure CCmd =
      CmdProveFn (val summary = "Prove a formula using the cascading prover"
                  val mode = NONE
                  structure Prover = Prop.Provers.Cascade)

   structure PCmd =
      CmdProveFn (val summary = "Prove a formula using the partial-application prover"
                  val mode = NONE
                  structure Prover = Prop.Provers.Partial)


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

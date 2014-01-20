
structure CmdOrdered :> Cmd = struct
   structure C = Command

   open PP.Ops

   structure Show =
      CmdShowFn (val summary = "Show a ordered logic formula"
                 structure Frontend = Constr.Provers.Ordered.Frontend)

   structure Prove =
      CmdProveFn (val summary = "Prove a ordered logic formula"
                  val mode = NONE
                  structure Prover = Constr.Provers.Ordered)

   val summary = "Intuitionistic ordered logic"

   fun readme () = $summary

   val cmd = C.group
                { readme = readme
                , summary = summary
                , subs =
                  [ ("show", Show.cmd)
                  , ("prove", Prove.cmd)
                  ] }
end

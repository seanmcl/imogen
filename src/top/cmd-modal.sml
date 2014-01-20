
structure CmdModal :> Cmd = struct
   structure C = Command
   structure D = Parse.Meta.Mode
   open PP.Ops

   structure M = Constr.Provers.Modal

   structure CmdShowS =
      CmdShowFn (val summary = "Show a formula with the Simpson translaton"
                 structure Frontend = M.K.Frontend)

   structure CmdShowD =
      CmdShowFn (val summary = "Show a formula with the Davies translation"
                 structure Frontend = M.PD.Frontend)

   structure K =
      CmdProveFn (val summary = "Prove a formula in K"
                  val mode = SOME D.K
                  structure Prover = M.K)

   structure T =
      CmdProveFn (val summary = "Prove a formula in T"
                  val mode = SOME D.T
                  structure Prover = M.T)

   structure B =
      CmdProveFn (val summary = "Prove a formula in B"
                  val mode = SOME D.B
                  structure Prover = M.B)

   structure K4 =
      CmdProveFn (val summary = "Prove a formula in K4"
                  val mode = SOME D.K4
                  structure Prover = M.K4)

   structure S4 =
      CmdProveFn (val summary = "Prove a formula in S4"
                  val mode = SOME D.S4
                  structure Prover = M.S4)

   structure S5 =
      CmdProveFn (val summary = "Prove a formula in S5"
                  val mode = SOME D.S5
                  structure Prover = M.S5)

   structure PD =
      CmdProveFn (val summary = "Prove a formula in Pfenning-Davies logic"
                  val mode = SOME D.P
                  structure Prover = M.PD)

   val summary = "Intuitionistic modal logic"

   fun readme () = $summary

   val cmd = C.group
                { readme = readme
                , summary = summary
                , subs =
                  [ ("simpson", CmdShowS.cmd)
                  , ("davies", CmdShowD.cmd)
                  , ("k", K.cmd)
                  , ("t", T.cmd)
                  , ("b", B.cmd)
                  , ("k4", K4.cmd)
                  , ("s4", S4.cmd)
                  , ("s5", S5.cmd)
                  , ("pd", PD.cmd)
                  ] }

end

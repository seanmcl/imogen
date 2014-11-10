
structure Bug = struct
   structure H = Heuristics
   structure F = PFormula
   structure P = Provers.Modal.K
   structure P = Provers.Modal.S5
   structure P = Provers.Modal.K4
   structure P = Provers.Modal.S4
   structure P = Provers.Modal.K
   structure P = Provers.Modal.S4
   structure P = Provers.Modal.PD
   open General
   open PP.Ops

   val bugs = "forall X. box (p(X)) => box (forall X. p(X))"
   val bugs = "box (forall X. p(X)) => forall X. box (p(X))"
   val bugs = "P => box P"
   val bugs = "(dia p => false) => (box (p => false))"
   val bugs = "(dia p => box q) => (box (p => q))"
   val bugs = "box (box q) => q"
   val bugs = "P"
   val bugr = true
   val verbose = true
   val heuristic = Heuristics.minimal

   val g : unit -> unit = fn () =>
      let
         (* val _ = State.reset () *)
         val _ = Log.setLevel Log.Critical
         val _ = Log.setLevel Log.Debug
         val _ = Log.setLevel Log.Nothing
         val _ = Log.setLevel Log.Trace
         (* val _ = PP.ppl (Signat.pp ()) *)
         val _ = Log.trace (fn () => $"Parsing as modal formula")
         val f = Parse.imogen.Formula.ofString bugs
         val _ = Log.trace (fn () => $"Translating to modal pformula")
         val f = P.Frontend.parse f
         val _ = Log.trace (fn () => &[$"imogen.Formula:", %[\\, P.Frontend.pp f]])
         val _ = Log.trace (fn () => $"Translating to pformula")
         val f = P.Frontend.pformula f
         val _ = Log.trace (fn () => &[$"PFormula:", %[\\, F.neg F.pp f]])
         val f = F.neg F.separate f
         val _ = Log.trace (fn () => $"Parsing complete.")

         val _ = Parameters.Prover.statusInterval := 1
         val _ = Parameters.Proof.reconstruct := true
         val inp = P.Input.T
                      { formula = f
                      , heuristics = heuristic }
         val res = P.prove inp
         open P.Output
      in
         case res of
            Success (SOME (nd, f), _) =>
            if bugr then
               let in
                  printl "Success, proof found"
                ; printl "Checking..."
                ; ND.check { eq = Term.eq, ctx = [], term = nd, form = f}
                ; PP.ppl (ND.pp nd)
               end
            else printl "Expected saturation, found proof"
          | Success (NONE, _) =>
            if bugr then
               printl "Success, no proof requested"
            else printl "Expected proof, found saturated"
          | Saturated _ =>
            if bugr then printl "Expected proof, found saturation"
            else printl "Correct saturation"
          | TimeOut => printl "Timeout"
      end

end

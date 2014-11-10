
structure Bug = struct
   structure H = Heuristics
   structure F = PFormula
   structure P = Provers.Linear
   open General
   open PP.Ops

   val bugs = "(p -o q) -o (p -o q)"
   val bugs = "(top -o q) -o (top -o q)"
   val bugs = "(p -o q) -o (p -o q)"
   val bugs = "p => p => p"
   val blocks =
      " (forall X Y. free(X) * on(X, Y) * empty -o free(Y) * holds(X)) => \
      \ (forall X Y. holds(X) * free(Y) -o empty * free(X) * on(X,Y)) => \
      \ (forall X. free(X) * on(X,tab) * empty -o holds(X)) => \
      \ (forall X. holds(X) -o empty * on(X,tab) * free(X)) => "

   val bugs = blocks ^ "free(b1) * on(b1, b2) * on(b2, tab) * empty -o on(b1, tab) * on(b2,tab) * top"

   (* val bugs = "free(c) => on(b2,tab) * top" *)

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
         val _ = Log.trace (fn () => $"Parsing as linear formula")
         val f = Parse.imogen.Formula.ofString bugs
         val _ = PP.ppl (Parse.imogen.Formula.pp f)
         val _ = Log.trace (fn () => $"Translating to linear pformula")
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
                ; ND.check { eq = CUnif.eq, ctx = [], term = nd, form = f}
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

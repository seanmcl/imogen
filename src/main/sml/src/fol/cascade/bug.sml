
structure Bug = struct
   structure H = Heuristics
   structure P = Prover
   open General
   open PP.Ops

   val parse = Parse.imogen.Formula.ofString
   val bugs = "(exists Y. forall X. p(X, Y)) -> (forall X. exists Y. p(X, Y))"
   val bugs = "(exists X. p(X)) => (exists X. p(X))"
   val bugs = "(forall X Y. ( p(X) => p(Y) )) | false"
   val bugs = "(forall X. exists Y. p(X, Y)) -> (exists Y. forall X. p(X, Y))"
   val bugs = "  ( forall X. ( a(X) | b(X) )) => \
              \ ( exists Y. a(Y) => exists Z. ~ a(Z) ) => \
              \ ( forall X. ~ b(X) ) => dummy"
   val bugs = "exists X. forall Y. exists Z. f(X,Y) => f(Y,X)"
   val bugs = "q(a1, a2, a3, a1, a2, a3) => \
              \ exists X1 X2 X3 Y1 Y2 Y3. \
              \ ( q(X1, X2, X3, Y1, Y2, Y3) & ( ( P(X1) & P(X2) & P(X3)  ) => \
              \ ( P(Y1) & P(Y2) & P(Y3) ) )   )"
   val bugs = "q(a1, a2, a1, a2) => \
              \ exists X1 X2  Y1 Y2. \
              \ ( q(X1, X2, Y1, Y2) & ( ( P(X1) & P(X2)  ) <=> \
              \ ( P(Y1) & P(Y2) ) )   )"

   val bugs = "( forall U V W. p(U,V) | p(V,W)  ) => ( exists X. forall Y. p(X,Y) )"
   val bugs = "( ( exists X. P(X) ) <=> ( forall Y. P(Y) ) ) => ( exists X. forall Y. ( P(X) <=> P(Y) ) )"
   val bugs = "\
              \ ( ( exists X. forall Y. big_p(X) <=> big_p(Y) ) \
              \      <=> ( (exists U. big_q(U))  <=> (forall W. big_q(W) ) )) \
              \  => ( exists X1.  forall Y1. big_q(X1) <=> big_q(Y1) ) \
              \  => ( (exists U1. big_p(U1)) <=> (forall W1. big_p(W1) ))"
   val bugs = "\
              \ q(a1, a2, a1, a2) => \
              \         exists X1 X2 Y1 Y2. \
              \            ( q(X1, X2, Y1, Y2) & ( ( p(X1) & p(X2) ) => \
              \                ( p(Y1) & p(Y2)  ) )   )"
   val bugs = "((forall X1. (exists X2. (p1(X1) & p2(X2)))) => (exists X2. (forall X1. (p1(X1) & p2(X2)))))"
   val bugs = "((forall X1. (exists X2. (p1(X1) & p2(X2)))) => (exists X2. (forall X1. (p1(X1) & p2(X2)))))"
   val bugs = "(forall X Y. ( p(X) => p(Y) )) | false"
   val bugs ="( forall X Y. ( X = Y => Y = X ) | true )"
   val bugs =
      "( exists X1. forall X2. exists X3. forall X4. p1(X1,X2) & p2(X3,X4) ) => \
      \( forall X4. exists X3. forall X2. exists X1. p1(X1,X2) & p2(X3,X4) )"

   val bugs = " (forall X. p(X)) => \
              \ ( p(c) => exists Z. ~ p(Z) ) => \
              \ false"

   val bugs = " forall X. \
              \ exists Y. \
              \ forall Z. \
              \ f(Y,X) => \
              \ f(X,Y) => \
              \ ~ f(X,Z) => \
              \ f(Y,X) & f(Z,Y)"
   val bugs = "\
              \ q(a1, a2, a3, a1, a2, a3) => \
              \         exists X1 X2 X3 Y1 Y2 Y3. \
              \            ( q(X1, X2, X3, Y1, Y2, Y3) & ( ( p(X1) & p(X2) & p(X3)  ) => \
              \                ( p(Y1) & p(Y2) & p(Y3) ) )   )"

   val bugs = " ( (forall X. p) -> p)"
   val bugs = "forall X. (p(X) => p (X))"
   val bugs = "((forall Y. p(f2) <=> p(Y))) => p(f2) => p(f1)"

   val bugr = true
   val verbose = true
   val heuristic = Heuristics.optimize
   val heuristic = Heuristics.singleStep
   val heuristic = Heuristics.nothing

   val g : unit -> unit = fn () =>
      let
         val _ = State.reset ()
         val _ = Log.setLevel Log.Critical
         val _ = Log.setLevel Log.Error
         val _ = Log.setLevel Log.Warning
         val _ = Log.setLevel Log.Info
         val _ = Log.setLevel Log.Debug
         val _ = Log.setLevel Log.Trace
         val _ = Log.setLevel Log.Error
         val _ = Log.setLevel Log.Info
         val _ = Log.setLevel Log.Debug
         val _ = Log.setLevel Log.Trace

         val f = Parse.imogen.Formula.ofString bugs
         val f = PFormula.parse f
         val f' = LFormula.make f
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

(* val f : unit -> unit = fn () => *)
(*    let *)
(*       val _ = State.reset () *)
(*       (\* val _ = Log.setLevel Log.Trace *\) *)
(*       (\* val _ = Log.setLevel Log.Debug *\) *)
(*       val _ = Log.setLevel Log.Info *)
(*       (\* val _ = PP.ppl (Parse.imogen.Formula.pp f) *\) *)
(*       fun doit s = *)
(*          let *)
(*             val f = Parse.imogen.Formula.ofString s *)
(*             val f = PFormula.parse f *)
(*             val ps = H.apply heuristic (f, {maxSecs = 1000}) *)
(*          in *)
(*             PP.ppl (&[\, \, H.Problem.pp (List.last ps), \]) *)
(*          end *)
(*    (\* val _ = PP.ppl (PFormula.pp f) *\) *)
(*    (\* val f' = LFormula.label f *\) *)
(*    (\* val _ = PP.ppl (LFormula.ppLabels f') *\) *)
(*    (\* val subs = Sublabels.make f' *\) *)
(*    (\* val _ = PP.ppl (Sublabels.pp subs) *\) *)
(*    (\* val cs = LFormula.Conflicts.make (subs, f') *\) *)
(*    (\* val _ = PP.ppl (Conflicts.pp (Conflicts.make (subs, f'))) *\) *)
(*    in *)
(*       doit bugs *)
(*    end *)

end

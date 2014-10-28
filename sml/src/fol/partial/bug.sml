
structure Bug = struct
   structure H = Heuristics
   structure P = Prover
   structure F = PFormula
   open General
   open PP.Ops

   val parse = Parse.Formula.ofString
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
   val bugs = " ( (forall X. p) -> p)"
   val bugs = "forall X. (p(X) => p (X))"
   val bugs = "\
              \ q(a1, a2, a3, a1, a2, a3) => \
              \         exists X1 X2 X3 Y1 Y2 Y3. \
              \            ( q(X1, X2, X3, Y1, Y2, Y3) & ( ( p(X1) & p(X2) & p(X3)  ) => \
              \                ( p(Y1) & p(Y2) & p(Y3) ) )   )"
   val bugs =
      "( exists X1. forall X2. exists X3. forall X4. p1(X1,X2) & p2(X3,X4) ) => \
      \( forall X4. exists X3. forall X2. exists X1. p1(X1,X2) & p2(X3,X4) )"

   val bugs =
      "forall X. exists Y. forall Z. f(Y,X) => ( f(X,Z) => f(X,Y) ) & ( f(X,Y) => ~ f(X,Z) => f(Y,X) & f(Z,Y) )"
   val bugs = "( forall Y. p(a) <=> p(Y) ) => p(a) => p(d)"
   val bugs = "p(X)"
   val bugs = "(forall Y. p(f3) <=> p(Y)) => p(f2) => p(f1)"
   val bugs = "forall X. X = X"
   val bugs = "(( forall X. ( r(X) => ( ~ ( exists X. p(X) ) ) ) ))"
   val bugs = "( forall X. exists Y. q(X) & s(Y) ) => ( exists Y. forall X. q(X) & s(Y))"

   val bugs = "(forall X. a(X) | b(X)) => ( a(c) => ~ a(c) ) => ( ~ b(c) ) => false"

   val bugs = "( forall X. ( ( exists Y. p(X, Y) ) => ( forall Z. p(Z, Z) ) ) ) & \
              \ ( forall U. ( exists V. ( p(U, V) | ( m(U) & q(f(U, V)) ) ) ) ) & \
              \ ( forall W. ( q(W) => ( ~ m(g(W)) ) ) ) => \
              \ ( forall U. ( exists V. ( p(g(U), V) & p(U, U) ) ) )"

   val bugs = "f = f"


   val bugr = true
   val verbose = true
   val heuristic = Heuristics.optimize
   val heuristic = Heuristics.singleStep
   val heuristic = Heuristics.nothing
   val heuristic = Heuristics.minimal

   val g : unit -> unit = fn () =>
      let
         val _ = State.reset ()
         val _ = Log.setLevel Log.Critical
         val _ = Log.setLevel Log.Debug
         val _ = Log.setLevel Log.Trace

         val f = Parse.Formula.ofString bugs
         val f = F.parse f
         val f = F.neg F.separate f
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
                ; ND.check {eq = Term.eq, ctx = [], term = nd, form = f}
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
(*       (\* val _ = PP.ppl (Parse.Formula.pp f) *\) *)
(*       fun doit s = *)
(*          let *)
(*             val f = Parse.Formula.ofString s *)
(*             val f = F.parse f *)
(*             val ps = H.apply heuristic (f, {maxSecs = 1000}) *)
(*          in *)
(*             PP.ppl (&[\, \, H.Problem.pp (List.last ps), \]) *)
(*          end *)
(*    (\* val _ = PP.ppl (F.pp f) *\) *)
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

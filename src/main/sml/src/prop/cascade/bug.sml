
structure Bug = struct
   structure H = Heuristics
   structure P = ProverFn(structure Backend = Backend
                          structure Frontend = Frontend
                          val eq = Term.eq)
   open General
   open PP.Ops

   val parse = Parse.imogen.Formula.ofString
   val bugs = "( true | true ) => p4 => p4"
   val bugs = "( true | true ) => p2 => p2"
   val bugs = " ( p1 | p2 ) \
              \ =>  ( ~ p1 | p2 ) \
              \ => ( p1 | ~ p2 )  \
              \ => ~ ( ~ p1 | ~ p2 )"
   val bugs = "1 => 1"
   val bugs = "1 => p2 => p2"
   val bugs = "p & q => p"
   val bugs = "(p => q) => (p => q)"
   val bugs = "p => ~ ~p"
   val bugs = "~ ~ (p | ~p | ~p)"
   val bugs = "true"
   val bugs = "(P => P => Q) => (P => Q)"
   val bugs = "(A => B => C) => A & B => C"
   val bugs = "P <=> P %.\n\n abc"
   val bugs = "P <=> P"
   val bugs = "((p0 => p7 <=> p1 & ~(p5 & (~p0 => ~~~p4)))) | a <=> a <=> a <=> a <=> a => ~~~p <=> a <=> a => ~~~p <=> a <=> a => ~~~p <=> a <=> a => ~~~p"
   val bugs = "((p0 => p7 <=> p1 & ~(p5 & (~p0 => ~~~p4)) | (((p4 <=> p7) => ~p2 <=> (p3 => ~p2) & ~p3) | p7)) & ((p2 => p1 <=> p7 => ~p8 <=> ~~p8 => p2) => ~p7) ) | (p1 | ((p1 | ~p4 => false) => (p1 => ~~p8)) <=> ~p6 <=> ~p2 | ((p7 & ~~p6 => false) | ((p3 => ~p6) & (~p6 <=> p4) => false) => false))"
   val bugs = "  O11 ∧ O21  ∧   O11 ∧ O31  ∧   O11 ∧ O41  ∧   O11 ∧ O51  ∧   O11 ∧ O61  ∧   O11 ∧ O71  ∧   O11 ∧ O81  ∧   O11 ∧ O91  ∧   O11 ∧ O101  ∧   O11 ∧ O111  ∧   O11 ∧ O121  ∧   O11 ∧ O131  ∧   O11 ∧ O141  ∧   O11 ∧ O151  ∧   O11 ∧ O161  ∧   O11 ∧ O171  ∧   O11 ∧ O181  ∧   O11 ∧ O191  ∧   O11 ∧ O201  ∧   O11 ∧ O211  ∧   O21 ∧ O31  ∧   O21 ∧ O41  ∧   O21 ∧ O51                         "
   val bugs = "p"
   val bugs = "p & q"
   val bugs = "(A => B => C) => A & B => C"
   val bugs = "(~ ~ (p | ~p) => p | ~p) => (p | ~p)"
   val bugs = "(a1 o-o a2) o-o (a2 o-o a1)"
   val bugs = "p & q => p"
   val bugs = "A | false <=> A"

   val p201 = "( (p1 <=> p2) => p1 & p2 & p3 ) => ( (p2 <=> p3) => p1 & p2 & p3 ) => ( (p3 <=> p1) => p1 & p2 & p3 ) => ( p1 & p2 & p3 )"
   val p202 = "(( o11 ∨ o12 )) ⊃ (( o21 ∨ o22 )) ⊃ (( o31 ∨ o32 )) ⊃ (( ( o11 ∧ o21 ) ∨ ( ( o11 ∧ o31 ) ∨ ( ( o21 ∧ o31 ) ∨ ( ( o12 ∧ o22 ) ∨ ( ( o12 ∧ o32 ) ∨ ( o22 ∧ o32 ) ) ) ) ) ))"
   val p202 = "(( o11 ∨ ( o12 ∨ ( o13 ∨ o14 ) ) )) ⊃ (( o21 ∨ ( o22 ∨ ( o23 ∨ o24 ) ) )) ⊃ (( o31 ∨ ( o32 ∨ ( o33 ∨ o34 ) ) )) ⊃ (( o41 ∨ ( o42 ∨ ( o43 ∨ o44 ) ) )) ⊃ (( o51 ∨ ( o52 ∨ ( o53 ∨ o54 ) ) )) ⊃ (( ( o11 ∧ o21 ) ∨ ( ( o11 ∧ o31 ) ∨ ( ( o11 ∧ o41 ) ∨ ( ( o11 ∧ o51 ) ∨ ( ( o21 ∧ o31 ) ∨ ( ( o21 ∧ o41 ) ∨ ( ( o21 ∧ o51 ) ∨ ( ( o31 ∧ o41 ) ∨ ( ( o31 ∧ o51 ) ∨ ( ( o41 ∧ o51 ) ∨ ( ( o12 ∧ o22 ) ∨ ( ( o12 ∧ o32 ) ∨ ( ( o12 ∧ o42 ) ∨ ( ( o12 ∧ o52 ) ∨ ( ( o22 ∧ o32 ) ∨ ( ( o22 ∧ o42 ) ∨ ( ( o22 ∧ o52 ) ∨ ( ( o32 ∧ o42 ) ∨ ( ( o32 ∧ o52 ) ∨ ( ( o42 ∧ o52 ) ∨ ( ( o13 ∧ o23 ) ∨ ( ( o13 ∧ o33 ) ∨ ( ( o13 ∧ o43 ) ∨ ( ( o13 ∧ o53 ) ∨ ( ( o23 ∧ o33 ) ∨ ( ( o23 ∧ o43 ) ∨ ( ( o23 ∧ o53 ) ∨ ( ( o33 ∧ o43 ) ∨ ( ( o33 ∧ o53 ) ∨ ( ( o43 ∧ o53 ) ∨ ( ( o14 ∧ o24 ) ∨ ( ( o14 ∧ o34 ) ∨ ( ( o14 ∧ o44 ) ∨ ( ( o14 ∧ o54 ) ∨ ( ( o24 ∧ o34 ) ∨ ( ( o24 ∧ o44 ) ∨ ( ( o24 ∧ o54 ) ∨ ( ( o34 ∧ o44 ) ∨ ( ( o34 ∧ o54 ) ∨ ( o44 ∧ o54 ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ))"
   val p206 = "(( ( ( a1 ⇔ a2 ) ⇔ a3 ) ⇔ ( a3 ⇔ ( a2 ⇔ a1 ) ) ))"
   val p207 = "(( ( p1 ⇔ p2 ) ⊃ ( p1 ∧ p2 ) ))  ⊃ (( ( p2 ⇔ p1 ) ⊃ ( p1 ∧ p2 ) )) ⊃ (( p0 ∨ ( ( p1 ∧ p2 ) ∨ ( ¬ p0 ) ) ))"
   val p208 = "(( o11 ∨ ( ¬ ( ¬ o12 ) ) )) ⊃ (( o21 ∨ ( ¬ ( ¬ o22 ) ) )) ⊃ (( o31 ∨ ( ¬ ( ¬ o32 ) ) )) ⊃ (( ( o11 ∧ o21 ) ∨ ( ( o11 ∧ o31 ) ∨ ( ( o21 ∧ o31 ) ∨ ( ( o12 ∧ o22 ) ∨ ( ( o12 ∧ o32 ) ∨ ( o22 ∧ o32 ) ) ) ) ) ))"
   val bugs = "( (p1 <=> p2) => p1 & p2 & p3 ) => ( (p2 <=> p3) => p1 & p2 & p3 ) => ( (p3 <=> p1) => p1 & p2 & p3 ) => ( p1 )"
   val bugs = "~ ~ p => p"
   val bugs = "false => p"
   val bugs = "(p & ~p) => p"
   val bugs = "~~(p & ~p) => p & p"
   val bugs = "~~(p & ~p) => p & p"
   val bugs = "False => p"
   val bugs = "((p => q) <=> p) => q"
   val bugs = "(((p => Q) <=> p) => Q) | p"
   val bugs = "p => q => ( p <=> q )"
   val bugs = "( (p1 <=> p2) => p1 & p2 & p3 & p4 & p5) =>( (p2 <=> p3) => p1 & p2 & p3 & p4 & p5) =>( (p3 <=> p4) => p1 & p2 & p3 & p4 & p5) =>( (p4 <=> p5) => p1 & p2 & p3 & p4 & p5) =>( (p5 <=> p1) => p1 & p2 & p3 & p4 & p5) =>p1"

   val bugr = true
   val verbose = true
   val heuristic = Heuristics.singleStep
   val heuristic = Heuristics.nothing

   val ps = [p201, p202, p206, p207, p208]

   val g : unit -> unit = fn () =>
      let
         val _ = State.reset ()
         val _ = Log.setLevel Log.Trace
         val _ = Log.setLevel Log.Info
         (* val _ = Parameters.Rules.useConflicts := false *)
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
(*       val _ = Log.setLevel Log.Trace *)
(*       val _ = Parameters.Rules.useConflicts := false *)
(*       val _ = Parameters.Rules.useExactMatches := false *)
(*       val _ = Parameters.Db.useRuleSubsumption := false *)
(*       val _ = Parameters.Rules.useDisableBranchOnExactMatch := false *)
(*       val _ = Parameters.Rules.usePruning := false *)
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
(*       app doit ps *)
(*    end *)

end


functor CmdProveFn (val summary : string
                    val mode : Parse.Meta.Mode.t option
                    structure Prover : Prover) :> Cmd = struct

   open General
   open PP.Ops

   structure C = Command
   structure F = PFormula
   structure U = CmdUtil
   structure G = U.Flags
   structure PF = Parse.imogen.Formula
   structure Meta = Parse.Meta
   structure S = Meta.Pre
   structure O = OS.Process

   structure P = Prover

   fun hasMode modes =
      let
         open Parse.Meta.Mode
         val lt = fn
            (K, K) => true
          | (K, T) => true
          | (K, B) => true
          | (K, K4) => true
          | (K, S4) => true
          | (K, S5) => true
          | (T, T) => true
          | (T, S4) => true
          | (T, S5) => true
          | (K4, K4) => true
          | (K4, S4) => true
          | (K4, S5) => true
          | (S5, S5) => true
          | (P, P) => true
          | _ => false
      in
         case mode of
            NONE => true
          | SOME m => List.exists (fn m' => lt (m', m)) modes
      end

   structure Args = struct
      val heuristics = ref (valOf (Heuristics.fromString Parameters.Heuristics.default))
      val syntax = ref U.Syntax.Basic
      val inline = ref false
   end

   fun readme () =
      &[ $summary
       , ~
       , $"Like cat, read from stdin unless a file is passed"
       , ~
       , $"Examples:"
       , %[\\, &[ $"prove foo.imo"
                , $"prove < foo.imo"
                , $"prove -i \"a => a\"" ]]]

   val flags =
      [ G.depthInterval
      , G.heuristics Args.heuristics
      , G.noGlobalization
      , G.noExact
      , G.noPruning
      , G.noDisableBranchOnExactMatch
      , G.noRuleSubsumption
      , G.reconstructProof
      , G.statusInterval
      , G.noConflicts
      , G.noBackwardSubsumption
      , G.noBiimpContinue
      , G.noForwardSubsumption
      , G.syntax Args.syntax
      , G.printLength
      , G.parseInline Args.inline
      , G.tptpOutput
      , G.depthInterval
      , G.swapInterval
      , G.exhaustRules
      , G.verbose
      , G.indexTableSize
      , G.proofTableSize
      , G.noAssertions
      , G.timeout ]

   val usageArg = "FILE"

   fun run { anons } =
      let
         val _ = State.reset ()
         val _ = Log.trace (fn () => $"Parsing.")
         val _ = Log.warning (fn () => if !Debug.assertP then $"Warning: assertions are on" else ~)
         val (meta, f) = case (anons, !Args.inline) of
            ([], _) => ([], PF.ofStdin ())
          (* (Meta.ofStdin (), PF.ofStdin ()) *)
          | ([file], false) =>
            (Meta.ofFile file, PF.ofFile file)
          | ([form], true) =>
            (Meta.ofString form, PF.ofString form)
          | _ => failwith "Too many args"
         (* val _ = Log.trace (fn () => PF.pp f) *)
         val f = P.Frontend.parse f
         val _ = Log.trace (fn () => &[$"imogen.Formula:", %[\\, P.Frontend.pp f]])
         val f = P.Frontend.pformula f
         val f = F.neg F.separate f
         val _ = Log.trace (fn () => $"Parsing complete.")
         val hasConjecture = case f of
            F.Lolli (_, F.Up F.Zero) => false
          | _ => true
         val timer = Timer.startCPUTimer ()
         val input = Prover.Input.T
                        { formula = f
                        , heuristics = !Args.heuristics }
         fun ppStats stats =
            let in
               Log.info (fn () => P.Stats.pp stats)
             ; Log.info
                  (fn () => %[$"Time: ",
                              PP.time (#usr (Timer.checkCPUTimer timer))])
            end
         val tptp = !Parameters.Prover.tptpOutput
         fun saturate stats =
            let in
               if tptp then
                  if hasConjecture
                  then printl "% SZS status CounterSatisfiable."
                  else printl "% SZS status Satisfiable."
               else
                  printl "Unprovable."
             ; ppStats stats
             ; case Meta.status meta of
                  NONE => O.success
                | SOME (s, modes) =>
                  case s of
                     S.Theorem =>
                     if hasMode modes then
                        ( printl "Error!  Failed to prove theorem.";
                         O.failure )
                     else O.success
                   | S.NonTheorem => O.success
                   | S.Open =>
                     ( printl "Warning: saturated on open problem";
                      O.success )
                   | S.Unknown =>
                     ( printl "Warning: saturated on unknown problem";
                      O.success )
            end
         fun succeed (pt, stats) =
            let in
               if tptp then
                  if hasConjecture
                  then printl "% SZS status Theorem."
                  else printl "% SZS status Unsatisfiable."
               else printl "Provable."
             ; case pt of
                  NONE => ()
                | SOME (tm, _) =>
                  let
                     val doc = ND.pp tm
                  in
                     if tptp then printl "% SZS output start proof." else ()
                   ; PP.ppl (&[~, doc, ~])
                   ; if tptp then printl "% SZS output end proof." else ()
                  end
             ; ppStats stats
             ; case Meta.status meta of
                  NONE => O.success
                | SOME (s, modes) =>
                  case s of
                     S.Theorem =>
                     if hasMode modes then O.success
                     else ( printl "Error!  Proved non-theorem.";
                           O.failure )
                   | S.NonTheorem =>
                     ( printl "Error!  Proved non-theorem.";
                      O.failure )
                   | S.Open =>
                     ( printl "Warning: proved open problem";
                      O.success )
                   | S.Unknown =>
                     ( printl "Warning: proved unknown problem";
                      O.success )
            end
         fun timeout () =
            let in
               if tptp
               then printl "% SZS status Timeout."
               else printl "Timeout."
             ; O.failure
            end
         open P.Output
      in
         case P.prove input of
            Saturated stats => saturate stats
          | Success (pt, stats) => succeed (pt, stats)
          | TimeOut => timeout ()
      end

   val cmd = C.create
                { readme = readme
                , summary = summary
                , usageArg = usageArg
                , flags = flags
                , run = run }

end

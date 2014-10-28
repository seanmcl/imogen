
structure CmdUtil :> CmdUtil = struct

   structure F = Command.Flag
   structure P = Parameters
   open General
   open PP.Ops

   structure Syntax = struct
      datatype t =
         Basic
       | Polar
       | Prolog
      val ofString = fn
         "basic" => SOME Basic
       | "polar" => SOME Polar
       | "prolog" => SOME Prolog
       | _ => NONE
   end

   structure Flags = struct
      type 'a t = 'a ref -> Command.Flag.t

      fun syntax c =
         let
            fun process s =
               case Syntax.ofString s of
                  NONE => failwith "Parse"
                | SOME s => c := s
         in
            F.string
               { name = "syntax"
               , doc = "ARG (basic|polar|prolog)"
               , process = process }
         end

      fun classical c =
         F.set
            { name = "classical"
            , doc = " Prove the formula classically via double-negation"
            , process = c }

      fun heuristics c =
         let
            fun process s =
               case Heuristics.fromString s of
                  NONE => failwith "Heuristics"
                | SOME s => c := s
         in
            F.string
               { name = "heuristics"
               , doc = "ARG (optimize|minimal|negAtoms|posAtoms)"
               , process = process }
         end

      val parseInline : bool t = fn c =>
         F.set
            { name = "inline"
            , doc = " specify the input formula on the command line"
            , process = c }

      val indexTableSize =
         F.setInt
            { name = "table-size-index" (* Don't clash with -inline *)
            , doc = "N hashtable size for the indexes"
            , process = P.Index.table }

      val proofTableSize =
         F.setInt
            { name = "table-size-proof" (* Don't clash with -inline *)
            , doc = "N hashtable size for the proofterm databases"
            , process = P.Proof.table }

      val depthInterval =
         F.setInt
            { name = "depth-interval"
            , doc = "N Interval between selections off time queue"
            , process = P.Db.depthInterval }

      val swapInterval =
         F.setInt
            { name = "swap-interval"
            , doc = "N Interval between swapping between rules and sequents"
            , process = P.Partial.swapInterval }

      val exhaustRules =
         F.unset
            { name = "exhaust-rules"
            , doc = "N exhaust rules before sequents"
            , process = P.Partial.exhaustSeqs }

      val noConflicts =
         F.unset
            { name = "no-conflicts"
            , doc = " turn off conflicts"
            , process = P.Db.useConflicts }

      val noDisableBranchOnExactMatch =
         F.unset
            { name = "no-disable-branch-on-exact-match"
            , doc = " turn off branch disabling"
            , process = P.Cascade.useDisableBranchOnExactMatch }

      val noPruning =
         F.unset
            { name = "no-pruning"
            , doc = " turn off pruning optimization (cascade)"
            , process = P.Cascade.usePruning }

      val noBackwardSubsumption =
         F.unset
            { name = "no-backward-subsumption"
            , doc = " turn off backward subsumption"
            , process = P.Db.useBackwardSubsumption }

      val noBiimpContinue =
         F.unset
            { name = "no-biimp-continue"
            , doc = " Turn off biimplication continuation"
            , process = P.Focus.useBiimpContinue }

      val noExact =
         F.unset
            { name = "no-exact"
            , doc = " Turn off the exact match optimization"
            , process = P.Cascade.useExactMatches }

      val noForwardSubsumption =
         F.unset
            { name = "no-forward-subsumption"
            , doc = " turn off forward subsumption"
            , process = P.Db.useForwardSubsumption }

      val noGlobalization =
         F.unset
            { name = "no-globalization"
            , doc = " Turn off globalization"
            , process = P.Focus.useGlobalization }

      val noRuleSubsumption =
         F.unset
            { name = "no-rule-subsumption"
            , doc = " Turn off rule-subsumption"
            , process = P.Db.useRuleSubsumption }

      val statusInterval =
         F.setInt
            { name = "status-interval"
            , doc = "N Interval between status messages"
            , process = P.Prover.statusInterval }

      val printLength =
         F.setInt
            { name = "print-length"
            , doc = "N Number of database lines to show"
            , process = P.Db.printLength }

      val reconstructProof =
         F.set
            { name = "proof"
            , doc = " Show the proof term"
            , process = P.Proof.reconstruct }

      val timeout =
         F.setInt
            { name = "timeout"
            , doc = "N seconds before timeout"
            , process = P.Prover.maxSeconds }

      val tptpOutput =
         F.set
            { name = "tptp"
            , doc = " TPTP output"
            , process = P.Prover.tptpOutput }

      val verbose =
         F.int
            { name = "verbose"
            , doc = "ARG (0-5)"
            , process = Log.setLevel' }

      val noAssertions =
         F.unset
            { name = "no-assertions"
            , doc = " Turn assertions off"
            , process = Debug.assertP }
   end

end

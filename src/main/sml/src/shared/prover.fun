
functor ProverFn (structure Frontend : Frontend
                  structure Backend : Backend
                  val eq : Term.t * Term.t -> bool
                  val norm : Rel.t -> Rel.t) :> Prover = struct
   structure F = PFormula
   structure H = Heuristics
   structure Frontend = Frontend

   open General
   open PP.Ops

   structure Stats : sig
      type t
      include Printable where type printable = t
      val create : {mainTimer : Timer.cpu_timer, stable : int} -> t
      val add : t * Backend.Stats.one -> t
      val solved : t -> int
   end = struct
      datatype t = T of
         { mainTimer: Timer.cpu_timer
         , localTimer: Timer.cpu_timer
         , stable: int
         , solved: int
         , stats : Backend.Stats.t }
      type printable = t

      fun solved (T {solved, ...}) = solved

      fun create {mainTimer, stable} =
         T { mainTimer = mainTimer
           , localTimer = Timer.startCPUTimer ()
           , stable = stable
           , solved = 0
           , stats = Backend.Stats.empty }

      fun pp (T {mainTimer, localTimer, solved, stable, stats}) =
         &[ %[$"time (total)   : ", PP.time (Timer.checkCPUTimer' mainTimer)]
          , %[$"time (this)    : ", PP.time (Timer.checkCPUTimer' localTimer)]
          , %[$"solved         : ", PP.int solved, $" / ", PP.int stable]
          , %[$"database stats : ", Backend.Stats.pp stats]
          ]

      fun add (T { mainTimer, localTimer, stable, solved, stats }, s) =
         T { mainTimer = mainTimer, localTimer = localTimer, stable = stable
           , solved = solved+1, stats = Backend.Stats.add (stats, s) }
   end

   structure Input = struct
      datatype t = T of
         { formula: PFormula.neg
         , heuristics: Heuristics.t }
   end

   (* The result for a single stable sequent. *)
   structure Output1 = struct
      datatype t =
         Success of SC.t option * Backend.Stats.one
       | Saturated of Backend.Stats.one
   end

   (* Result for the entire process. *)
   structure Output = struct
      datatype t =
         Success of (ND.t * imogen.Formula.t) option * Stats.t
       | Saturated of Stats.t
       | TimeOut
   end

   fun prove1 return stable (cseq as Focus.CSeq.T { frozen, ... }, (stats, proofs)) =
      let
         val solved = Stats.solved stats
         val n = List.length (Focus.Stable.seqs stable)
         val _ = Log.warning
                    (fn () =>
                       if solved > 0 andalso solved mod (!Parameters.Prover.statusInterval) = 0 then
                          %[$"Solved: ", PP.int solved,$"/", PP.int n]
                       else ~)
         val _ = Log.info
                    (fn () =>
                       (&[ ~, Util.line, ~, $"Focusing on:", ~, %[\\, Focus.CSeq.pp cseq], ~ ]))
         val foci = Focus.initial
                       { seq = cseq
                       , include_inconsistent_seq = Frontend.usesConstraints }
         val _ = Log.debug (fn () => $"Focusing complete.")
         val _ = Log.trace (fn () => &[$"Foci", %[\\, Focus.Foci.pp foci]])
         val db = Backend.create { stable = stable, foci = foci }
         fun printBackend () = Log.msg (fn
            Log.Trace => SOME (&[Backend.pp db])
          | Log.Debug => SOME (&[Backend.pp db])
          | Log.Info => SOME (&[Backend.ppShort db])
          | Log.Warning => SOME (&[Backend.ppShort db])
          | _ => NONE)
         val ival = !Parameters.Prover.statusInterval
         val iters = ref 0
         fun step () =
            let open BackendUtil.Status in
               Ref.incr iters
             ; if !iters mod ival = 0
               then printBackend ()
               else ()
             ; case Backend.step db of
                  Inconclusive => step ()
                | Saturated =>
                  let in
                     Log.info (fn () => $"Final database:")
                   ; printBackend ()
                   ; Output1.Saturated (Backend.stats db)
                  end
                | Proved pf =>
                  let
                     val _ = Log.info (fn () => $"Final database:")
                     val _ = printBackend ()
                  in
                     Output1.Success (pf, Backend.stats db)
                  end
            end
      in
         case step () of
            Output1.Success (pf, dbStats) =>
            let
               val pf = Option.map (SC.thaw frozen) pf
            in
               (Stats.add (stats, dbStats), pf :: proofs)
            end
          | Output1.Saturated dbStats =>
            return (Output.Saturated (Stats.add (stats, dbStats)))
      end

   (* Generate all stable sequents and prove them. *)
   fun proveStable (f, mainTimer) =
      let
         (* Generate initials stable sequents. *)
         val stable as Focus.Stable.T {seqs, proof, ...} =
            Focus.stabilize f
         (* val _ = Log.debug (fn () => %[$"Stable seqs: ", PP.int (length seqs)]) *)
         (* val _ = Log.debug (fn () => &[%[$"Stable seqs: ", PP.int (length seqs)], *)
         (*                               %[\\, &(map Focus.Seq.pp seqs)]]) *)
         val _ = Log.debug (fn () => &[ $"Stable: "
                                      , %[\\, Focus.Stable.pp stable]])
         (* Don't print the stable sequents.  They are huge in general. *)
         val stats = Stats.create { mainTimer = mainTimer, stable = length seqs }
      in
         WithReturn.f (fn return =>
            let
               val (stats, pterms) =
                  foldl (prove1 return stable) (stats, []) seqs
               (* Reconstruct the proof term *)
               val proof =
                  if not (!Parameters.Proof.reconstruct) then NONE else
                  let
                     val _ = Log.debug (fn () => $"Reconstructing proofterm...")
                     val pf = Fragment.sc (proof, rev (map valOf pterms))
                     val _ = Log.debug (fn () =>
                        &[ $"Sequent proof:"
                         , %[\\, SC.pp pf]
                         , $"Translating to natural deduction."])
                     val (pf, _) = SC.nd {norm = norm} pf
                     val _ = Log.trace (fn () =>
                        &[ $"Natural deduction proof:"
                         , %[\\, ND.pp pf]])
                     val n = ND.size pf
                     val pf =
                        if n > Parameters.Proof.maxSize then
                           let in
                              printl "The un-normalized proof is large. Not normalizing."
                            ; pf
                           end
                        else
                           let
                              val _ = Log.info (fn () => $"Normalizing...")
                              val pf = ND.normalize pf
                              val _ = Log.info (fn () => $"Normalization complete.")
                           in
                              pf
                           end
                     val form = F.neg F.erase f
                     val pf = ND.label(pf, form)
                     val form = imogen.Formula.unlabel form
                  in
                     Log.info (fn () => ND.pp pf)
                   ; Log.info (fn () => $"Checking proof...")
                   ; ND.check { eq = eq, ctx = [], term = pf, form = form }
                   ; Log.info (fn () =>
                      &[ $"Checked."
                       , $"Proofterm reconstruction succeeded." ])
                   ; SOME (pf, form)
                  end
            in
               Output.Success (proof, stats)
            end)
      end

   fun proveLimit (formula, timer, secs) =
      let
         val secs = Time.fromSeconds (IntInf.fromInt secs)
      in
         TimeLimit.timeLimit secs proveStable (formula, timer)
         handle TimeLimit.TimeOut => Output.TimeOut
      end

   (* Apply heuristics to a formula to yield a list of equivalent formulas
   (proof) or instances (disproof) and try to (dis-)prove them. *)
   val prove : Input.t -> Output.t = fn
      (Input.T {formula, heuristics}) =>
      let
         val mainTimer = Timer.startCPUTimer ()
         val maxSecs = !Parameters.Prover.maxSeconds
         val _ = Log.info (fn () =>
            &[ Parameters.pp ()
             , $"Input formula: "
             , %[\\, F.neg F.pp formula ]
             , %($"Trying heuristic: "
                 :: $(Heuristics.name heuristics)
                 :: [$" for ", PP.int maxSecs, $" seconds"]) ])
         val problems = Heuristics.apply heuristics (formula, {maxSecs=maxSecs})
         fun appFn return (H.Problem.T {formula, kind, maxSecs}) =
            let
               val _ = Log.info (fn () =>
                  &[ %[ $"Trying for ", PP.int maxSecs, $" seconds:"]
                   , %[\\, F.neg F.pp formula] ])
               val res = proveLimit (formula, mainTimer, maxSecs)
            in
               case res of
                  Output.Saturated _ => return res
                | Output.TimeOut => ()
                | Output.Success _ =>
                  case kind of
                     H.Instance => () (* Try the next formula. *)
                   | H.Equivalent => return res
            end
      in
         WithReturn.f (fn return =>
            let in
               List.app (appFn return) problems
             ; Output.TimeOut
            end)
      end
end

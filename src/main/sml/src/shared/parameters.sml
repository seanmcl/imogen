
structure Parameters = struct
   open PP.Ops

   structure Parse = struct
      val symbolTableSize = 100
   end

   structure Heuristics = struct
      val maxStable = 100000
      val default = "minimal"
   end

   structure Focus = struct
      val useBiimpContinue = ref true
      val useGlobalization = ref true
   end

   structure Proof = struct
      val table = ref 100
      val reconstruct = ref false
      val maxSize = 1000
   end

   structure Index = struct
      (* If this is more than 100 it starts to slow down.  *)
      val table = ref 10
   end

   structure Cascade = struct
      val usePruning = ref true
      val useExactMatches = ref true
      val useDisableBranchOnExactMatch = ref true
   end

   structure Partial = struct
      val exhaustSeqs = ref true
      val swapInterval = ref 4
   end

   structure Prover = struct
      val statusInterval = ref 10
      val maxSeconds = ref 600
      val tptpOutput = ref false
   end

   structure Modal = struct
      val logic = ref "k"
   end

   structure Db = struct
      val useConflicts = ref true
      val useForwardSubsumption = ref true
      val useBackwardSubsumption = ref true
      val useRuleSubsumption = ref true
      val printLength = ref 100
      val depthInterval = ref 10
   end

   val pp : unit -> PP.t = fn () =>
      &[ $"Parameters"
       , %[\\,
           &[ $"General"
            , %[\\,
                &[ %[$"depth interval       : ", PP.int (!Db.depthInterval)]
                 , %[$"index table size     : ", PP.int (!Index.table)]
                 , %[$"proof table size     : ", PP.int (!Proof.table)]
                 , %[$"max seconds          : ", PP.int (!Prover.maxSeconds)] ]]
            , $"Optimizations"
            , %[\\,
                &[ %[$"globalization        : ", PP.bool (!Focus.useGlobalization)]
                 , %[$"o-o continuation     : ", PP.bool (!Focus.useBiimpContinue)]
                 , %[$"forward subsumption  : ", PP.bool (!Db.useForwardSubsumption)]
                 , %[$"backward subsumption : ", PP.bool (!Db.useBackwardSubsumption)]
                 , %[$"rule subsumption     : ", PP.bool (!Db.useRuleSubsumption)]
                 , %[$"conflicts            : ", PP.bool (!Db.useConflicts)]
                 , $"Output"
                 , %[\\, &[ %[$"print length      : ", PP.int (!Db.printLength)]
                          , %[$"status interval   : ", PP.int (!Prover.statusInterval)]
                          , %[$"reconstruct proof : ", PP.bool (!Proof.reconstruct)]
                          , %[$"tptp output       : ", PP.bool (!Prover.tptpOutput)] ]]
                 ]]
            , &[ $"Partial application"
               , %[\\,
                   &[ %[$"swap interval        : ", PP.int (!Partial.swapInterval)]
                    , %[$"selection policy     : ", $(if !Partial.exhaustSeqs then "exhaust sequents" else "exhaust rules")]
                    ]]]
            , &[ $"Cascading"
               , %[\\,
                   &[ %[$"pruning              : ", PP.bool (!Cascade.usePruning)]
                    , %[$"disable exact match  : ", PP.bool (!Cascade.useDisableBranchOnExactMatch)]
                    , %[$"exact matches        : ", PP.bool (!Cascade.useExactMatches)] ]
                  ]]
            , &[ $"Modal logic"
               , %[\\,
                   &[ %[$"logic              : ", $(!Modal.logic)]
                    ]]]
            ]]]
end

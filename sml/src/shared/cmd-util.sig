
signature CmdUtil = sig

   structure Syntax : sig
      datatype t =
         Basic
       | Polar
       | Prolog
      val ofString : string -> t option
   end

   structure Flags : sig
      type 'a t = 'a ref -> Command.Flag.t

      (* Formulas that affect parsing *)
      val heuristics  : Heuristics.t t
      val parseInline : bool t
      val syntax      : Syntax.t t
      val classical   : bool t

      (* Parameters that affect performance *)
      val indexTableSize        : Command.Flag.t
      val depthInterval         : Command.Flag.t
      val noAssertions          : Command.Flag.t
      val swapInterval          : Command.Flag.t
      val exhaustRules          : Command.Flag.t

      (* Turn off optimizations *)
      val noPruning             : Command.Flag.t
      val noConflicts           : Command.Flag.t
      val noBackwardSubsumption : Command.Flag.t
      val noBiimpContinue       : Command.Flag.t
      val noExact               : Command.Flag.t
      val noForwardSubsumption  : Command.Flag.t
      val noGlobalization       : Command.Flag.t
      val noRuleSubsumption     : Command.Flag.t
      val noDisableBranchOnExactMatch : Command.Flag.t

      (* flags that modify the output but not the behavior *)
      val statusInterval        : Command.Flag.t
      val printLength           : Command.Flag.t
      val reconstructProof      : Command.Flag.t
      val proofTableSize        : Command.Flag.t
      val timeout               : Command.Flag.t
      val tptpOutput            : Command.Flag.t
      val verbose               : Command.Flag.t
   end

end

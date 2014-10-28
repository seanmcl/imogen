
structure Prover = struct
   structure L  = Logic
   structure K  = MProverFn (val logic = L.K)
   structure T  = MProverFn (val logic = L.T)
   structure B  = MProverFn (val logic = L.B)
   structure K4 = MProverFn (val logic = L.K4)
   structure S4 = MProverFn (val logic = L.S4)
   structure S5 = MProverFn (val logic = L.S5)

   structure PD = struct
      structure Entails = EntailsFn(val logic = L.S4)
      structure Backend = Constr.BackendFn (val entails = Entails.f)
      structure Frontend = Davies
      structure Prover = ProverFn (structure Frontend = Frontend
                                   structure Backend = Backend)
      open Prover
   end
end

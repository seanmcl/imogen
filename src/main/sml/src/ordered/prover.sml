
structure Prover = struct
   structure Frontend = imogen.Formula
   structure Backend = Constr.BackendFn (val entails = Entails.f)
   structure Prover = ProverFn (structure Frontend = Frontend
                                structure Backend = Backend)
   open Prover
end

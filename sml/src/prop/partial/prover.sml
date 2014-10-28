
structure Prover =
   ProverFn (structure Frontend = Frontend
             structure Backend = Backend
             val eq = Term.eq
             val norm = Fun.id)



structure Cascade = struct
   structure Prover =
      ProverFn
         (structure Backend = Backend
          structure Frontend = Frontend
          val eq = Term.eq
          val norm = Fun.id)

   structure Test = Test
end

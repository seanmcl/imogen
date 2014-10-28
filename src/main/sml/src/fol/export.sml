
structure Fol = struct
   structure Provers = struct
      structure Cascade = Cascade.Prover
      structure Partial = Partial.Prover
   end
   structure Test = Test
end

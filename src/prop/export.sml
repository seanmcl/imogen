
structure Prop = struct
   structure Provers = struct
      structure Partial = Partial.Prover
      structure Cascade = Cascade.Prover
   end
   structure Test = Test
end

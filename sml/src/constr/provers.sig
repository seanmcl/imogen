
signature Provers = sig
   structure Modal : sig
      structure K : Prover
      structure T : Prover
      structure B : Prover
      structure K4 : Prover
      structure S4 : Prover
      structure S5 : Prover
      structure PD : Prover
   end

   structure Linear : Prover
   structure Ordered : Prover
end

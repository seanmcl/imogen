
signature Prover = sig
   structure Stats : sig
      type t
      include Printable where type printable = t
   end

   structure Frontend : Frontend

   structure Input : sig
      datatype t = T of
         { formula: PFormula.neg
         , heuristics: Heuristics.t }
   end

   structure Output : sig
      datatype t =
         Success of (ND.t * Formula.t) option * Stats.t
       | Saturated of Stats.t
       | TimeOut
   end

   val prove: Input.t -> Output.t
end

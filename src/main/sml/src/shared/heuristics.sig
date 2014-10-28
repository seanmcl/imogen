
signature Heuristics = sig

   (* A proof or disproof for an Equivalent formula is a proof or disproof for
      the original.  A disproof of an Instance is a disproof for the original
      formula, but a proof says nothing about the original. *)
   datatype kind = Equivalent | Instance

   structure Problem : sig
      datatype t = T of
         { formula : PFormula.neg
         , kind : kind
         , maxSecs : int }
      val pp : t -> PP.t
   end

   (* Try a list of polarizations in order for a given length of time.
      int is number of seconds to try the heuristic. *)
   type t

   val name : t -> string
   val apply
      : t
      -> PFormula.neg * {maxSecs : int}
      -> Problem.t list

   val fromString : string -> t option

   val nothing : t
   val minimal : t
   val posAtoms : t
   val negAtoms : t
   val singleStep : t
   val optimize : t
   val random : t
end

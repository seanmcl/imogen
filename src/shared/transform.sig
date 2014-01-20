
signature Transform = sig
   type t = PFormula.neg

   val simplify : t -> t

   val negAtoms: t -> t
   val posAtoms: t -> t
   val optimize: t -> t
   val minimal: t -> t
   val singleStep : t -> t
   val specialize : t -> t list
end


(* Unpolarized formulas.  *)

signature Formula = sig
   structure Export : sig
      datatype t =
         Atom of Rel.t
       | Top
       | Bot
       | Not of t
       | And of t * t
       | Or of t * t
       | Imp of t * t
       | Iff of t * t
       | All of (Var.t * Sort.t) * t
       | Ex of (Var.t * Sort.t) * t
       | Label of string * t
   end
   datatype t = datatype Export.t

   include Printable where type printable = t
   include Eqable where type eqable = t

   val eq' : (Term.t * Term.t -> bool) -> t * t -> bool

   (* Free variables and parameters. *)
   val atoms : t -> Atoms.t

   (* Capture avoiding substitution *)
   val apply1: t * (Var.t * Term.t) -> t

   (* Remove labels. *)
   val unlabel: t -> t
end

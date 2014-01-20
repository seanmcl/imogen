
(*
 Term indexing.

 - Insertion and lookup assume the free variables and parameters
   in the term are universally quantified.  Thus, the names of variables
   and parameters are irrelevant for search.  For instance, if we insert
   f (X, X) into the database, and query for terms unifiable with f (g (X), Y)
   f (X, X) should be returned, even though unifying these terms as displayed
   is impossible.  The reason is that the X's in the two terms are different
   variables.
 *)

signature TermIndex = sig
   structure Id : Id

   type t
   include Printable where type printable = t

   val name: string

   val create: unit -> t
   val clear: t -> unit
   val size: t -> int
   val contains: t * Id.t -> bool
   val items: t -> Id.set
   val insert: t * Rel.t * Id.t -> unit

   (* delete does nothing if the term/id pair is missing. *)
   val delete: t * Rel.t * Id.t -> unit

   val variants: t * Rel.t -> Id.set
   val instances: t * Rel.t -> Id.set
   val unifiable: t * Rel.t -> Id.set
   val general: t * Rel.t -> Id.set
end

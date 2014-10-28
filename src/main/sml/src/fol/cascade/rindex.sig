
(*
 Sequent Indexing

 A seq-index is a data structure for storing sequents with associated
 ids and values.  The structure supports 3 main operations:

 subsumes (t, seq) returns true iff there is a sequent in t that
 subsumes seq.

 subsumed (t, seq) returns the ids associated with the sequents in t that
 are subsumed by seq.

 matches (t, seq) returns the ids associated with the sequents in t that
 will match seq.
*)

signature RIndex = sig
   val name: string

   (* Sequent index with stored value type 'a *)
   type 'a t
   val pp: 'a t -> PP.t
   val ppSeqs: 'a t -> PP.t

   val create: {global : Seq.Ants.t, atoms : Atoms.t, pp : 'a -> PP.t} -> 'a t
   val insert: 'a t * RSeq.t * 'a -> unit
   (* remove does nothing if the id is missing. *)
   val remove: 'a t * Id.t -> unit

   val size: 'a t -> int
   val toList: 'a t -> 'a list
   val toListi: 'a t -> (RSeq.t * 'a) list
   val listSeqs: 'a t -> RSeq.t list
   val find: 'a t * Id.t -> 'a option

   val member: 'a t * Id.t -> bool
   val isEmpty: 'a t -> bool

   (* forward subsumption *)
   val subsumes: 'a t * RSeq.t -> bool

   (* backward subsumption *)
   val subsumed: 'a t * RSeq.t -> Id.set

   val removeSubsumed : 'a t * RSeq.t -> unit
   val insertRemoveSubsumed : 'a t * RSeq.t * 'a -> unit

   (* You must not update the underlying index during the app. *)
   val app : (RSeq.t * 'a -> unit) -> 'a t -> unit
end

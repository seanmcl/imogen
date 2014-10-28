
signature PDB = sig
   structure Node : sig
      datatype t =
         Initial of Fragment.t
       | Derived of
         (RuleId.t * {renaming:Subst.t}) * (Id.t * {renaming:Subst.t}) * Subst.t
       | Contract of Id.t * Subst.t * {renaming:Subst.t}
       | ContractRule of RuleId.t * Subst.t * {renaming:Subst.t}
   end

   type t
   include Printable where type printable = t

   val create : unit -> t
   val seq : t * Id.t * Node.t -> unit
   val rule : t * RuleId.t * Node.t -> unit
   val reconstruct : t * Id.t -> SC.t
end

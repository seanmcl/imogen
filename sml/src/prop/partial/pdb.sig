
signature PDB = sig
   structure Node : sig
      datatype t =
         Initial of Fragment.t
       | Derived of RuleId.t * Id.t
   end

   type t
   include Printable where type printable = t

   val create : unit -> t
   val seq : t * Id.t * Node.t -> unit
   val rule : t * RuleId.t * Node.t -> unit
   val reconstruct : t * Id.t -> SC.t
end

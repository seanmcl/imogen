
signature Proof = sig
   structure Node : sig
      datatype t =
         Initial of SC.t
       | Derived of RuleId.t * Id.t list
      val pp : t -> PP.t
   end

   type t
   include Printable where type printable = t

   val create : unit -> t
   val seq : t * Id.t * Node.t -> unit
   val rule : t * RuleId.t * Fragment.t -> unit
   val parents : t * Id.t -> RuleId.set
   val reconstruct : t * Id.t -> SC.t
end

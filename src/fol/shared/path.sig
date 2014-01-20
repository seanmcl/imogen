
signature Path = sig
   structure Id : Id
   type t
   include Printable where type printable = t

   val empty: t

   val size: t -> int
   val isEmpty: t -> bool
   val contains: t * Id.t -> bool
   val items: t -> Id.set

   val insert: t * Term.t * Id.t -> t
   val delete: t * Term.t * Id.t -> t

   val variants: t * Term.t -> Id.set
   val instances: t * Term.t -> Id.set
   val unifiable: t * Term.t -> Id.set
   val general: t * Term.t -> Id.set
end

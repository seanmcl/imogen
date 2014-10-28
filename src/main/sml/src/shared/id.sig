
signature Id = sig
   type t
   include Collectable where type collectable = t
   include Comparable where type comparable = t
   include Hashable where type hashable = t
   include Showable where type showable = t
   include Printable where type printable = t

   val next: unit -> t
   val toInt: t -> int
   val hashSet : hash_set -> set
   val reset : unit -> unit
end

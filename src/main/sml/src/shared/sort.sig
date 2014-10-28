
signature Sort = sig
   structure Base : sig
      datatype t = I | MWorld | LWorld | LHead | OWorld | OHead
      include Showable where type showable = t
      include Printable where type printable = t
      include Eqable where type eqable = t
      include Hashable where type hashable = t
      val parse : Parse.Sort.t option -> t
      val isInd : t -> bool
   end
   datatype t = datatype Base.t

   structure Func : sig
      type t = Base.t list * Base.t
      include Printable where type printable = t
      include Eqable where type eqable = t
   end

   structure Pred : sig
      type t = Base.t list
      include Printable where type printable = t
      include Eqable where type eqable = t
   end
end

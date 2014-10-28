
structure BackendUtil = struct
   structure Status = struct
      datatype t =
         Inconclusive
       | Proved of SC.t option
       | Saturated
   end
end

signature Backend = sig
   type t
   include Printable where type printable = t
   val ppShort : t -> PP.t

   val create :
      { stable : Focus.Stable.t
      , foci : Focus.Foci.t } -> t

   val step: t -> BackendUtil.Status.t

   structure Stats : sig
      type one
      type t
      val empty : t
      val add : t * one -> t
      val pp: t -> PP.t
   end

   val stats : t -> Stats.one
end

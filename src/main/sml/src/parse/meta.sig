
(* Metadata for a formula. *)
signature Meta = sig
   structure Pre : sig
      datatype t = Theorem | NonTheorem | Open | Unknown
      val toString : t -> string
      val pp : t -> PP.t
   end

   structure Mode : sig
      datatype t = K | T | B | K4 | S4 | S5 | P
   end

   structure Status : sig
      type t = Pre.t * Mode.t list
      val pp : t -> PP.t
   end

   datatype one =
      Status of Status.t
    | Rating of real

   type t = one list
   include Printable where type printable = t

   val rating : t -> real option
   val status : t -> Status.t option
   val nonempty : t -> bool

   val ofString : string -> t
   val ofStdin : unit -> t
   val ofFile : string -> t
end

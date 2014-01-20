
signature UnitTest = sig

   datatype t =
      Unit of unit -> unit
    | List of t list
    | Label of string * t

   val count: t -> int

   (* raises f succeeds if f () raises an exception. *)
   val raises: (unit -> 'a) -> t
   val ok: (unit -> 'a) -> t

   exception Fail of string

   val assert: (unit -> bool) -> t
   val assertMsg: (unit -> bool * string) -> t
   val fail: string -> unit

   val run: t -> unit
   val run': {verbose:bool, timeout:Time1.t option} -> t -> unit

   structure Ops : sig
      val $ : string * t -> t
      val % : (unit -> bool) -> t
      val & : t list -> t
   end

   val makeCommand: t -> Command.t
end



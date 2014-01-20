
(* The lack of first-class modules prevents us from having a slick
   command inteface like Core_command in ocaml-core *)
signature Command = sig
   structure Flag : sig
      type t

      type 'a create =
         { name : string
         , process : 'a
         , doc : string }
         -> t

      val noarg : (unit -> unit) create
      val string : (string -> unit) create
      val int : (int -> unit) create
      val set : (bool ref) create
      val unset : (bool ref) create
      val setString : (string ref) create
      val setInt : (int ref) create
   end

   type t

   val create :
      { readme : unit -> PP.t
      , summary : string
      , usageArg : string
      , flags : Flag.t list
      , run : { anons : string list } -> OS.Process.status
      } -> t

   val group :
      { readme : unit -> PP.t
      , summary : string
      , subs : (string * t) list
      } -> t

   val runArgs : t * string list -> unit
   val run : t -> unit
end

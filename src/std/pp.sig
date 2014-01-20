
signature PP = sig

   type t

   datatype mode = Page_mode | Left_mode | One_line_mode
   datatype text_details = Chr of char | Str of string
   type style = { mode : mode, line_length : int, ribbons_per_line : real }

   val char : char -> t
   val text : string -> t
   val string : string -> t
   val int : int -> t
   val bool : bool -> t
   val unit : unit -> t
   val time : Time.time -> t
   val real : real -> t
   val sized_text : int -> string -> t
   val zero_width_text : string -> t

   val semi : t
   val comma : t
   val commas : t list -> t list
   val colon : t
   val space : t
   val spaces : int -> t
   val equals : t
   val lparen : t
   val rparen : t
   val lbrack : t
   val rbrack : t
   val lbrace : t
   val rbrace : t
   val list : t list -> t
   val tuple : t list -> t

   val paren: t -> t
   val bracket : t -> t
   val option : ('a -> t) -> 'a option -> t
   val pair: t * t -> t
   val brace : t -> t
   val quote : t -> t
   val doubleQuote : t -> t

   val empty : t
   val hcat : t list -> t
   val hsep : t list -> t
   val vcat : t list -> t
   val sep : t list -> t
   val cat : t list -> t
   val fsep : t list -> t
   val fcat : t list -> t
   val nest : int -> t -> t
   val hang : t -> int -> t -> t
   val punctuate : t -> t list -> t list

   val is_empty : t -> bool

   val pp: t -> unit
   val ppl: t -> unit
   val writeFile: t * {file:string} -> unit
   val ppSingleLine: t -> unit

   val style : style
   val render_style : style -> t -> string list

   val full_render
      : mode
      -> int
      -> real
      -> (text_details * 'a -> 'a)
      -> 'a
      -> t
      -> 'a

   (* infix $$ $+$ <-> <+> *)
   structure Infix : sig
      val <> : t * t -> t
      val <+> : t * t -> t
      val $$ : t * t -> t
      val $+$  : t * t -> t
   end

   structure Ops : sig
      val $ : string -> t (* string *)
      val % : t list -> t (* hcat *)
      val %% : t list -> t (* hsep *)
      val & : t list -> t (* vcat *)
      val ~ : t (* empty *)
      val \ : t (* space 1 *)
      val \\ : t (* nbspace 2 *)
   end

   (* DEBUG *)
   (* val render : t -> string *)
   val toString : t -> string
   (* val size : t -> int *)
end

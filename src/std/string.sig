
signature String = sig

   eqtype char
   eqtype string
   eqtype t

   val ^ : string * string -> string
   val concat: string list -> string
   val explode: string -> char list
   val implode: char list -> string
   val size: string -> int
   val str: char -> string
   val substring: string * int * int -> string
   val eq: string * string -> bool

   val < : string * string -> bool
   val <= : string * string -> bool
   val > : string * string -> bool
   val >= : string * string -> bool
   val collate: (char * char -> order) -> string * string -> order
   val compare: string * string -> order
   val concatWith: string -> string list -> string
   val extract: string * int * int option -> string
   val fields: (char -> bool) -> string -> string list
   val ofCString: String.string -> string option
   val isPrefix: string -> string -> bool
   val isSubstring: string -> string -> bool
   val isSuffix: string -> string -> bool
   val map: (char -> char) -> string -> string
   val maxSize: int
   val scan: (Char.char, 'a) StringCvt.reader -> (string, 'a) StringCvt.reader
   val sub: string * int -> char
   val toCString: string -> String.string
   val tokens: (char -> bool) -> string -> string list
   val translate: (char -> string) -> string -> string

   (* strip space and newlines *)
   val lstrip: t -> t
   val rstrip: t -> t
   val strip: t -> t

   include Showable where type showable = t
   include Parseable where type parseable = t
   include Collectable where type collectable = t
   include Hashable where type hashable = t

   val isCapitalized: string -> bool
   val capitalize: string -> string
   val upcase: string -> string
   val downcase: string -> string

   (**
    * Return the first index of a char in a string.
    *)
   val index: string -> char -> int option

   (**
    * Expensive!  Copies string.
    *)
   val update: string * int * char -> string
   val space: int -> string
   val nullTerminate: string -> string

   (**
    * splitString #"_" "to_be_or_not_to_be" ~~> ["to","be","or","not","to","be"]
    *)
   val splitString: char -> string -> string list
   val lines: string -> string list

   (**
    * Simple tokenizer based on single character separators
    *)
   val chop: char list -> string -> string list

   (** [abc] *)
   val bracket: string -> string

   (** <abc> *)
   val abracket: string -> string

   (** [|abc|] *)
   val arrbracket: string -> string

   (** a,b,c *)
   val commas: string list -> string

   (** a, b, c *)
   val scommas: string list -> string

   (** {abc} *)
   val curly: string -> string

   (** (abc) *)
   val paren: string -> string

   (** a;b;c *)
   val semis: string list -> string

   (** a; b; c *)
   val ssemis: string list -> string

   (** "s" *)
   val quote: string -> string

   (** a b c *)
   val spaces: string list -> string

   (* Truncate a string at the first null character *)
   val truncateCString: string -> string

   (* join two paths *)
   val ^/ : string * string -> string

end

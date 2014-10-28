
signature Int = sig
   include INTEGER

   structure Key : OrdKey
   structure Table : MonoHashTable where type key = int
   structure Map : OrdMap where type key = int
   structure Set : OrdSet where type item = int
   structure HashSet : HashSet where type item = int

   val log: int -> int -> int
   (** log base n *)

   val eq: int * int -> bool
   val hash: int -> word
   val sqrt: int -> int
   val log2: int -> int
   val log10: int -> int
   val pow: int * int -> int
   val sum: int list -> int
   val prod: int list -> int
   val odd: int -> bool
   val even: int -> bool
   val pp: int -> PP.t
   val maxList: int list -> int
   val minList: int list -> int
   val ofString: string -> int option
end


signature General = sig
   type 'a array
   datatype bool = datatype bool
   type char
   type exn
   type int
   datatype 'a option = NONE | SOME of 'a
   datatype order = LESS | EQUAL | GREATER
   datatype either = datatype Either.t
   datatype result = datatype Result.t
   datatype list = datatype list
   datatype ref = datatype ref
   type string
   type unit

   val = : ''a * ''a -> bool
   val <> : ''a * ''a -> bool

   val < : int * int -> bool
   val <= : int * int -> bool
   val > : int * int -> bool
   val >= : int * int -> bool
   val + : int * int -> int
   val - : int * int -> int
   val * : int * int -> int
   val ~ : int -> int

   val fst : 'a * 'b -> 'a
   val snd : 'a * 'b -> 'b
   val ! : 'a ref -> 'a
   val := : 'a ref * 'a -> unit
   val @ : ('a list * 'a list) -> 'a list
   val ^ : string * string -> string
   val ^/ : string * string -> string
   val before : 'a * unit -> 'a
   val ignore : 'a -> unit
   val not : bool -> bool
   val o : ('a -> 'b) * ('c -> 'a) -> 'c -> 'b
   val print : string -> unit
   val printl : string -> unit
   val map : ('a -> 'b) -> 'a list -> 'b list
   val app : ('a -> unit) -> 'a list -> unit
   val failwith : string -> 'a
   val failwith' : PP.t -> 'a
   val mod : int * int -> int
   val div : int * int -> int

   val isSome: 'a option -> bool
   val isNone: 'a option -> bool
   val id: 'a -> 'a
   val curry: ('a * 'b -> 'c) -> 'a -> 'b -> 'c
   val swap: ('a * 'b -> 'c) -> 'b * 'a -> 'c
   val flip: ('a -> 'b -> 'c) -> 'b -> 'a -> 'c
   val list: 'a -> 'a list
   val rev : 'a list -> 'a list
   val length : 'a list -> int
   val foldr : ('a * 'b -> 'b) -> 'b -> 'a list -> 'b
   val foldl : ('a * 'b -> 'b) -> 'b -> 'a list -> 'b
   val assert : (unit -> bool) * (unit -> PP.t) -> unit
   val asserts : (unit -> bool) * string -> unit
   val assert' : (unit -> bool) -> unit
   val uassert : (unit -> unit) -> unit
   val valOf : 'a option -> 'a
   val null : 'a list -> bool

   exception Impossible
   exception Unimplemented
   exception Fail of string
   exception Overflow
   exception NotFound

   val noWarnUnused : 'a -> unit
end

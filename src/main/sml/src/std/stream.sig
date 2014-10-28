
signature Stream = sig

   type 'a t
   datatype 'a front = Nil | Cons of 'a * 'a t
   exception Undefined
   exception Empty
   val force: 'a t -> 'a front
   val delay: (unit -> 'a front) -> 'a t
   val empty: 'a t
   val undefined: 'a t
   val isUndefined: 'a t -> bool
   val isEmpty: 'a t -> bool
   val map: ('a -> 'b) -> 'a t -> 'b t
   val mapi: (int * 'a -> 'b) -> 'a t -> 'b t
   val app: ('a -> unit) -> 'a t -> unit
   val all: ('a -> bool) -> 'a t -> bool
   val exists: ('a -> bool) -> 'a t -> bool
   val repeat: 'a -> 'a t
   val filter: ('a -> bool) -> 'a t -> 'a t
   val append: 'a t * 'a t -> 'a t
   val append1: 'a t * 'a -> 'a t
   val foldr: ('a * 'b -> 'b) -> 'b -> 'a t -> 'b
   val foldl: ('a * 'b -> 'b) -> 'b -> 'a t -> 'b
   val take: 'a t * int -> 'a t
   val drop: 'a t * int -> 'a t
   val index: ('a -> bool) -> 'a t -> int option
   val length: 'a t -> int
   val toList: 'a t -> 'a list
   val find: ('a -> bool) -> 'a t -> 'a option
   val findIndices: ('a -> bool) -> 'a t -> int t
   val ofList: 'a list -> 'a t
   val head: 'a t -> 'a
   val tail: 'a t -> 'a t
   val last: 'a t -> 'a
   val init: 'a t -> 'a t
   val inits: 'a t -> 'a t t
   val memBy: ('a * 'a -> bool) -> 'a * 'a t -> bool
   val mem: ''a * ''a t -> bool
   (* raises Empty *)
   val span: ('a -> bool) -> 'a t -> 'a t * 'a t
   val cons: 'a * 'a t -> 'a t
   val concat: 'a t t -> 'a t
   val singleton: 'a -> 'a t
   val join: ('a * 'b -> 'c) -> 'a t * 'b t -> 'c t
   (**
    * in GHC
    * ? [x:xs|x <- [1..4], xs <- [[5],[6],[7]]]
    * [[1,5],[1,6],[1,7],[2,5],[2,6],[2,7],[3,5],[3,6],[3,7],[4,5],[4,6],[4,7]]
    *)
   val joinBy: ('a * 'b -> 'c) -> 'a t * ('a -> 'b t) -> 'c t
   val nth : 'a t * int -> 'a
   (* raises Empty *)
   val eq: ''a t * ''a t -> bool
   val eqBy: ('a * 'a -> bool) -> 'a t * 'a t -> bool
   val finite: 'a t -> bool
   val nubBy: ('a * 'a -> bool) -> 'a t -> 'a t
   val nub: ''a t -> ''a t
   val sort: ('a * 'a -> order) -> 'a t -> 'a t
   val lexOrder: ('a * 'a -> order) -> 'a t * 'a t -> order
   val stringStream: string -> char t
   val fileStream: string -> char t
   (* doesn't terminate for infinite ts *)
   val toString: ('a -> string) -> 'a t -> string
   val toString2: ('a -> string) -> 'a t t -> string
   (* write the elements as you force them *)
   val outputBy: (TextIO.outstream * 'a -> unit)
      -> TextIO.outstream * 'a t -> unit
   val print: ('a -> unit) -> 'a t -> unit
   structure Ops: sig
      val !! : 'a t * int -> 'a (* nth *)
      val $ : 'a t -> 'a list (* toList *)
      val % : 'a list -> 'a t (* ofList *)
   end

end


(*
 structure S = Stream
 fun fib 0 = 1 | fib 1 = 1 | fib n = fib (n-1) + fib (n-2);
 fun fibStream n = S.delay (fn () => S.Cons (fib n,fibStream (n+1)))
 val fibStream = fibStream 0

 open S
 infix !!

 fibStream !! 40
 fibStream !! 41
 *)

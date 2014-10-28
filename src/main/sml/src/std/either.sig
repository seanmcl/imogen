
signature Either = sig
   datatype ('a, 'b) t =
      Left of 'a
    | Right of 'b

   val either: ('a -> 'c) -> ('b -> 'c) -> ('a, 'b) t -> 'c
   val lefts: ('a, 'b) t list -> 'a list
   val rights: ('a, 'b) t list -> 'b list
   val leftExn : ('a, 'b) t -> 'a
   val rightExn : ('a, 'b) t -> 'b
   val partition: ('a, 'b) t list -> 'a list * 'b list
   val compare
      :  ('a * 'a -> order)
      -> ('b * 'b -> order)
      -> ('a, 'b) t * ('a, 'b) t
      -> order
end

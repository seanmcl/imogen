
(* Lists with both ends accessible.  This should be implemented as in
   Haskell's Data.Sequence, but that requires non-regular datatypes.  *)
signature Dlist = sig
   type 'a t

   datatype 'a left =
      EmptyL
    | Cons of 'a * 'a t
   val viewl : 'a t -> 'a left

   datatype 'a right =
      EmptyR
    | Snoc of 'a t * 'a
   val viewr : 'a t -> 'a right

   datatype 'a left2 =
      EmptyL2
    | SingL2 of 'a
    | Cons2 of 'a * 'a * 'a t
   val viewl2 : 'a t -> 'a left2

   datatype 'a right2 =
      EmptyR2
    | SingR2 of 'a
    | Snoc2 of 'a t * 'a * 'a
   val viewr2 : 'a t -> 'a right2

   val empty : 'a t
   val isEmpty : 'a t -> bool
   val cons : 'a * 'a t -> 'a t
   val snoc : 'a t * 'a -> 'a t
   val singleton : 'a -> 'a t
   val ofList : 'a list -> 'a t
   val toList : 'a t -> 'a list
   val append : 'a t * 'a t -> 'a t
   val map : ('a -> 'b) -> 'a t -> 'b t
   val foldr : ('a * 'b -> 'b) -> 'b -> 'a t -> 'b
   val foldr1 : ('a * 'a -> 'a) -> 'a t -> 'a
end

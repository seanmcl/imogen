
structure Dlist :> Dlist = struct
   open General

   type 'a t = 'a list * 'a list

   val empty = ([], [])
   fun cons (x, (xs, ys)) = (x :: xs, ys)
   fun snoc ((xs, ys), y) = (xs, y :: ys)
   fun singleton x = ([x], [])
   fun ofList l = (l, [])
   fun toList (xs, ys) = xs @ rev ys
   fun append ((xs1, ys1), (xs2, ys2)) = (xs1 @ rev ys1, ys2 @ rev xs2)

   val isEmpty = fn
      ([], []) => true
    | _ => false

   datatype 'a left =
      EmptyL
    | Cons of 'a * 'a t

   val viewl = fn
      ([], []) => EmptyL
    | (x :: xs, ys) => Cons (x, (xs, ys))
    | ([], ys) =>
      case rev ys of
         [] => raise Impossible
       | y :: ys => Cons (y, (ys, []))

   datatype 'a right =
      EmptyR
    | Snoc of 'a t * 'a

   val viewr = fn
      ([], []) => EmptyR
    | (xs, y :: ys) => Snoc ((xs, ys), y)
    | (xs, []) =>
      case rev xs of
         [] => raise Impossible
       | y :: ys => Snoc (([], ys), y)

   datatype 'a left2 =
      EmptyL2
    | SingL2 of 'a
    | Cons2 of 'a * 'a * 'a t

   fun viewl2 (xs, ys) = case xs @ rev ys of
      []  => EmptyL2
    | [x] => SingL2 x
    | x1 :: x2 :: xs => Cons2 (x1, x2, (xs, []))

   datatype 'a right2 =
      EmptyR2
    | SingR2 of 'a
    | Snoc2 of 'a t * 'a * 'a

   fun viewr2 (xs, ys) = case ys @ rev xs of
      []  => EmptyR2
    | [x] => SingR2 x
    | y1 :: y2 :: ys => Snoc2 (([], ys), y2, y1)

   fun map f (xs, ys) = (List.map f xs, List.map f ys)

   fun foldr f x l = List.foldr f x (toList l)
   fun foldr1 f l = List1.foldr1 f (toList l)

end

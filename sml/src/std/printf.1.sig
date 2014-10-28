
signature Printf = sig
   type ('a, 'b) t
   val printf : ('a, unit) t -> 'a
   val sprintf : ('a, string) t -> 'a
   val d : (int -> 'b, 'b) t
   val s : (string -> 'b, 'b) t
   val b : (bool -> 'b, 'b) t
   val n : string -> ('a, 'a) t
   structure Ops : sig
      val $ : string * ('a, 'b) t -> ('a, 'b) t
      val ` : ('a, 'b) t * ('b, 'c) t -> ('a, 'c) t
   end
end

functor PrintfTest (P : Printf) = struct
   open P
   open P.Ops
   infixr 5 $
   infixr 5 `
   val _ =
      let in
         printf d 5
       ; printf ("num: " $ d) 5
       ; printf ("num: " $ d ` s) 5 "sean"
       ; printf ("num: " $ d ` ", name: " $ s) 5 "sean"
       ; printf ("num: " $ d ` ", name: " $ s ` n "\n") 5 "sean"
      end
end

structure Printf :> Printf = struct
   infixr 5 $
   infixr 5 `

   type ('a, 'b) t = (string -> 'b) -> 'a

   fun sprintf f = f (fn s => s)
   fun printf f = f print

   fun d k n = k (Int.toString n)
   fun s k n = k n
   fun b k n = k (Bool.toString n)
   fun n s k = k s

   structure Ops = struct
      fun (s $ t) k = t (fn s' => k (s ^ s'))
      fun (t1 ` t2) k = t1 (fn s1 => t2 (fn s2 => k (s1 ^ s2)))
   end

end


signature Printf = sig
   type ('a, 'b) t
   val printf : (unit, 'a) t -> 'a
   val sprintf : (string, 'a) t -> 'a
   val ` : string -> ('a, 'a) t
   val D : (int -> 'a, 'b) t * string -> ('a, 'b) t
   val S : (string -> 'a, 'b) t * string -> ('a, 'b) t
   val B : (bool -> 'a, 'b) t * string -> ('a, 'b) t
end

functor PrintfTest (P : Printf) = struct
   open P
   infix D S B
   val _ =
      let in
         printf (`"{num = " D "}\n") 5
       ; printf (`"{num = " D ", name = " S "}\n") 5 "sean"
      end
end

structure Printf :> Printf = struct
   type ('a, 'b) t = (string -> 'a) -> 'b

   fun sprintf f = f (fn s => s)
   fun printf f = f print

   fun D (t, s) k' = t (fn s' => fn n => k' (s' ^ Int.toString n ^ s))
   fun S (t, s) k' = t (fn s' => fn s'' => k' (s' ^ s'' ^ s))
   fun B (t, s) k' = t (fn s' => fn b => k' (s' ^ Bool.toString b ^ s))
   fun ` n k = k n
end

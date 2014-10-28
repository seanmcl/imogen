
structure Int1 = struct
   fun sqrt x = Real.floor (Math.sqrt (real x))

   val equal = op=
   val hash = Word1.ofInt

   structure Key = struct
      type t = int
      val compare = Int.compare
      val pp = PP.int
      val hash = hash
      val eq = equal
   end

   structure Table = HashTableFn (Key)
   structure Map = OrdMapFn(Key)
   structure Set = OrdSetFn(Key)
   structure HashSet = HashSetFn(Key)

   open Key

   (* n^k by repeated squaring, cps form *)
   fun pow (n, k) =
      let
         fun pow (_, 0) c = c 1
           | pow (n, 1) c = c n
           | pow (n, k) c =
            let
               val k' = k div 2
               val odd = k mod 2 = 1
            in
               pow (n, k') (fn res => if odd then c (n * res * res) else c (res * res))
            end
      in
         if k >= 0 then pow (n, k) Fun.id else raise Fail "pow: negative exponent"
      end

   local
      fun log' store base n = if n < base then store else log' (1 + store) base (n div base)
   in
      fun log base n = log' 0 base n
   end

   val log2 = log 2
   val log10 = log 10

   fun sum l = foldr op+ 0 l
   fun prod l = foldr op* 1 l

   fun odd x = x mod 2 = 1
   fun even x = x mod 2 = 0

   fun maxList' [] x = x
     | maxList' (h::t) x = if h > x then maxList' t h else maxList' t x

   fun maxList (h::t) = maxList' t h
     | maxList [] = raise List.Empty

   fun minList' [] x = x
     | minList' (h::t) x = if h < x then minList' t h else minList' t x

   fun minList (h::t) = minList' t h
     | minList [] = raise List.Empty

   open Int

   val eq = equal
   val ofString = fromString
end


structure String2 = struct
   open String1

   type t = string
   type collectable = t
   type hashable = t
   type parseable = t
   type showable = t

   structure Key = struct
      type t = string
      val compare = compare
      val hash = HashString.hashString
      val eq = op=
      val pp = PP.text
   end

   structure C = HashableFn (Key)
   open C

   structure C = CollectableFn (Key)
   open C

   val hash = HashString.hashString
   val equal = op=
   val ofCString = fromCString
   val ofString = Fun.id

   fun ws c = List1.mem (c, [#" ", #"\n", #"\r"])

   fun lstrip s =
      let
         open Int
         val n = String.size s
         fun loop k =
            if k >= n - 1 then NONE
            else if ws (String.sub (s, k)) then loop (k+1)
            else SOME k
      in
         case loop 0 of
            NONE => ""
          | SOME k => substring (s, k, n-k)
      end

   fun rstrip s =
      let
         open Int
         val n = String.size s
         fun loop k =
            if k <= 0 then NONE
            else if ws (String.sub (s, k)) then loop (k-1)
            else SOME k
      in
         case loop (n-1) of
            NONE => ""
          | SOME k => substring (s, 0, k+1)
      end

   val strip = lstrip o rstrip
   val eq = equal

   val _ = fn _ : hashable * collectable => ()
end

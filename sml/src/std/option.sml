
structure Option : Option = struct
   structure O = Option

   fun compare p = fn
      (NONE,NONE) => EQUAL
    | (NONE,_) => LESS
    | (_,NONE) => GREATER
    | (SOME x,SOME y) => p (x,y)

   val extract = fn
      (SOME x, _) => x
    | (NONE, exn) => raise exn

   fun option NONE x _ = x
     | option (SOME x) _ f = f x

   fun value t x = option t x Fun.id

   val isNone = fn
      NONE => true
    | SOME _ => false

   open O
end

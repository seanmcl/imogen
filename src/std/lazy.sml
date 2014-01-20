
structure Lazy :> Lazy = struct

   datatype 'a susp = NotYet of unit -> 'a
                    | Done of 'a

   type 'a t = unit -> 'a susp ref

   exception Undefined

   fun delay f =
      let
         val r = ref (NotYet f)
      in
         fn () => r
      end

   fun force f =
      case f () of
         ref (Done x) => x
       | r as ref (NotYet f') =>
         let
            val a = f'()
         in
            r := Done a
          ; a
         end

   val undefined = fn () => raise Undefined

   fun inject x = delay (fn () => x)

   fun isUndefined x =
      (ignore (force x)
      ; false)
      handle Undefined => true

   fun toString f x = if isUndefined x then "_|_" else f (force x)

   fun eqBy p (x,y) = p (force x,force y)
   fun eq (x,y) = eqBy op= (x,y)
   fun compare p (x,y) = p (force x,force y)

   structure Ops =
      struct
         val ! = force
         val ? = inject
         val % = delay
      end

   fun map f x = delay (fn () => f (force x))

end

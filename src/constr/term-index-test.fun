
functor TermIndexTestFn(Index : TermIndex where type Id.t = Id.t) : Test = struct
   structure T = UnitTest
   structure X = Index

   open UnitTest.Ops
   open General

   infix ==

   val i1 = Id.next ()
   val t = Term.ofString

   val t0 = T.Unit (fn () =>
      let
         val d = X.create ()
         fun ins t id = X.insert (d, t, id)
         fun del t id = X.delete (d, t, id)
         val t1 = t "f(X)"
      in
         ins t1 i1
       ; assert' (fn () => X.size d = 1)
       ; del t1 i1
       ; assert' (fn () => X.size d = 0)
      end)

   val test = &[ t0 ]
end

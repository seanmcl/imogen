
functor IndexTestFn (Index : Index where type Id.t = Id.t) : Test = struct
   structure U = UnitTest
   structure X = Index
   structure T = UnitTest

   open UnitTest.Ops
   open General

   infix ==

   val q = Seq.ofString

   val t0 : U.t = T.Unit (fn () =>
      let
         val d = X.create {global=NONE, pp = PP.unit}
         fun ins q = X.insert (d, Seq.id q, q, ())
         fun rem q = X.remove (d, Seq.id q)
         val q1 = q "|- f(X)"
         val q2 = q "|- f(c)"
      in
         ins q1
       ; assert' (fn () => X.size d = 1)
       ; rem q1
       ; assert' (fn () => X.size d = 0)
       ; rem q1
       ; assert' (fn () => X.size d = 0)
       ; ins q1
       ; assert' (fn () => (ins q1; false) handle _ => true)
       ; assert' (fn () => X.size d = 1)
       ; assert' (fn () => X.subsumes (d, q2))
       ; ins q2
       ; assert' (fn () => X.size d = 2)
       ; X.removeSubsumed (d, q2)
       ; assert' (fn () => X.size d = 1)
      end)

   val test = &[ t0 ]
end

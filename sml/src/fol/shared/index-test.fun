
functor IndexTestFn (Index : Index where type Id.t = Id.t) : Test = struct
   structure U = UnitTest
   structure X = Index
   structure T = UnitTest

   open UnitTest.Ops
   open General

   infix ==

   val q = Seq.ofString

   fun assert (b, msg) = if b () then () else U.fail msg
   fun raises (b, msg) =
      let
         val _ = Debug.print_error_on_failure := false
         val raised = (ignore (b ()); false) handle _ => true
      in
         Debug.print_error_on_failure := true
       ; if raised then () else raise U.Fail msg
      end

   val t0 : U.t = T.Unit (fn () =>
      let
         val d = X.create {global=NONE, pp = PP.unit}
         fun ins q = X.insert (d, Seq.id q, q, ());
         fun rem q = X.remove (d, Seq.id q)
         val q1 = q "|- f(X)"
         val q2 = q "|- f(c)"
      in
         ins q1
       ; assert (fn () => X.size d = 1, "1")
       ; rem q1
       ; assert (fn () => X.size d = 0, "2")
       ; rem q1
       ; assert (fn () => X.size d = 0, "3")
       ; ins q1
       ; raises (fn () => ins q1, "4")
       ; assert (fn () => X.size d = 1, "5")
       ; assert (fn () => X.subsumes (d, q2), "6")
       ; ins q2
       ; assert (fn () => X.size d = 2, "7")
       ; X.removeSubsumed (d, q2)
       ; assert (fn () => X.size d = 1, "8")
      end)

   val test = &[ t0 ]
end

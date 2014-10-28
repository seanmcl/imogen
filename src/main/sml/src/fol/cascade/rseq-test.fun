
functor RSeqTestFn (RSeq : RSeq) : Test = struct
   structure T = UnitTest
   open General
   open UnitTest.Ops
   infix ==

   fun r s = RSeq.ofString s

   val vars =
      map Var.ofString
         ["X", "Y", "Z", "X0", "X1", "X2", "X3", "X4", "X5", "X6"
         , "X7", "X8", "X9", "X10", "X11"]

   val params = map Param.ofString ["@a", "@a1", "@a2", "@b", "@c#", "@d#"]

   val atoms = Atoms.make (Var.Set.ofList vars, Param.Set.ofList params)

   fun subs (t1, t2) = RSeq.subsumes (r t1, r t2, {atoms = atoms, global = Seq.Ants.empty})
   fun comb (t1, t2) = RSeq.combine (r t1, r t2)
   (* fun combEq (t1, t2, t3) = RSeq.eq (valOf (comb (t1, t2)), r t3) *)

   val t0 =
      T.assert (fn () =>
         subs
            ( "q(X, X1, X1, X) |- p23 {X2 -> X1,X3 -> X}"
            , "q(X1, X1, X1, X1) |- p23 {X -> X1,X2 -> X1,X3 -> X1}" ))

   (* val t1 = *)
   (*    %(fn () => *)
   (*         T.assert *)
   (*            (combEq *)
   (*                ( "|- p {X -> Y, Z -> c, A -> d}" *)
   (*                , "|- p {X -> c}" *)
   (*                , "|- p {X -> c, Z -> c, A -> d}"))) *)

   val t1 = %(fn () => true)

   val t2 =
      T.assert
         (fn () =>
            isNone (comb
                       ( "|- p {X -> @c#, Y -> f}"
                       , "|- p {Y -> g}")))

   val t3 =
      T.raises
         (fn () =>
            (comb
                ( "|- p {X -> @c# }"
                , "|- p {X -> @c}")))

   (* val t4 = *)
   (*    %(fn () => *)
   (*         T.assert (combEq *)
   (*             ( "|- p(X1, @a1) {Y -> X1, Z -> @a1}" *)
   (*             , "|- p(X2, @a2) {X -> X2, Y -> @a2}" *)
   (*             , "|- p(@a3, @a3) {X -> @a3, Y -> @a3, Z -> @a3}"))) *)

   val test = &[ t0, t1, t2, t3 ]
end

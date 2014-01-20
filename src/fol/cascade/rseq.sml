
structure RSeq :> RSeq = struct
   structure Cons = Seq.Cons
   structure Ants = Seq.Ants

   open General
   open PP.Ops

   type t = Seq.t * Subst.t
   type printable = t
   type parseable = t
   type eqable = t

   fun eq ((q1, s1), (q2, s2)) = Seq.eq (q1, q2) andalso Subst.eq (s1, s2)

   fun invariant ((seq, theta), {global, atoms=_, fresh}) =
      let in
         Subst.invariant (theta)
       ; Seq.invariant (seq, {global = global, fresh = fresh})
       ; asserts
            (fn () => Seq.eq (seq, Seq.apply (seq, theta)), "rseq: subst not applied")
      end

   val make = Fun.id
   val dest = Fun.id

   fun pp (q, t) = %[Seq.pp q, $":", Subst.pp t]

   fun ofSeq q = (q, Subst.id)

   fun parse (q, t) = (Seq.parse q, Subst.parse t)
   val ofString = parse o Parse.RSeq.ofString

   val seq = fst
   (* val subst = snd *)
   (* fun apply ((q, t1), t2) = (Seq.apply (q, t2), Subst.compose (t1, t2)) *)

   (* fun restrict ((q, t), atoms) = (q, Subst.restrict (t, atoms)) *)

   fun combine (r1 as (t1, s1), r2 as (t2, s2)) =
      let
         (* val _ = Log.trace (fn () => *)
         (*    &[ $"Rseq.combine:" *)
         (*     , %[\\, &[ pp r1 *)
         (*              , pp r2]]]) *)
         val () = noWarnUnused (r1, r2)
         val res = case Cons.unify (Seq.cons t1, Seq.cons t2) of
            NONE => NONE
          | SOME (cons, sc) =>
            case Unif.plusl [s1, s2, sc] of
               NONE => NONE
             | SOME s =>
               let
                  (* val _ = Log.trace (fn () => &[ %[$"cons: ", Subst.pp sc] *)
                  (*                              , %[$"s1  : ", Subst.pp s1] *)
                  (*                              , %[$"s2  : ", Subst.pp s2] *)
                  (*                              , %[$"s   : ", Subst.pp s] *)
                  (*                              ]) *)
                  val ants = Ants.union (Seq.ants t1, Seq.ants t2)
                  val seq = Seq.apply (Seq.new (ants, cons), s)
               in
                  SOME (seq, s)
               end
      in
         (* Log.trace (fn () => *)
         (*               &[ $"Rseq.combine:" *)
         (*                , %[\\, &[ pp r1 *)
         (*                         , pp r2 *)
         (*                         , PP.option pp res ]]]); *)
         res
      end

   fun unfix (t, s) = (Seq.unfix t, Subst.unfix s)

   fun atoms (t, _) = Seq.atoms t

   (* fun rename (t, s) = *)
   (*    let *)
   (*       val (t, s) = Seq.rename (t, s) *)
   (*       val s = Subst.rename s *)
   (*    in *)
   (*       (Seq.apply (t, s), s) *)
   (*    end *)

   fun subsumes' (r1 as (seq1, theta1), r2 as (seq2, theta2), {global, atoms}) =
      let
         (* val _ = Log.trace (fn () => &[$"-> RSeq.subsumes': ", &[pp r1, pp r2, %[$"atoms: ", Atoms.pp atoms]]]) *)
         val () = noWarnUnused (r1, r2)
         val theta1 = Subst.restrict (theta1, atoms)
         val res = case Unif.subsumes' (theta1, theta2, atoms) of
            NONE => NONE
          | SOME theta =>
            let
               val seq1 = Seq.apply (seq1, theta)
            in
               case Seq.subsumes' (seq1, seq2, {global=SOME global}) of
                  NONE => NONE
                | SOME theta' => SOME (Subst.compose (theta, theta'))
            end
      in
         (* Log.trace (fn () => %[$"<- RSeq.subsumes': ", PP.option Subst.pp res]); *)
         res
      end

   val subsumes = isSome o subsumes'

   val () = noWarnUnused (fn _ : printable * parseable * eqable => (eq))

end


structure Seq :> Seq = struct
   structure U = Unicode
   structure PSet = Param.Set
   structure P = Parse
   structure F = LFormula

   open General
   open PP.Ops

   type atoms = Atoms.t

   (* ----------------------------------------------------------------------- *)

   structure Cons = struct
      datatype t =
         P of Rel.t
       | Xi

      fun map f = fn
         Xi => Xi
       | P r => P (f r)

      val unify = fn
         (P r, Xi) => SOME (P r, Subst.id)
       | (Xi, P r) => SOME (P r, Subst.id)
       | (Xi, Xi) => SOME (Xi, Subst.id)
       | (P r, P r') =>
         case Unif.Rel.unify (r, r') of
            NONE => NONE
          | SOME theta => SOME (P (Rel.apply (r, theta)), theta)

      val eq = fn
         (P p, P p') => Rel.eq (p, p')
       | (Xi, Xi) => true
       | _ => false

      val pp = fn
         (P p) => Rel.pp p
       | Xi => $U.cdot

      fun apply (c, t) = map (fn r => Rel.apply (r, t)) c

      val isXi = fn
         Xi => true
       | _ => false

      val preds = fn
         Xi => Pred.Set.empty
       | P p => Pred.Set.singleton (Rel.pred p)

      val atoms = fn
         Xi => Atoms.empty
       | P r => Rel.atoms r

      val rename = fn
         (Xi, theta) => (Xi, theta)
       | (P rel, theta) =>
         let
            val (r, theta') = Rel.rename (rel, theta)
         in
            (P r, theta')
         end

      fun fix' ps t = map (Rel.fix' ps) t
      fun fix t = map Rel.fix t
      fun unfix t = map Rel.unfix t

      val match = fn
         (Xi, _) => SOME {cons = Xi, theta = Subst.id, filledHole = false }
       | (P r, Xi) => SOME {cons = P r, theta = Subst.id, filledHole = true }
       | (P r1, P r2) =>
         case Unif.Rel.unify (r1, r2) of
            NONE => NONE
          | SOME theta => SOME {cons = Xi, theta = theta, filledHole = false }

      structure Ops = struct
         datatype ops = datatype t
      end

      val () = noWarnUnused (isXi)
   end
   datatype cons = datatype Cons.t

   (* ----------------------------------------------------------------------- *)

   structure Ants : sig
      type t
      include Printable where type printable = t
      include Eqable where type eqable = t
      include Fixable where type fixable = t
      val fix' : Atoms.t -> t -> t
      val empty: t
      val ofList: Rel.t list -> t
      val mem: Rel.t * t -> bool
      val size: t -> int
      val app: (Rel.t -> unit) -> t -> unit
      val fold: (Rel.t * 'a -> 'a) -> 'a -> t -> 'a
      val map: (Rel.t -> Rel.t) -> t -> t
      val isEmpty: t -> bool
      val union: t * t -> t
      val intersection: t * t -> t
      val difference: t * t -> t
      val filter: (Rel.t -> bool) -> t -> t
      val hd: t -> Rel.t option
      (* val isSubset: t * t -> t *)
      val preds: t -> Pred.set
      val atoms: t -> Atoms.t
      val apply: t * Subst.t -> t
      val rename: t * Subst.t -> t * Subst.t
      val choose: t -> (Pred.t * Rel.set * t) option
      val choose1: t -> (Rel.t * t) option
      val maybeSubsumes: t * t -> bool
      val remove: t * Pred.t -> (t * Rel.set) option
      val contract: t * {global:t} -> (t * Subst.t) list
   end = struct
      structure M = Pred.Map
      structure S = Rel.Set

      (* Invariants: in A ([(p1, s1), (p2, s2), ..., (pn, sn)], k)
         - pi ≠ ∅
         - Σ|pi| = k *)
      datatype t = A of Rel.set Pred.map * int

      type eqable = t
      type printable = t
      type fixable = t

      val empty = A (M.empty, 0)

      fun foldM f x m =
         let
            fun foldFn (l, acc) = S.foldr f acc l
         in
            M.foldr foldFn x m
         end

      fun fold f x (A (m, _)) = foldM f x m

      fun size (A (_, n)) = n

      fun sizeM m = foldM (fn (_, acc) => 1 + acc) 0 m

      fun invariant (A (ants, n)) =
         let in
            asserts (fn () => sizeM ants = n, "ant size mismatch")
          ; asserts (fn () => Pred.Map.all (not o Rel.Set.isEmpty) ants, "empty rel set")
         end

      fun check seq = (invariant seq; seq)

      fun make m = check (A (m, sizeM m))

      fun appM f ants =
         let
            fun appFn l = S.app f l
         in
            M.app appFn ants
         end

      fun app f (A (ants, _)) = appM f ants

      fun mem (r, A (ants, _)) = case M.find (ants, Rel.pred r) of
         NONE => false
       | SOME rs => Rel.Set.mem (rs, r)

      fun all f ants =
         WithReturn.f
            (fn g =>
               let in
                  app (fn rel => if f rel then () else g false) ants
                ; true
               end)

      fun mapM f m =
         let
            fun f' s =
               let
                  val s = S.map f s
               in
                  if S.isEmpty s then NONE else SOME s
               end
         in
            M.mapPartial f' m
         end

      (* Recalculate the size in case ants were contracted. *)
      fun map f (A (m, _)) = make (mapM f m)

      fun eq (A (m1, _), A (m2, _) : eqable) = M.eqBy S.eq (m1, m2)

      fun fix (ants:fixable) = map Rel.fix ants
      fun fix' ps ants = map (Rel.fix' ps) ants
      fun unfix ants = map Rel.unfix ants
      fun isFixed _ = raise Unimplemented

      val extend: t * Rel.t list -> t =
         fn (A (m, _), rels) =>
            let
               fun f (rel, m) =
                  let
                     val p = Rel.pred rel
                  in
                     case M.find (m, p) of
                        NONE => M.insert (m, p, S.singleton rel)
                      | SOME rs => M.replace (m, p, S.add (rs, rel))
                  end
               val m = foldl f m rels
            in
               make m
            end

      fun filter f m = fold (fn (r, acc) =>
         if f r then extend (acc, [r]) else acc) empty m

      (* By invariant, if a Prop has an entry, it's nonempty,
         so we don't need to check for NONE in choose' *)
      fun hd (A (m, _)) = Option.map Rel.Set.choose' (Pred.Map.first m)

      fun ofList l = check (extend (empty, l))

      fun isEmpty (A (_, n)) = n = 0

      fun find (A (ants, _), p) = M.find (ants, p)

      fun findExn ap = case find ap of
         NONE => raise Impossible
       | SOME rs => rs

      fun union (A (m1, _), A (m2, _)) =
         make (M.unionWith S.union (m1, m2))

      fun intersection (A (m1, _), A (m2, _)) =
         let
            val f = fn
               (_, NONE) => NONE
             | (NONE, _) => NONE
             | (SOME x, SOME y) =>
               let val s = S.intersection (x, y) in
                  if S.isEmpty s then NONE else SOME s
               end
            val m = M.mergeWith f (m1, m2)
         in
            make m
         end

      fun difference (A (m1, _), A (m2, _)) =
         let
            val f = fn
               (x, NONE) => x
             | (NONE, _) => NONE
             | (SOME x, SOME y) =>
               let val s = S.difference (x, y) in
                  if S.isEmpty s then NONE else SOME s
               end
            val m = M.mergeWith f (m1, m2)
         in
            make m
         end

      fun atoms ants =
         fold (fn (rel, acc) => Atoms.union (Rel.atoms rel, acc))
            Atoms.empty ants

      fun preds (A (ants, _)) = Pred.Set.ofList (M.listKeys ants)

      val pp : printable -> PP.t =
         let
            fun ppRels rels =
               %(PP.punctuate ($", ")
                    (List.map Rel.pp (S.toList rels)))
         in
            fn A (ants, _) =>
               %(PP.punctuate ($", ")
                    (List.map ppRels (M.toList ants)))
         end

      fun apply (A (m, _), theta) =
         make (M.map (S.map (fn r => Rel.apply (r, theta))) m)

      val rename =
         let
            fun renameRels (rs, theta) =
               let
                  fun foldFn (r, (rs, theta)) =
                     let
                        val (r, theta) = Rel.rename (r, theta)
                     in
                        (S.add (rs, r), theta)
                     end
               in
                  S.foldr foldFn (S.empty, theta) rs
               end
            fun foldFn (k, ant, (ants, theta)) =
               let
                  val (ant, theta) = renameRels (ant, theta)
               in
                  (M.replace (ants, k, ant), theta)
               end
         in
            fn (A (m, n), theta) =>
               let
                  val (m, theta) = M.foldri foldFn (M.empty, theta) m
                  val res = check (A (m, n))
               in
                  (res, theta)
               end
         end

      val maybeSubsumes =
         let
            fun setFuncs rels = S.foldr (fn (rel, acc) =>
               Func.Set.union (acc, Rel.funcs rel)) Func.Set.empty rels
         in
            fn (a1 as A (m1, _), a2 as A (m2, _)) =>
               (* Prop and function symbols must be fewer in subsuming ants *)
               Pred.Set.subset (preds a1, preds a2)
               andalso
               M.alli
                  (fn (p, rels1) =>
                      case M.find (m2, p) of
                         NONE => true
                       | SOME rels2 =>
                         Func.Set.subset (setFuncs rels1, setFuncs rels2))
                  m1
         end

      fun choose (A (m, n)) = case M.choose m of
         NONE => NONE
       | SOME (p, rels, unused) =>
         SOME (p, rels, A (unused, n - S.size rels))

      fun choose1 (A (m, n)) = case M.choose m of
         NONE => NONE
       | SOME (p, rels, unused) =>
         case S.choose rels of
            NONE => raise Impossible
          | SOME (rels, rel) =>
            SOME (rel, A (M.replace (unused, p, rels), n - 1))

      fun remove (A (m, n), p) = case M.remove (m, p) of
         NONE => NONE
       | SOME (m, rels) =>
         SOME (check (A (m, n - S.size rels)), rels)

      fun contract (ants as A (m, _), {global as A (gm, _)}) =
         let
            fun loop m = case M.choose m of
               NONE => [Subst.id]
             | SOME (p, rels, m) =>
               let
                  val thetas = loop m
               in
                  case M.find (gm, p) of
                     NONE => thetas
                   | SOME arels =>
                     let
                        fun loop' rels = case S.choose rels of
                           NONE => thetas
                         | SOME (rels, rel) =>
                           let
                              val thetas = loop' rels
                              fun mfn theta =
                                 let
                                    val rel = Rel.apply (rel, theta)
                                    fun ffn (arel, thetas) =
                                       case Unif.Rel.unify (rel, arel) of
                                          NONE => thetas
                                        | SOME theta' =>
                                          Subst.compose (theta, theta') :: thetas
                                 in
                                    S.fold ffn [theta] arels
                                 end
                           in
                              List.concatMap mfn thetas
                           end
                     in
                        loop' rels
                     end
               end
            fun mfn theta = (difference (apply (ants, theta), global), theta)
         in
            List.map mfn (loop m)
         end

      val () = noWarnUnused (isFixed, isEmpty, all, findExn, intersection, choose1)
   end (* structure Ants *)

   type ants = Ants.t

   (* ----------------------------------------------------------------------- *)

   datatype t = S of Id.t * ants * cons
   type printable = t
   type eqable = t
   type fixable = t

   fun new (ants, cons) = S (Id.next (), ants, cons)
   fun id (S (id, _, _)) = id
   fun dest (S (_, ants, cons)) = (ants, cons)
   fun eq (S (_, ants1, cons1),
           S (_, ants2, cons2) : eqable) =
      Cons.eq (cons1, cons2) andalso Ants.eq (ants1, ants2)

   fun fix' ps (S (id, ants, cons)) =
      S (id, Ants.fix' ps ants, Cons.fix' ps cons)

   fun fix (S (id, ants, cons) : fixable) =
      S (id, Ants.fix ants, Cons.fix cons)

   fun unfix (S (id, ants, cons)) =
      S (id, Ants.unfix ants, Cons.unfix cons)

   fun ppGen pid (S (id, ants, cons)) =
      let
         val id = if pid then %[$":", \, Id.pp id] else PP.empty
      in
         PP.hsep [Ants.pp ants, $U.vdash, Cons.pp cons, id]
      end

   fun fill (q as S (_, ants, cons), cons') = case cons of
      Cons.Xi => new (ants, cons')
    | Cons.P _ => q

   val pp = ppGen true
   val ppNoId = ppGen false

   val () = noWarnUnused (eq)

   type parseable = t

   fun parse (P.Seq.T { ants, cons, ... }) =
      let
         val cons = case cons of NONE => Xi | SOME r => P (Rel.parse r)
         val ants = Ants.ofList (map Rel.parse ants)
      in
         new (ants, cons)
      end

   fun ofFocus (ls, r) =
      let
         open Focus.Left
         open Focus.Right
         val cons = case r of
            Var => Xi
          | Pos f => P (F.pos F.label f)
          | NegAtom f => P (F.neg F.label f)
         val ants = Ants.ofList (map (fn
            Neg f => F.neg F.label f
          | PosAtom f => F.pos F.label f) ls)
      in
         new (ants, cons)
      end

   val ofString = parse o P.Seq.ofString

   (* ----------------------------------------------------------------------- *)

   val ants = fst o dest
   val cons = snd o dest

   fun preds t =
      Pred.Set.union (Ants.preds (ants t), Cons.preds (cons t))

   fun atoms t = Atoms.union (Cons.atoms (cons t), Ants.atoms (ants t))
   val atomsfn = atoms

   val params = Atoms.params o atoms

   fun apply (seq, theta) =
      if Subst.isId theta then seq else
      let
         val (ants, cons) = dest seq
      in
         new (Ants.apply (ants, theta), Cons.apply (cons, theta))
      end

   fun rename (seq, theta) =
      let
         val (ants, cons) = dest seq
         val (cons, theta) = Cons.rename (cons, theta)
         val (ants, theta) = Ants.rename (ants, theta)
      in
         (new (ants, cons), theta)
      end

   (* CR: This should be improved. *)
   fun prio {seq, goal} =
      let
         val goalCons = cons goal
         val (ants, cons) = dest seq
         val n =
            Ants.size ants
               + Atoms.size (atoms seq)
      in
         case (cons, goalCons) of
            (Xi, Xi) => 100 - n
          | (Xi, P _) => 100 - n
          | (P _, Xi) => 70 - n
          | (P rel, P goalRel) =>
            if Rel.eq (rel, goalRel)
            then 1000 - n
            else 70 - n
      end

   (* ----------------------------------------------------------------------- *)

   fun subsumes' (seq1, seq2, {global}) =
      let
         (* We will fix all vars and params of seq2, so we must have
            the vars/params be disjoint between seq1/seq2. *)
         val atoms2 = atoms seq2
         val seq1 = fix' atoms2 seq1
         val seq2 = fix seq2
         val (ants1, cons1) = dest seq1
         val (ants2, cons2) = dest seq2
         val global = case global of
            NONE => Ants.empty
          | SOME g => g
         val ants2 = Ants.union (ants2, global)
         (* Note that ants2 is fixed, so ants2 [θ] = ants2 (resp cons2) *)
         val consSub = fn
            (Xi, _) => SOME Subst.id
          | (P r1, P r2) => Unif.Rel.unify (r1, r2)
          | _ => NONE
         (* Invariant.  ants1 [θ] = ants1 *)
         fun antsSub (ants1, ants2, theta) =
            case Ants.choose ants1 of
               NONE => SOME theta
             | SOME (p, rels1, ants1) =>
               case Ants.remove (ants2, p) of
                  NONE => NONE
                | SOME (ants2, rels2) =>
                  let
                     (* Invariant.  rels1 [θ] = rels1 *)
                     fun relsSub (rels1, rels2, theta) =
                        case Rel.Set.choose rels1 of
                           NONE =>
                           antsSub (Ants.apply (ants1, theta), ants2, theta)
                         | SOME (rels1, rel1) =>
                           let
                              fun findFn rel2 =
                                 case Unif.Rel.unify (rel1, rel2) of
                                    NONE => NONE
                                  | SOME theta' =>
                                    let
                                       val rels1 =
                                          Rel.Set.map (fn r =>
                                             Rel.apply (r, theta')) rels1
                                    in
                                       relsSub (rels1, rels2,
                                                Subst.compose (theta, theta'))
                                    end
                           in
                              Rel.Set.findMap findFn rels2
                           end
                  in
                     relsSub (rels1, rels2, theta)
                  end
      in
         if not (Ants.maybeSubsumes (ants1, ants2)) then NONE else
         case consSub (cons1, cons2) of
            NONE => NONE
          | SOME theta => antsSub (Ants.apply (ants1, theta), ants2, theta)
      end

   val subsumes' = fn (args as (_, _, _)) =>
      let
         (* val _ = *)
         (*    Log.trace *)
         (*       (fn () => %[ \\, $"Seq.subsumes': ", *)
         (*                    %[\\, &[Seq.pp s1, Seq.pp s2 ]]]) *)
         val res = subsumes' args
      (* val _ = Log.trace (fn () => case res of *)
      (*    NONE => %[\\, $"Seq.subsumes': NONE"] *)
      (*  | SOME s => %[\\, $"Seq.subsumes': ", Subst.pp s]) *)
      in
         res
      end

   fun subsumes seqs = isSome (subsumes' seqs)

   (* ----------------------------------------------------------------------- *)

   (* Note that we don't need to match against the global parameters here.
      If some ant were to match a global, the substitution would be more
      restricted, and be subsumed by the match that doesn't match the global.
      However, we need to remove globals at the end.
   *)
   fun match {seq, hyp, concl, fresh, global, atoms} =
      let
         (* val _ = Log.trace (fn () => *)
         (*    &[ $"Seq.match:" *)
         (*     , %[\\, &[ %[$"seq: ", ppNoId seq] *)
         (*              , %[$"hyp: ", ppNoId hyp]]]]) *)
         val ats = atomsfn seq
         val _ = asserts (fn () =>
            PSet.all Param.isFixed fresh
            andalso
            Atoms.isEmpty (Atoms.intersection (ats, atoms))
            andalso
            PSet.isEmpty (PSet.intersection (Atoms.params ats, fresh)), "Seq.match failure")
         (* θ is invalid for (seq, hyp, concl) if 'a' and 'b' are fresh and
            1. A {x -> t(a)} ∈ θ and x ∈ vars(hyp)
            2. {a -> b} ∈ θ
            2 can't happen since both fresh vars are fixed, so we
            check 1.
         *)
         fun ok theta =
            let
               val img = Subst.img (Subst.restrict (theta, atomsfn hyp))
               val ps = snd (Atoms.dest img)
            in
               PSet.isEmpty (PSet.intersection (ps, fresh))
            end
         val (antsQ, consQ) = dest seq
         val (antsH, consH) = dest hyp
         val (antsC, consC) = dest concl
         fun antsMatch (ants1, ants2, acc) =
            case Ants.choose ants1 of
               NONE => acc
             | SOME (p, rels1, ants1) =>
               case Ants.remove (ants2, p) of
                  NONE => antsMatch (ants1, ants2, acc)
                | SOME (ants2, rels2) =>
                  let
                     fun relsSub (rels1, rels2, acc) = case Rel.Set.choose rels1 of
                        NONE => antsMatch (ants1, ants2, acc)
                      | SOME (rels1, rel1) =>
                        let
                           val acc = relsSub (rels1, rels2, acc)
                           fun mfn theta =
                              let
                                 val rel1 = Rel.apply (rel1, theta)
                                 val rels2 = Rel.Set.map (fn r =>
                                    Rel.apply (r, theta)) rels2
                                 fun mfn rel2 = case Unif.Rel.unify (rel1, rel2) of
                                    NONE => NONE
                                  | SOME theta' =>
                                    let
                                       val theta' = Subst.compose (theta, theta')
                                    in
                                       if ok theta' then SOME theta' else NONE
                                    end
                              in
                                 if Rel.Set.mem (rels2, rel1) then [theta] else
                                 theta ::
                                 List.mapPartial mfn (Rel.Set.toList rels2)
                              end
                        in
                           List.concatMap mfn acc
                        end
                  in
                     relsSub (rels1, rels2, acc)
                  end
      in
         case Cons.match (consQ, consH) of
            NONE => []
          | SOME {cons, theta = thetaC, filledHole} =>
            let
               val cons = case cons of
                  Xi => consC
                | P _ => cons
               val antsQ = Ants.apply (antsQ, thetaC)
               val antsH = Ants.apply (antsH, thetaC)
               val thetas = antsMatch (antsQ, antsH, [thetaC])
               fun mfn theta1 =
                  let
                     val theta2 = Subst.compose (thetaC, theta1)
                     val antsQ = Ants.apply (antsQ, theta1)
                     val antsH = Ants.apply (antsH, theta1)
                     val antsC = Ants.apply (antsC, theta1)
                     val ants =
                        Ants.union (antsC, Ants.difference (antsQ, antsH))
                     val ants = Ants.difference (ants, global)
                     val cons = Cons.apply (cons, theta2)
                     val concl = new (ants, cons)
                  in
                     if PSet.disjoint (fresh, Atoms.params (atomsfn concl))
                        andalso ok theta2
                     then SOME { concl = concl
                               , theta = theta2
                               , filledHole = filledHole }
                     else NONE
                  end
               val res = List.mapPartial mfn thetas
            in
               (* Log.trace (fn () => *)
               (*    &[ $"Seq.match:" *)
               (*     , %[\\, &[ %[$"seq: ", ppNoId seq] *)
               (*              , %[$"hyp: ", ppNoId hyp] *)
               (*              , %[$"res: ", *)
               (*                  &(map (fn {concl, theta=s, ...} => *)
               (*                            %[pp concl, $" : ", Subst.pp s]) res)]]]]); *)
               res
            end
      end

   fun contract (seq, glob as {global}) =
      let
         val cons = cons seq
         fun mfn (ants, theta) = (new (ants, Cons.apply (cons, theta)), theta)
         val res = map mfn (Ants.contract (ants seq, glob))
      in
         Log.trace (fn () =>
            &[ $"Seq.contract:"
             , %[\\, &[ %[$"seq : ", ppNoId seq]
                      , %[$"glob: ", Ants.pp global]
                      , %[$"res: ",
                          &(map (fn (seq, s) =>
                                    %[pp seq, $" : ", Subst.pp s]) res)]]]]);
         res
      end

   fun invariant (seq, {global, fresh}) =
      let
         val ants = ants seq
      in
         Param.invariant (Atoms.params (atoms seq), {fresh=fresh});
         assert
            (fn () => Ants.isEmpty (Ants.intersection (ants, global)),
             fn () => %[ $"globals in ants: ", Ants.pp ants
                       , $", glob: ", Ants.pp global])
      end

   val () = noWarnUnused (fn _ : printable * parseable * atoms => (ofString, atoms))
end

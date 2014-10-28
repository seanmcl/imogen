
structure Seq :> Seq = struct
   structure U = Unicode
   structure PSet = Param.Set
   structure P = Parse
   structure F = LFormula
   structure C = CFormula

   open General
   open PP.Ops

   type atoms = Atoms.t

   (* ----------------------------------------------------------------------- *)

   structure Cons : sig
      type t
      datatype t' = P of Rel.t | Xi
      include Printable where type printable = t
      include Fixable' where type fixable = t and type atoms = Atoms.t
      val show : t -> t'
      val make : t' -> t
      val isXi : t -> bool
      val eq : t * t -> bool
      (* val unify : t * t -> (t * CSubst.t) option *)
      val atoms : t -> Atoms.t
      val apply : t * Subst.t -> t
      val capply : t * CSubst.t -> t
      val preds : t -> Pred.set
      val rename : t * Subst.t -> t * Subst.t
      val match : t * t -> {cons: t, filledHole:bool, theta:CSubst.t} option
      structure Ops : sig
         datatype ops = datatype t'
      end
   end = struct
      datatype t' =
         P of Rel.t
       | Xi
      datatype t = datatype t'
      type fixable = t
      type atoms = Atoms.t
      type printable = t
      val show = Fun.id

      val make = fn
         Xi => Xi
       | P r => P (CUnif.reduce r)

      fun map f = fn
         Xi => Xi
       | P r => P (CUnif.reduce (f r))

      val eq = fn
         (P p, P p') => Rel.eq (p, p')
       | (Xi, Xi) => true
       | _ => false

      val pp = fn
         (P p) => Rel.pp p
       | Xi => $U.cdot

      fun apply (c, t) = map (fn r => CUnif.reduce (Rel.apply (r, t))) c
      fun capply (c, t) = apply (c, CSubst.subst t)

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
         (Xi, _) => SOME {cons = Xi, theta = CSubst.id, filledHole = false }
       | (P r, Xi) => SOME {cons = P r, theta = CSubst.id, filledHole = true }
       | (P r1, P r2) =>
         case CUnif.unify (r1, r2) of
            NONE => NONE
          | SOME theta => SOME {cons = Xi, theta = theta, filledHole = false }

      structure Ops = struct
         datatype ops = datatype t'
      end

      val () = noWarnUnused (fn _ : printable * atoms * fixable => isXi)
   end
   datatype cons = datatype Cons.t'

   (* ----------------------------------------------------------------------- *)

   structure Ants : sig
      type t
      include Printable where type printable = t
      include Eqable where type eqable = t
      include Fixable where type fixable = t
      val fix' : Atoms.t -> t -> t
      val empty: t
      val ofList: Rel.t list -> t
      (* val mem: Rel.t * t -> bool *)
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
      val capply: t * CSubst.t -> t
      val rename: t * Subst.t -> t * Subst.t
      val choose: t -> (Pred.t * Rel.set * t) option
      val choose1: t -> (Rel.t * t) option
      val maybeSubsumes: t * t -> bool
      val remove: t * Pred.t -> (t * Rel.set) option
      val contract: t * {global:t} -> (t * CSubst.t) list
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

      (* fun mem (r, A (ants, _)) = case M.find (ants, Rel.pred r) of *)
      (*    NONE => false *)
      (*  | SOME rs => Rel.Set.mem (rs, r) *)

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
                     val rel = CUnif.reduce rel
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
            fn ants as A (m, _) =>
               if isEmpty ants then $U.cdot else
               %(PP.punctuate ($", ")
                    (List.map ppRels (M.toList m)))
         end

      fun apply (A (m, _), theta) =
         make (M.map (S.map (fn r => CUnif.reduce (Rel.apply (r, theta)))) m)

      fun capply (ants, t) = apply (ants, CSubst.subst t)

      val rename =
         let
            fun renameRels (rs, theta) =
               let
                  fun foldFn (r, (rs, theta)) =
                     let
                        val (r', theta') = Rel.rename (r, theta)
                     in
                        (S.add (rs, r'), theta')
                     end
               in
                  S.foldr foldFn (S.empty, theta) rs
               end
            fun foldFn (k, ant, (ants, theta)) =
               let
                  val (ant', theta') = renameRels (ant, theta)
               in
                  (M.replace (ants, k, ant'), theta')
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
               NONE => [CSubst.id]
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
                                    val rel = CSubst.applyR (rel, theta)
                                    fun ffn (arel, thetas) =
                                       case CUnif.unify (rel, arel) of
                                          NONE => thetas
                                        | SOME theta' =>
                                          CSubst.compose (theta, theta') :: thetas
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
            fun mfn theta = (difference (capply (ants, theta), global), theta)
         in
            List.map mfn (loop m)
         end

      val () = noWarnUnused (isFixed, isEmpty, all, findExn, intersection, choose1)
   end (* structure Ants *)

   (* ----------------------------------------------------------------------- *)

   structure Pre = struct
      type t = Ants.t * Cons.t
      type printable = t
      type fixable = t
      type eqable = t
      type atoms = Atoms.t

      fun eq ((ants1, cons1), (ants2, cons2)) =
         Cons.eq (cons1, cons2) andalso Ants.eq (ants1, ants2)
      fun atoms (ants, cons) = Atoms.union (Cons.atoms cons, Ants.atoms ants)

      fun fix (ants, cons) = (Ants.fix ants, Cons.fix cons)
      fun unfix (ants, cons) = (Ants.unfix ants, Cons.unfix cons)
      fun fix' ps (ants, cons) = (Ants.fix' ps ants, Cons.fix' ps cons)
      fun apply ((ants, cons), t) = (Ants.apply (ants, t), Cons.apply (cons, t))

      fun preds (ants, cons) =
         Pred.Set.union (Ants.preds ants, Cons.preds cons)

      fun pp (ants, cons) = PP.hsep [Ants.pp ants, $U.vdash, Cons.pp cons]

      fun rename ((ants, cons), theta) =
         let
            val (cons, theta) = Cons.rename (cons, theta)
            val (ants, theta) = Ants.rename (ants, theta)
         in
            ((ants, cons), theta)
         end

      fun fill (q as (ants, cons), cons') = case Cons.show cons of
         Cons.Xi => (ants, cons')
       | Cons.P _ => q

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
            (ants, Cons.make cons)
         end

      fun contract ((ants, cons), glob) =
         let
            fun mfn (ants, theta) = ((ants, Cons.capply (cons, theta)), theta)
         in
            map mfn (Ants.contract (ants, glob))
         end

      val () = noWarnUnused (fn _ : printable * fixable * eqable * atoms => (fix))
   end

   (* ----------------------------------------------------------------------- *)

   datatype t = S of
      { id : Id.t
      , constr : CFormula.t
      , ants : Ants.t
      , cons : Cons.t }
   type printable = t
   type eqable = t
   type fixable = t

   fun new (constr, ants, cons) =
      S { id = Id.next ()
        , constr = C.simplify constr
        , ants = ants
        , cons = cons }

   fun id (S {id, ...}) = id
   fun cons (S {cons, ...}) = cons
   fun ants (S {ants, ...}) = ants
   fun constr (S {constr, ...}) = constr
   fun dest (S {constr, ants, cons, ...}) = (constr, ants, cons)
   fun pre (S {ants, cons, ...}) = (ants, cons)
   fun eq ( S {ants = ants1, cons = cons1, constr = constr1, ...}
          , S {ants = ants2, cons = cons2, constr = constr2, ...} ) =
      Cons.eq (cons1, cons2) andalso Ants.eq (ants1, ants2)
      andalso C.eq (constr1, constr2)

   fun fix' ps (S {id, ants, cons, constr}) =
      S { id = id, ants = Ants.fix' ps ants, cons = Cons.fix' ps cons
        , constr = constr }

   fun fix (S {id, ants, cons, constr}) =
      S { id = id, ants = Ants.fix ants, cons = Cons.fix cons, constr = constr }

   fun unfix (S {id, ants, cons, constr}) =
      S { id = id, ants = Ants.unfix ants, cons = Cons.unfix cons
        , constr = constr }

   fun ppGen pid (S { id, ants, cons, constr }) =
      let
         val id = if pid then %[$":", \, Id.pp id] else PP.empty
      in
         PP.hsep [C.pp constr, $"|", Ants.pp ants, $U.vdash, Cons.pp cons, id]
      end

   val pp = ppGen true
   val ppNoId = ppGen false

   type parseable = t

   fun parse (P.Seq.T { ants, cons, ... }) =
      let
         val cons = case cons of NONE => Xi | SOME r => P (Rel.parse r)
         val cons = Cons.make cons
         val ants = Ants.ofList (map Rel.parse ants)
      in
         new (C.Top, ants, cons)
      end

   fun ofFocus (cst, pre) =
      let
         val (ants, cons) = Pre.ofFocus pre
      in
         new (cst, ants, cons)
      end

   val ofString = parse o P.Seq.ofString

   val preds = Pre.preds o pre

   val atoms = Pre.atoms o pre

   fun rename (seq, theta) =
      let
         val (cst, ants, cons) = dest seq
         val ((ants, cons), theta) = Pre.rename ((ants, cons), theta)
         (* All fv of cst are among the fv of cons, ants. *)
         val cst = C.apply (cst, theta)
      in
         (new (cst, ants, cons), theta)
      end

   (* CR: This should be improved. *)
   fun prio {seq, goal} =
      let
         val goalCons = cons goal
         val (_, ants, cons) = dest seq
         val n =
            Ants.size ants
               + Atoms.size (atoms seq)
      in
         case (Cons.show cons, Cons.show goalCons) of
            (Xi, Xi) => 100 - n
          | (Xi, P _) => 100 - n
          | (P _, Xi) => 70 - n
          | (P rel, P goalRel) =>
            if Rel.eq (rel, goalRel)
            then 1000 - n
            else 70 - n
      end

   (* ----------------------------------------------------------------------- *)

   fun subsumes {subsumer, subsumed, global, entails} =
      let
         (* val _ = Log.trace (fn () => *)
         (*    &[ $"Seq.subsumes:" *)
         (*     , %[\\, &[ %[$"subsumer: ", pp subsumer] *)
         (*              , %[$"subsumed: ", pp subsumed]]]]) *)
         (* We will fix all vars and params of seq2, so we must have
            the vars/params be disjoint between seq1/seq2. *)
         val atoms2 = atoms subsumed
         val seq1 = fix' atoms2 subsumer
         val seq2 = fix subsumed
         val (cst1, ants1, cons1) = dest seq1
         val (cst2, ants2, cons2) = dest seq2
         (* Note that ants2 is fixed, so ants2 [θ] = ants2 (resp cons2) *)
         val consSub = fn
            (Xi, _) => SOME CSubst.id
          | (P r1, P r2) => CUnif.unify (r1, r2)
          | _ => NONE
         (* Invariant.  ants1 [θ] = ants1 *)
         fun antsSub (ants1, ants2, theta) =
            case Ants.choose ants1 of
               NONE =>
               let
                  val cst1 = CSubst.constr (theta, cst1)
               in
                  if entails { entailer = cst2
                             , entailed = cst1
                             , global = global }
                  then SOME (CSubst.subst theta) else NONE
               end
             | SOME (p, rels1, ants1) =>
               case Ants.remove (ants2, p) of
                  NONE => NONE
                | SOME (ants2, rels2) =>
                  let
                     (* Invariant.  rels1 [θ] = rels1 *)
                     fun relsSub (rels1, rels2, theta) =
                        case Rel.Set.choose rels1 of
                           NONE =>
                           antsSub (Ants.capply (ants1, theta), ants2, theta)
                         | SOME (rels1, rel1) =>
                           let
                              fun findFn rel2 =
                                 case CUnif.unify (rel1, rel2) of
                                    NONE => NONE
                                  | SOME theta' =>
                                    let
                                       val rels1 =
                                          Rel.Set.map (fn r =>
                                             CSubst.applyR (r, theta')) rels1
                                    in
                                       relsSub (rels1, rels2,
                                                CSubst.compose (theta, theta'))
                                    end
                           in
                              Rel.Set.findMap findFn rels2
                           end
                  in
                     relsSub (rels1, rels2, theta)
                  end
         val res =
            if not (Ants.maybeSubsumes (ants1, ants2)) then NONE else
            case consSub (cons1, cons2) of
               NONE => NONE
             | SOME theta => antsSub (Ants.capply (ants1, theta), ants2, theta)
      in
         Log.trace (fn () =>
            &[ $"Seq.subsumes:"
             , %[\\, &[ %[$"subsumer: ", pp subsumer]
                      , %[$"subsumed: ", pp subsumed]
                      , %[$"res     : ", PP.option Subst.pp res]]]]);
         res
      end

   (* ----------------------------------------------------------------------- *)

   (* Note that we don't need to match against the global parameters here.
      If some ant were to match a global, the substitution would be more
      restricted, and be subsumed by the match that doesn't match the global.
      However, we need to remove globals at the end.
   *)
   fun match {seq, hyp, concl, fresh, global, atoms=ats} =
      let
         (* val _ = Log.trace (fn () => *)
         (*    &[ $"Seq.match:" *)
         (*     , %[\\, &[ %[$"seq: ", ppNoId seq] *)
         (*              , %[$"hyp: ", Pre.pp hyp] *)
         (*              , %[$"fsh: ", PSet.pp fresh] *)
         (*              , %[$"ats: ", Atoms.pp ats]]]]) *)
         val ats' = atoms seq
         val _ = asserts (fn () =>
            let
               val b1 = PSet.all Param.isFixed fresh
               val b2 = Atoms.isEmpty (Atoms.intersection (ats, ats'))
               val fresh' = PSet.map Param.unfix fresh
               val b3 = PSet.isEmpty
                           (PSet.intersection (Atoms.params ats', fresh'))
            in
               b1 andalso b2 andalso b3
            end, "Seq.match failure")
         (* θ is invalid for (seq, hyp, concl) if 'a' and 'b' are fresh and
            1. A {x -> t(a)} ∈ θ and x ∈ vars(hyp)
            2. {a -> b} ∈ θ
            2 can't happen since both fresh vars are fixed, so we
            check 1.
         *)
         fun ok theta =
            let
               val img = CSubst.img (CSubst.restrict (theta, Pre.atoms hyp))
               val ps = snd (Atoms.dest img)
               val res = PSet.isEmpty (PSet.intersection (ps, fresh))
            in
               (* Log.trace (fn () => *)
               (*    &[ $"ok:" *)
               (*     , %[\\, &[ %[$"theta: ", CSubst.pp theta] *)
               (*              , %[$"res  : ", PP.bool res]]]]); *)
               res
            end
         val (_, antsQ, consQ) = dest seq
         val (antsH, consH) = hyp
         val (antsC, consC) = concl
         fun antsMatch (ants1, ants2, acc) = case Ants.choose ants1 of
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
                              val rel1 = CSubst.applyR (rel1, theta)
                              val rels2 = Rel.Set.map (fn r =>
                                 CSubst.applyR (r, theta)) rels2
                              fun mfn rel2 = case CUnif.unify (rel1, rel2) of
                                 NONE => NONE
                               | SOME theta' =>
                                 let
                                    val theta' = CSubst.compose (theta, theta')
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
               val antsQ = Ants.capply (antsQ, thetaC)
               val antsH = Ants.capply (antsH, thetaC)
               val thetas = antsMatch (antsQ, antsH, [thetaC])
               fun mfn theta1 =
                  let
                     val theta2 = CSubst.compose (thetaC, theta1)
                     val antsQ = Ants.capply (antsQ, theta1)
                     val antsH = Ants.capply (antsH, theta1)
                     val antsC = Ants.capply (antsC, theta1)
                     val ants =
                        Ants.union (antsC, Ants.difference (antsQ, antsH))
                     (* val _ = PP.ppl (&[ %[$"antsQ: ", Ants.pp antsQ] *)
                     (*                  , %[$"antsH: ", Ants.pp antsH] *)
                     (*                  , %[$"antsC: ", Ants.pp antsC] *)
                     (*                  , %[$"ants: ", Ants.pp ants]]) *)
                     val ants = Ants.difference (ants, global)
                     val cons = Cons.capply (cons, theta2)
                     val concl = (ants, cons)
                  (* val _ = PP.ppl (%[$"concl: ", Pre.pp concl]) *)
                  in
                     if PSet.disjoint (fresh, Atoms.params (Pre.atoms concl))
                        andalso ok theta2
                     then SOME { concl = concl
                               , csubst = theta2
                               , filledHole = filledHole }
                     else NONE
                  end
               val res = List.mapPartial mfn thetas
            in
               Log.trace (fn () =>
                  &[ $"Seq.match:"
                   , %[\\, &[ %[$"seq: ", ppNoId seq]
                            , %[$"hyp: ", Pre.pp hyp]
                            , %[$"res: ",
                                &(map (fn {concl, csubst=s, ...} =>
                                          %[Pre.pp concl, $" : ", CSubst.pp s]) res)]]]]);
               res
            end
      end

   fun contract (seq, glob as {global}) =
      let
         val () = noWarnUnused (global)
         fun mfn ((ants, cons), theta) =
            let
               val constr = CSubst.constr (theta, constr seq)
            in
               (new (constr, ants, Cons.capply (cons, theta)), theta)
            end
         val res = map mfn (Pre.contract (pre seq, glob))
      in
         (* Log.trace (fn () => *)
         (*    if length res > 1 then *)
         (*       &[ $"Seq.contract:" *)
         (*        , %[\\, &[ %[$"seq : ", ppNoId seq] *)
         (*                 , %[$"glob: ", Ants.pp global] *)
         (*                 , %[$"res: ", *)
         (*                     &(map (fn (seq, s) => *)
         (*                               %[pp seq, $" : ", CSubst.pp s]) res)]]]] *)
         (*    else PP.empty); *)
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

   val () = noWarnUnused
               (fn _ : printable * parseable * eqable * fixable * atoms =>
                   (ofString, atoms, eq, fix, unfix, invariant))
end

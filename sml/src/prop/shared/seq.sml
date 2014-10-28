
structure Seq :> Seq = struct
   open General
   open PP.Ops

   structure U = Unicode
   structure F = LFormula

   (* ----------------------------------------------------------------------- *)

   structure Cons = struct
      datatype t =
         P of Pred.t
       | Xi

      val eq = fn
         (P p, P p') => Pred.eq (p, p')
       | (Xi, Xi) => true
       | _ => false

      val pp = fn
         P l => Pred.pp l
       | Xi => $U.Xi

      val isXi = fn
         Xi => true
       | _ => false

      structure Ops = struct
         datatype ops = datatype t
      end
   end
   datatype cons = datatype Cons.t

   (* ----------------------------------------------------------------------- *)

   structure Ants = struct
      open Pred.Set
      type printable = t
      val fold = foldr
      val eq = eq
      val mem = mem
      val () = noWarnUnused (empty:printable)
   end

   type ants = Ants.t

   (* ----------------------------------------------------------------------- *)

   datatype t = S of Id.t * ants * cons
   type printable = t
   type eqable = t

   fun preds (S (_, ants, cons)) = case cons of
      Xi => ants
    | P p => Pred.Set.add (ants, p)

   fun new (ants,cons) = S (Id.next (), ants, cons)
   fun ants (S (_, ants, _)) = ants
   fun cons (S (_, _, cons)) = cons
   fun id (S (id, _, _)) = id

   fun ofFocus (ls, r) =
      let
         open Focus.Left
         open Focus.Right
         val cons = case r of
            Var => Xi
          | Pos f => P (F.pos F.pred f)
          | NegAtom f => P (F.neg F.pred f)
         val ants = Ants.ofList (map (fn
            Neg f => F.neg F.pred f
          | PosAtom f => F.pos F.pred f) ls)
      in
         new (ants, cons)
      end

   fun eq (S (_, ants1, cons1), S (_, ants2, cons2):eqable) =
      Cons.eq (cons1, cons2) andalso Ants.eq (ants1, ants2)

   fun combine (S (_, ants1, cons1), S (_, ants2, cons2)) =
      let
         val ants = Ants.union (ants1, ants2)
      in
         case (cons1, cons2) of
            (P p1, P p2) =>
            if Pred.eq (p1, p2) then SOME (new (ants, cons1)) else NONE
          | (Xi, _) => SOME (new (ants, cons2))
          | (_, Xi) => SOME (new (ants, cons1))
      end

   (* One sequent subsumes another if the consequents are
      compatable and the antecedents of the first are a subset
      of the antecedents of the second.  The consequents are
      compatable if the first is a Var, or if they have identical
      labels. *)
   val subsumes =
      let
         fun subsumesC (P l, P l') = Pred.eq (l, l')
           | subsumesC (P _, Xi) = false
           | subsumesC (Xi, _) = true
      in
         fn (S (_, ants, cons), S (_, ants', cons')) =>
            (* A simple filter *)
            Ants.size ants <= Ants.size ants' andalso
            subsumesC (cons, cons') andalso
            Ants.subset (ants, ants')
      end

   (* The size of a variable rhs should be larger than a label because
      it conveys more information. *)
   fun prio { seq as S (_, ants, cons), goal as S (_, _, goalCons) } =
      if subsumes (seq, goal) then 100000000
      else let
         val n = Ants.size ants
      in
         case (cons, goalCons) of
            (Xi, Xi) => 30 - n
          | (Xi, P _) => 30 - n
          | (P _, Xi) => 20 - n
          | (P rel, P goalPred) =>
            if Pred.eq (rel, goalPred)
            then 10000 - n
            else 200 - n
      end

   val (pp, ppNoId) =
      let
         fun ppGen pid (S (id, ants, cons) : printable) =
            let
               val id = if pid then %[$":", \, Id.pp id] else PP.empty
               val seq = PP.sep (PP.punctuate PP.comma (map Pred.pp (Ants.toList ants))
                                    @ [$U.vdash, Cons.pp cons, id])
            in
               seq
            end
      in
         (ppGen true, ppGen false)
      end

end

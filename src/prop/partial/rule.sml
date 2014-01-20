
structure Rule :> Rule = struct
   structure C = Focus
   structure Ants = Seq.Ants
   structure Id = RuleId
   open General
   open PP.Ops
   open Seq.Cons.Ops

   structure Rule = struct
      datatype t = R of Id.t * Seq.t list * Seq.t
      type printable = t

      fun pp (R (id, hyps, concl) : printable) =
         &[ &(map Seq.pp hyps)
          , %[$"---------------------------",Id.pp id]
          , Seq.pp concl]
   end
   open Rule

   fun hyps (R (_, hyps, _)) = hyps
   fun firstHyp r = case hyps r of
      [] => raise Impossible
    | h :: _ => h
   fun concl (R (_, _, concl)) = concl
   fun id (R (id, _, _)) = id
   fun create (hyps, concl) = R (Id.next(), hyps, concl)

   fun ofFocus (C.Rule.T { hyps, concl, ...}) =
      let
         val hyps = map Seq.ofFocus hyps
         val concl = Seq.ofFocus concl
      in
         create (hyps, concl)
      end

   (* A note about matching Seq.Var.  If we have a sequent seq = G |- X, where X
      is Seq.Var, then for a successful match(seq, ruleSeq) we must have
      that G is a subset of the antecedents of ruleSeq.  If not, we have
      the situation:

        G |- X  matching

        G1, D1 |- Y1 ... GN, DN |- YN
      -------------------------------
           G1, ..., GN |- Y

      But if the intersection of G and D1 is empty, then G |- X subsumes
      G1, ..., GN |- Y.
      We call such a match 'strict'. *)

   structure Match = struct
      type rule = Rule.t
      datatype t = NewRules of rule list
                 | NewSeqs of Seq.t list
                 | NoMatch

      val pp = fn
         NewSeqs qs => &(map Seq.pp qs)
       | NewRules rs => &(map Rule.pp rs)
       | NoMatch => $"NoMatch"
   end
   structure M = Match

   fun match (R (_, hyps, concl), seq) = case hyps of
      [] => raise Impossible
    | hyp :: hyps =>
      let
         val r_ants = Seq.ants hyp
         val r_cons = Seq.cons hyp
         val q_ants = Seq.ants seq
         val q_cons = Seq.cons seq
         val c_ants = Seq.ants concl
         val c_cons = Seq.cons concl

         (* matchR (seq, ruleSeq) --> SOME (match, changed, strict)
            If the match has changed the rule, we need to update it.
            Strict means one of the antecedents must match. *)
         val matchR : Seq.cons * Seq.cons -> (Seq.cons * bool * bool) option =
            fn (p as P l1, P l2) =>
               if Pred.eq (l1, l2) then SOME (p, false, false) else NONE
             | (Xi, r) => SOME (r, false, true)
             | (r as P _, Xi) => SOME (r, true, false)

         fun updateHyp cons hyp = case Seq.cons hyp of
            P _ => hyp
          | Xi => Seq.new(Seq.ants hyp, cons)

         fun updateCons cons cons' = case cons' of
            P _ => cons'
          | Xi => cons
      in
         case matchR (q_cons, r_cons) of
            NONE => M.NoMatch
          | SOME (cons, changed, strict) =>
            (* See note above for strictness constraint *)
            if strict andalso Ants.isEmpty(Ants.intersection(q_ants, r_ants))
            then M.NoMatch
            else
               let
                  val ants = Ants.union(c_ants, Ants.difference(q_ants, r_ants))
                  val cons = if changed then updateCons cons c_cons else c_cons
                  val hyps' = if changed then map (updateHyp cons) hyps else hyps
                  val concl' = Seq.new(ants, cons)
               in
                  case hyps of
                     [] => M.NewSeqs [concl']
                   | _ =>
                     (* If any hypothesis subsumes the conclusion, don't add it *)
                     if List.exists (fn x => Seq.subsumes(x, concl')) hyps'
                     then M.NoMatch
                     else M.NewRules [create (hyps', concl')]
               end
      end

   (* FIXME: improve this *)
   fun prio (R (_, hyps, concl), {goal}) =
      Seq.prio {seq = concl, goal = goal} - 5 * length hyps

end

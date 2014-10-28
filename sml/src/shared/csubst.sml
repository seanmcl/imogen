
structure CSubst :> CSubst = struct
   structure C = CFormula
   structure S = Subst

   open General
   open PP.Ops

   type t = Subst.t * Term.eqs
   type printable = t
   val make = Fun.id
   val dest = Fun.id
   val subst = fst
   val eqs = snd
   val id = (Subst.id, [])

   val isId = fn
      (s, []) => Subst.isId s
    | _ => false

   (* (θ1, ψ1) o (θ2, ψ2) = (θ1 · θ2, ψ1 [θ2] @ ψ2) *)
   fun compose ((s1, ts1), (s2, ts2)) =
      let
         val s = Subst.compose (s1, s2)
         val ts1 = map (fn (t1, t2) =>
            (Subst.apply (t1, s2), Subst.apply (t2, s2))) ts1
      in
         (s, ts1 @ ts2)
      end

   fun apply (t, (s, _)) = S.apply (t, s)
   fun applyR (t, (s, _)) = Rel.apply (t, s)

   fun lift f (s, _) = f s
   fun img s = lift S.img s
   fun dom s = lift S.dom s
   fun restrict ((s, c), ats) = (S.restrict (s, ats), c)

   fun pp (s, ts) =
      PP.pair
         (Subst.pp s,
          PP.list ((map (fn (t1, t2) => %[Term.pp t1, $" = ", Term.pp t2]))
                   ts))

   fun constr ((s, ts), t) =
      let
         (* Unfix the substitution because we never fix the atoms in the
            constraint. *)
         val s = Subst.unfix s
         val t = C.apply (t, s)
      in
         List.foldr
            (fn ((x, y), t) =>
                C.And (C.Atom (Rel.make (Pred.ueq, [x,y])), t))
            t ts
      end

   fun plus ((s1, ts1), (s2, ts2)) = case Unif.plus (s1, s2) of
      NONE => NONE
    | SOME s => SOME (s, ts1 @ ts2)

   val rec plusl = fn
      [] => SOME id
    | [s] => SOME s
    | s1 :: s2 :: ss =>
      case plus (s1, s2) of
         NONE => NONE
       | SOME s => plusl (s :: ss)

end


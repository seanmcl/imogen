
structure World :> World = struct
   structure C = CFormula
   structure T = Term
   structure S = Term.Set

   open General
   open PP.Ops

   structure W = struct
      open S

      val star = Func.Linear.times

      fun timesT (t1, t2) = T.Fn (star, [t1, t2])

      (* val times = S.union *)

      val eps = S.empty

      val epsT = T.Fn (Func.Linear.eps, [])

      (* fun toList t = case t of *)
      (*    T.Fn (f, [t1, t2]) => *)
      (*    if Func.eq (star, f) then toList t1 @ toList t2 else [t] *)
      (*  | t => [t] *)
      (* fun ofList l = foldr times eps l *)

      fun toTerm t =
         if S.isEmpty t then epsT else
         Term.Set.foldr1 timesT t

      val isEps = S.isEmpty

      fun apply (t, s) = S.map (fn t => Subst.apply (t, s)) t

      fun numVars s = S.fold (fn (t, n) => if T.isUnfixedVar t then n + 1 else n) 0 s

      fun ofTerm t = case t of
         T.Var _ => S.singleton t
       | T.Param _ => S.singleton t
       | T.Fn (f, []) =>
         if Func.eq (f, Func.Linear.eps) then S.empty else S.singleton t
       | T.Fn (f, [w1, w2]) =>
         if Func.eq (f, Func.Linear.times) then S.union (ofTerm w1, ofTerm w2)
         else failwith ("bad binary constant in world: " ^ Func.toString f)
       | T.Fn (f, _) => failwith ("bad function symbol: " ^ Func.toString f)

      val isConst = fn
         T.Param _ => true
       | T.Fn (_, []) => true
       | _ => false

      fun getVar t =
         if S.size t <> 1 then NONE else
         case S.choose' t of
            T.Var x => if Var.isFixed x then NONE else SOME x
          | _ => NONE

      fun hasConst t = S.exists isConst t

      fun reduce s = toTerm (ofTerm s)
   end
   open W

   structure Eqs : sig
      type w = t
      type t = (W.t * W.t) list
      val ofForm : C.t -> t
      val toForm : t -> C.t
      val toEqs : t -> Term.eqs
      val ofEqs : Term.eqs -> t
      val apply : t * Subst.t -> t
   end = struct
      type w = W.t
      type t = (W.t * W.t) list

      fun toEqs l = List.map (fn (t1, t2) => (W.toTerm t1, W.toTerm t2)) l
      fun ofEqs l = List.map (fn (t1, t2) => (W.ofTerm t1, W.ofTerm t2)) l

      fun apply (eqs, s) =
         List.map (fn (t1, t2) => (W.apply (t1, s), W.apply (t2, s))) eqs

      fun ofForm f =
         let
            val mfn = fn
               C.Top => NONE
             | C.Atom r =>
               let in
                  case Rel.dest r of
                     (eq, [w1, w2]) =>
                     if Pred.eq (eq, Pred.ueq) then
                        SOME (W.ofTerm w1, W.ofTerm w2)
                     else failwith "Eqs.ofForm: bad pred"
                   | _ => failwith "Eqs.ofForm: bad pred"
               end
             | _ => failwith' (%[$"Eqs.ofForm: ", C.pp f])
         in
            List.mapPartial mfn (C.conjuncts f)
         end

      fun toForm eqs =
         let
            fun mfn (t1, t2) =
               C.Atom (Rel.make (Pred.ueq, [W.toTerm t1, W.toTerm t2]))
         in
            C.listConj (List.map mfn eqs)
         end
   end
   val () = noWarnUnused (fn _ : Eqs.w => ())

end

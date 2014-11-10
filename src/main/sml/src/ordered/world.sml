
structure World :> World = struct
   structure C = CFormula
   structure T = Term
   structure S = Dlist
   structure LW = Linear.World

   open General
   open PP.Ops

   structure W = struct
      type t = Term.t S.t

      val star = Func.Ordered.times
      val epsT = T.Fn (Func.Ordered.eps, [])
      val eps = S.empty
      val map = S.map

      val isEps = S.isEmpty
      val ofDlist = Fun.id
      val toDlist = Fun.id

      fun inj w = S.singleton w

      fun timesT (t1, t2) = T.Fn (star, [t1, t2])
      val times = S.append

      fun toTerm t =
         if S.isEmpty t then epsT else
         S.foldr1 timesT t

      fun ofTerm t = case t of
         T.Var _ => S.singleton t
       | T.Param _ => S.singleton t
       | T.Fn (f, []) =>
         if Func.eq (f, Func.Ordered.eps) then S.empty else S.singleton t
       | T.Fn (f, [x]) =>
         if Func.eq (f, Func.Ordered.inj) then S.singleton t
         else failwith ("bad unary function: " ^ Func.toString f)
       | T.Fn (f, [w1, w2]) =>
         if Func.eq (f, Func.Ordered.times) then S.append (ofTerm w1, ofTerm w2)
         else failwith ("bad binary constant in world: " ^ Func.toString f)
       | T.Fn (f, _) => failwith ("bad function symbol: " ^ Func.toString f)

      fun apply (t, s) = S.map (fn t => Subst.apply (t, s)) t

      fun ofLinear w = T.Fn (Func.Ordered.inj, [w])

      fun toList t = case t of
         T.Fn (f, [t1, t2]) =>
         if Func.eq (star, f) then toList t1 @ toList t2 else [t]
       | t => [t]

      fun ofList l = foldr times eps l
   end
   open W

   structure Eqs : sig
      type w = W.t
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
             | C.imogen.Atom r =>
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
               C.imogen.Atom (Rel.make (Pred.ueq, [W.toTerm t1, W.toTerm t2]))
         in
            C.listConj (List.map mfn eqs)
         end
   end

   val reduce = toTerm o ofTerm

   val () = noWarnUnused ()
end

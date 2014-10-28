
structure CUnif :> CUnif = struct
   structure T = Term
   structure S = CSubst
   structure B = Sort.Base

   open General
   open PP.Ops

   fun unify1 (t12 as (t1, t2)) =
      let
         fun var (x, t) = SOME (S.make (Subst.ofList [Left (x, t)], []))
         fun param (a, b) = SOME (S.make (Subst.ofList [Right (a, b)], []))
         fun i (t1, t2) = case Unif.unify1 (t1, t2) of
            NONE => NONE
          | SOME s => SOME (S.make (s, []))
      in
         case (t1, t2) of
            (T.Var x, T.Var y) =>
            if Var.eq (x, y) then SOME S.id
            else if not (Var.isFixed x) then var (x, t2)
            else if not (Var.isFixed y) then var (y, t1)
            else NONE
          | (T.Var x, _) =>
            if not (Var.isFixed x) then var (x, t2)
            else NONE
          | (_, T.Var _) => unify1 (t2, t1)
          | (T.Param x, T.Param y) =>
            if Param.eq (x, y) then SOME S.id
            else if not (Param.isFixed x) then param (x, y)
            else if not (Param.isFixed y) then param (y, x)
            else NONE
          | (T.Param _, T.Fn _) => unify1 (t2, t1)
          | (T.Fn (f, _), _) =>
            case Signat.func Signat.find f of
               SOME (_, s) =>
               let in
                  case s of
                     B.I => i t12
                   | B.MWorld => i t12
                   | B.LWorld => Linear.unify t12
                   | B.LHead => i t12
                   | B.OWorld => Ordered.unify t12
                   | B.OHead => i t12
               end
             (* This must be a frozen parameter. *)
             | NONE => i t12
      end

   fun unify (r1, r2) =
      let
         val (p1, ts1) = Rel.dest r1
         val (p2, ts2) = Rel.dest r2
         fun loop s = fn
            [] => SOME s
          | t12 :: ts =>
            case unify1 t12 of
               NONE => NONE
             | SOME s' =>
               case S.plus (s, s') of
                  NONE => NONE
                | SOME s => loop s ts
         val res =
            if not (Pred.eq (p1, p2)) then NONE else
            if not (length ts1 = length ts2) then failwith "unify: unequal lengths" else
            loop CSubst.id (List.zip (ts1, ts2))
      in
         (* Log.trace (fn () => *)
         (*    &[ $"CUnif.unify:" *)
         (*     , %[\\, &[ %[$"r1: ", Rel.pp r1] *)
         (*              , %[$"r2: ", Rel.pp r2] *)
         (*              , %[$"res: ", PP.option CSubst.pp res]]]]); *)
         res
      end

   fun reduceTerm t = case t of
      T.Var _ => t
    | T.Param _ => t
    | T.Fn (f, _) =>
      case Signat.func Signat.find f of
         SOME (_, s) =>
         let in
            case s of
               B.LWorld => Linear.reduce t
             | _ => t
         end
       (* This must be a frozen parameter. *)
       | NONE => t

   fun reduce r =
      let
         val (p, ts) = Rel.dest r
      in
         Rel.make (p, map reduceTerm ts)
      end

   fun eq t12 = case unify1 t12 of
      NONE => false
    | SOME s => CSubst.isId s

   fun general (r1, r2) =
      let
         val atoms = Rel.atoms r2
         val r1 = Rel.fix' atoms r1
      in
         case unify (r1, Rel.fix r2) of
            NONE => false
          | SOME s => null (S.eqs s)
      end

   fun instance (t1, t2) = general (t2, t1)

   fun unifiable t12 = isSome (unify t12)

   fun variant (r1, r2) = case unify (r1, r2) of
      NONE => false
    | SOME s =>
      null (S.eqs s) andalso
      Subst.all (fn Left (_, T.Var _) => true
                  | Right (_, a) => not (Param.isFixed a)
                  | _ => false) (CSubst.subst s)

end

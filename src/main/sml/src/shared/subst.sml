
(* A simple substitution representation.  This is quite inefficient
   because we keep the substitutions in idempotent form, and thus must
   solve them whenever they are extended.  A better
   implementation might store a flag whether the substitution was
   solved or not, and then solve it only before applying it.  This could
   be checked at compile time using phantom types.  *)

structure Subst :> Subst = struct
   open General
   open PP.Ops

   structure U = Unicode
   structure P = Parse
   structure VMap = Var.Map
   structure PMap = Param.Map
   structure VSet = Var.Set
   structure PSet = Param.Set
   structure T = Term

   type bind = (Var.t * Term.t, Param.t * Param.t) Either.t

   type t = Term.t Var.map * Param.t Param.map

   type printable = t
   type eqable = t
   type parseable = t
   type fixable = t

   fun remove (t, x) =
      let
         fun remove (env as (venv, penv), x) =
            case VMap.remove (venv, x) of
               NONE => env
             | SOME (m, _) => (m, penv)
         fun removeP (env as (venv, penv), x) =
            case PMap.remove (penv, x) of
               NONE => env
             | SOME (m, _) => (venv, m)
      in
         case x of
            Left x => remove (t, x)
          | Right x => removeP (t, x)
      end

   fun pp ((venv, penv) : printable) =
      let
         val terms = VMap.toListi venv
         val params = PMap.toListi penv
         val binds = map Left terms @ map Right params
         val binds =
            List.sort
               (fn (Left (x, _), Left (y, _)) => Var.compare (x, y)
                 | (Left (x, _), Right (y, _)) => Int.compare (Var.num x, Param.num y)
                 | (Right (x, _), Left (y, _)) => Int.compare (Param.num x, Var.num y)
                 | (Right (x, _), Right (y, _)) => Param.compare (x, y)) binds
         val eqs =
            List.map (fn Left (x, t) => %[Var.pp x, \, $U.mapsto, \, T.pp t]
                       | Right (a, b) => %[Param.pp a, \, $U.mapsto, \, Param.pp b])
               binds
      in
         PP.brace (%(PP.commas eqs))
      end

   fun all f (m1, m2) =
      VMap.alli (f o Left) m1 andalso
      PMap.alli (f o Right) m2

   fun fold f x (m1, m2) =
      PMap.foldri
         (fn (a, b, acc) => f (Right (a, b), acc))
         (VMap.foldri (fn (x, t, acc) => f (Left (x, t), acc)) x m1)
         m2

   fun toBindList t : bind list = fold op:: [] t
   fun toTermList t =
      fold
         (fn (Left (x, t), acc) => (T.Var x, t) :: acc
           | (Right (a, b), acc) => (T.Param a, T.Param b) :: acc)
         [] t

   val id = (VMap.empty, PMap.empty)

   fun eq ((v1, p1), (v2, p2) : eqable) =
      VMap.eqBy T.eq (v1, v2) andalso
      PMap.eqBy Param.eq (p1, p2)

   fun applyP (a, (_, penv)) = Option.value (PMap.find (penv, a)) a

   fun applyV (x, (venv, _)) = Option.value (VMap.find (venv, x)) (T.Var x)

   fun apply (tm, env) = case tm of
      T.Var x => applyV (x, env)
    | T.Param a => T.Param (applyP (a, env))
    | T.Fn (f, args) => T.Fn (f, List.map (fn t => apply (t, env)) args)

   fun applyS ((xs, ps), theta) =
      ( VMap.map (fn t => apply (t, theta)) xs
      , PMap.map (fn a => applyP (a, theta)) ps)

   fun applyB (bind, theta) = case bind of
      Left (x, t) => Left (x, apply (t, theta))
    | Right (a, b) => Right (a, applyP (b, theta))

   fun solve (env as (venv, penv)) =
      (* occurs check *)
      if VMap.existsi
            (fn (_, T.Var _) => false
              | (x, t) => VSet.mem (T.vars t, x))
            venv
      then NONE else
      let
         fun vins (m, x, t) =
            VMap.insert (m, x, t)
            handle _ => failwith "vins"
         fun pins (m, x, t) =
            PMap.insert (m, x, t)
            handle _ => failwith "pins"
         fun vfoldFn (x, t, m) = case t of
            T.Var x' =>
            if Var.eq (x, x')
            then m
            else vins (m, x, apply (t, env))
          | _ => vins (m, x, apply (t, env))
         val venv' = VMap.foldri vfoldFn VMap.empty venv
         fun pfoldFn (x, a, m) =
            if Param.eq (x, a)
            then m
            else pins (m, x, applyP (a, env))
         val penv' = PMap.foldri pfoldFn PMap.empty penv
         val env' = (venv', penv')
      in
         if eq (env, env') then SOME env
         else solve env'
      end

   fun solve' t = case solve t of
      SOME t => t
    | NONE => failwith "Subst.solve"

   fun extend ((venv, penv), bind) = case bind of
      Left (x, t) =>
      if Var.isFixed x
      then failwith ("extending substitution with a fixed variable: " ^ Var.toString x ^ " -> " ^ Term.toString t)
      else let in
         case VMap.find (venv, x) of
            NONE => solve' (VMap.insert (venv, x, t), penv)
          | SOME t' =>
            if T.eq (t, t') then (venv, penv) else
            failwith ("Subst.extend: existing mapping for " ^ Var.toString x)
      end
    | Right (a, b) =>
      if Param.isFixed a then
         failwith "extending substitution with a fixed parameter"
      (* if Param.isFixed a *)
      (*    andalso not (Param.isFixed b) then *)
      (*    failwith "extending substitution with a fixed parameter to non-fixed parameter" *)
      else
         case PMap.find (penv, a) of
            NONE => solve' (venv, PMap.insert (penv, a, b))
          | SOME b' =>
            if Param.eq (b, b') then (venv, penv) else
            failwith ("Subst.extendP: existing mapping for "
                      ^ Param.toString a)

   fun ofList l = foldl (fn (b, t) => extend (t, b)) id l
   fun sing t = ofList [t]

   fun map f t = ofList (List.map f (toBindList t))

   fun binding ((venv, _), x) = VMap.find (venv, x)
   fun bindingP ((_, penv), a) = PMap.find (penv, a)

   fun inDomain ((m,_), v) = VMap.inDomain (m, v)
   fun inDomainP ((_, u), a) = PMap.inDomain (u, a)

   fun isId t = eq (t, id)

   fun img t =
      let
         fun f (bind, (vars, params)) = case bind of
            Left (_, t) => (VSet.union (T.vars t, vars), PSet.union (T.params t, params))
          | Right (_, a) => (vars, PSet.add (params, a))
      in
         Atoms.make (fold f (VSet.empty, PSet.empty) t)
      end

   fun dom t =
      let
         fun f (bind, (vars, params)) = case bind of
            Left (x, _) => (VSet.add (vars, x), params)
          | Right (a, _) => (vars, PSet.add (params, a))
      in
         Atoms.make (fold f (VSet.empty, PSet.empty) t)
      end

   fun fix (t:fixable) =
      map (fn Left (x, t) => Left (x, T.fix t)
            | Right (a, b) => Right (a, Param.fix b))
         t

   fun fix' atoms t =
      map (fn Left (x, t) => Left (x, T.fix' atoms t)
            | Right (a, b) =>
              Right (a, if Atoms.mem (atoms, Right b) then Param.fix b else b))
         t

   fun unfix t =
      map (fn Left (x, t) => Left (x, T.unfix t)
            | Right (a, b) => Right (a, Param.unfix b))
         t

   fun restrict ((m, u), atoms) =
      ( VMap.filteri (fn (x, _) => Atoms.mem (atoms, Left x)) m
      , PMap.filteri (fn (x, _) => Atoms.mem (atoms, Right x)) u )

   fun parse p =
      let
         fun f (one, theta) = case one of
            P.Subst.Var (x, t) => extend (theta, Left (Var.parse x, T.parse t))
          | P.Subst.Param (a, b) =>
            extend (theta, Right (Param.parse a, Param.parse b))
      in
         foldl f id p
      end

   val ofString = parse o P.Subst.ofString


   (* To compose substitutions, t1 o t2, apply t2 to the
      codomain of t1, and add to that result all the assigned
      variables of t2 that aren't part of t1. *)
   fun compose' ((venv1, penv1), s2 as (venv2, penv2)) =
      let
         val venv1' = VMap.map (fn x => apply (x, s2)) venv1
         val vdiff = VMap.difference (venv2, venv1)
         (* Impossible because the intersection is empty by construction *)
         val venv = VMap.unionWith (fn _ => failwith "Subst.compose") (venv1', vdiff)
         val penv1' = PMap.map (fn a => applyP (a, s2)) penv1
         val pdiff = PMap.difference (penv2, penv1)
         (* Impossible because the intersection is empty by construction *)
         val penv =
            PMap.unionWith (fn _ => failwith "Subst.compose") (penv1', pdiff)
      in
         (venv, penv)
      end

   val compose = solve' o compose'

   val composes = foldl compose id

   (* No need to solve a renaming substitution *)
   fun renameTerm (tm, theta) = case tm of
      T.Var v =>
      if Var.isFixed v then (tm, theta) else
      if inDomain (theta, v) then
         (applyV (v, theta), theta)
      else
         let
            val v' = Var.next ()
            val v' = if Var.isFixed v then Var.fix v' else v'
            val v' = T.Var v'
         in
            (v', extend (theta, Left (v, v')))
         end
    | T.Param a =>
      if Param.isFixed a then (tm, theta) else
      if inDomainP (theta, a) then
         (T.Param (applyP (a, theta)), theta)
      else
         let
            val a' = Param.next ()
            val a' = if Param.isFixed a then Param.fix a' else a'
         in
            (T.Param a', extend (theta, Right (a, a')))
         end
    | T.Fn (f, ts) =>
      let
         fun foldFn (t, (ts, theta)) =
            let
               val (t', theta') = renameTerm (t, theta)
            in
               (t'::ts, theta')
            end
         val (ts', theta') = foldr foldFn ([], theta) ts
      in
         (T.Fn (f, ts'), theta')
      end

   (* fun rename t = *)
   (*    let *)
   (*       fun ffn (bind, acc) = case bind of *)
   (*          Left (x, t) => snd (renameTerm (t, acc)) *)
   (*        | Right (a, b) => snd (renameTerm (T.Param b, acc)) *)
   (*    in *)
   (*       foldl ffn t (toBindList t) *)
   (*    end *)

   fun renameAtoms es =
      let
         fun ffn (x, t) = case x of
            Left x => extend (t, Left (x, T.Var (Var.next ())))
          | Right a => extend (t, Right (a, Param.next ()))
      in
         Atoms.fold ffn id es
      end

   fun union (t1 as (x1, a1), t2 as (x2, a2)) =
      let
         fun xfn eq = fn
            (NONE, x) => x
          | (x, NONE) => x
          | (SOME x, SOME y) =>
            if eq (x, y) then SOME x else
            let in
               PP.ppl (%[$"Subst.union: illegal: ", pp t1, $", ", pp t2])
             ; failwith "Subst.union"
            end
      in
         ( VMap.mergeWith (xfn Term.eq) (x1, x2)
         , PMap.mergeWith (xfn Param.eq) (a1, a2) )
      end

   val unions = foldl union id

   fun applyA (ats, t) =
      let
         fun ffn (a, ats) = case a of
            Left x => Atoms.union (ats, T.atoms (apply (T.Var x, t)))
          | Right a => Atoms.add (ats, Right (applyP (a, t)))
      in
         Atoms.fold ffn Atoms.empty ats
      end

   fun invariant t =
      let
         val (xdom, pdom) = Atoms.dest (dom t)
         val (ximg, pimg) = Atoms.dest (img t)
         fun xfn x =
            if Var.isFixed x then
               not (VSet.mem (ximg, Var.unfix x))
            else
               not (VSet.mem (ximg, Var.fix x))
         fun pfn x =
            if Param.isFixed x then
               not (PSet.mem (pimg, Param.unfix x))
            else
               not (PSet.mem (pimg, Param.fix x))
      in
         asserts (fn () => VSet.all (not o Var.isFixed) xdom, "fixed var dom")
       ; asserts (fn () => PSet.all (not o Param.isFixed) pdom, "fixed param dom")
       ; asserts (fn () => VSet.all xfn xdom, "var subst mismatch")
       ; asserts (fn () => PSet.all pfn pdom, "param subst mismatch")
       ; asserts (fn () => VSet.disjoint (xdom, ximg), "vars not idempotent")
       ; asserts (fn () => PSet.disjoint (pdom, pimg), "params not idempotent")
      end

   val () = noWarnUnused (fn _ : parseable => (pp, restrict, applyS, composes, unions))
end

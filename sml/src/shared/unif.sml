
structure Unif :> Unif = struct
   structure T = Term
   structure S = Subst

   open General
   open PP.Ops

   (* istriv returns SOME true if the mapping is trivial,
      SOME false if new information is learned about the
      variable, and NONE if the occurs check fails. *)
   fun istriv env (x, t) = case t of
      T.Var y =>
      if Var.eq (x, y) then SOME true
      else
         let in
            case S.binding (env, y) of
               NONE => SOME false
             | SOME t => istriv env (x, t)
         end
    | T.Fn (_, args) =>
      if List.exists (fn t =>
         case istriv env (x, t) of
            SOME false => false
          | _ => true) args
      then NONE
      else SOME false
    | T.Param _ => SOME false

   (* detect if a param is fixed transitively by substitution *)
   fun isFixedP env a =
      Param.isFixed a orelse
      case S.bindingP (env, a) of
         NONE => false
       | SOME a' => isFixedP env a'

   fun unifyTerms env [] = SOME env
     | unifyTerms env ((t, t')::eqs) =
      case (t, t') of
         (T.Fn (f, fargs), T.Fn (g, gargs)) =>
         if Func.eq (f, g) andalso length fargs = length gargs
         then unifyTerms env (List.zip (fargs, gargs) @ eqs)
         else NONE
       | (T.Var x, _) => unifyVar env (x, t') eqs
       | (_, T.Var x) => unifyVar env (x, t) eqs
       | (T.Param a, T.Param b) => unifyParam env (a, b) eqs
       | _ => NONE

   and unifyVar env (x, t) eqs =
      case S.binding (env, x) of
         SOME t' => unifyTerms env ((t', t) :: eqs)
       | NONE =>
         case istriv env (x, t) of
            NONE => NONE
          | SOME true => unifyTerms env eqs
          | SOME false =>
            if Var.isFixed x then
               case t of
                  T.Var y =>
                  if Var.isFixed y then NONE
                  else if Var.eq (Var.unfix x, y) then failwith "variable fix mismatch"
                  else unifyVar env (y, T.Var x) eqs
                | _ => NONE
            else
               unifyTerms (S.extend (env, Left (x, t))) eqs

   and unifyParam env (a, b) eqs =
      if Param.eq (a, b) then unifyTerms env eqs
      else if Param.isFixed a then
         if Param.isFixed b then NONE
         else if Param.eq (Param.unfix a, b) then
            failwith "parameter fix mismatch"
         else unifyParam env (b, a) eqs
      else
         case S.bindingP (env, a) of
            SOME a' => unifyParam env (a', b) eqs
          | NONE =>
            case S.bindingP (env, b) of
               SOME b' => unifyParam env (a, b') eqs
             | NONE => unifyTerms (S.extend (env, Right (a, b))) eqs

   fun unify eqs = unifyTerms S.id eqs

   fun unify1 eq = unify [eq]

   fun unifiable t12 = isSome (unify1 t12)

   fun general (t1, t2) =
      let
         val atoms = Atoms.make (Term.vars t2, Term.params t2)
         val t1 = Term.fix' atoms t1
         val res = isSome (unify1 (t1, Term.fix t2))
      in
         res
      end

   fun instance (t1, t2) = general (t2, t1)

   fun variant t12 = case unify1 t12 of
      NONE => false
    | SOME tau =>
      S.all (fn Left (_, T.Var _) => true
              | Right (_, a) => not (Param.isFixed a)
              | _ => false) tau

   (* CR: This could be improved by applying bindings from s1 to s2
      incrementally. *)
   fun plus (s1, s2) = unify (S.toTermList s1 @ S.toTermList s2)

   val rec plusl = fn
      [] => SOME S.id
    | [s] => SOME s
    | s1 :: s2 :: ss =>
      case plus (s1, s2) of
         NONE => NONE
       | SOME s => plusl (s :: ss)

   fun subsumes' (t1, t2, atoms) =
      if Subst.isId t1 then SOME Subst.id else
      let
         (* val _ = Log.trace (fn () => %[$"Unif.subsumes': ", Subst.pp t1, \, Subst.pp t2, \, Atoms.pp atoms]) *)
         val (t1, t2) = (Subst.restrict (t1, atoms), Subst.restrict (t2, atoms))
         val (dom1, dom2) = (Subst.dom t1, Subst.dom t2)
      in
         if not (Atoms.subset (dom1, dom2)) then NONE else
         let
            val img2 = Subst.img t2
            val fixed = Atoms.fixed (Atoms.union (Subst.img t1, Subst.img t2))
            val t1' = Subst.fix' img2 t1
            val t2' = Subst.fix t2
            fun loop (acc, t2) = fn
               [] => SOME (Subst.union (acc, t2))
             | Left (x, t) :: rest =>
               let in
                  case Subst.binding (t2', x) of
                     NONE => NONE
                   | SOME t' =>
                     case unify1 (t, t') of
                        NONE => NONE
                      | SOME th =>
                        loop (Subst.compose (acc, th), Subst.remove (t2, Left x))
                           (map (fn b => Subst.applyB (b, th)) rest
                               @ Subst.toBindList th)
               end
             | Right (a, b) :: rest =>
               case Subst.bindingP (t2', a) of
                  NONE => NONE
                | SOME b' =>
                  case unify1 (T.Param b, T.Param b') of
                     NONE => NONE
                   | SOME th =>
                     loop (Subst.compose (acc, th), Subst.remove (t2, Right a))
                        (map (fn b => Subst.applyB (b, th)) rest
                            @ Subst.toBindList th)
         in
            case loop (Subst.id, t2') (Subst.toBindList t1') of
               NONE => NONE
             | SOME t3 =>
               case unify (Subst.toTermList t3 @ Subst.toTermList t2') of
                  NONE => NONE
                | SOME t3 =>
                  let
                     val t3 = Subst.fix' fixed (Subst.unfix t3)
                  in
                     assert
                        (fn () => Subst.eq (Subst.compose (t1, t3), t2),
                         fn () => %[ $"subst subsumption failed: "
                                   , %[\\, &[ Subst.pp t1
                                            , Subst.pp t2
                                            , Subst.pp t3]]])
                   ; SOME t3
                  end
         end
      end

   val subsumes = isSome o subsumes'

   fun check t = (Option.app Subst.invariant t; t)

   val unify = check o unify
   val unify1 = check o unify1
   val plus = check o plus
   val plusl = check o plusl
   val subsumes' = check o subsumes'

   structure Rel = struct
      type t = Rel.t
      val eqs: t * t -> Term.eq list option =
         fn (r1, r2) =>
            let
               val (r1, args1) = Rel.dest r1
               val (r2, args2) = Rel.dest r2
            in
               if not (Pred.eq (r1, r2) andalso length args1 = length args2)
               then NONE
               else SOME (List.zip (args1, args2))
            end

      val unifyfn = unify
      fun unify r12 =
         let in
            (* PP.ppl (%[$"unify: ", Rel.pp (fst r12), Rel.pp (snd r12)]); *)
            case eqs r12 of
               SOME qs => unifyfn qs
             | NONE => NONE
         end
   end

   val () = noWarnUnused (unify1)

end

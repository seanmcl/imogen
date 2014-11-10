
structure Proof :> Proof = struct
   structure Set = RuleId.Set
   structure F = LFormula
   structure Ants = Seq.Ants
   structure Cons = Seq.Cons

   open General
   open PP.Ops

   structure TableQ = Id.Table
   structure TableR = RuleId.Table

   datatype parents = Parents of
      { rid : RuleId.t
      , parents : { id : Id.t, renaming : Subst.t } list
      , unifier : Subst.t }

   structure Node = struct
      datatype t =
         Initial of SC.t
       | Derived of parents
      val pp = fn
         Initial sc => %[SC.pp sc]
       | Derived (Parents { rid, parents, unifier }) =>
         PP.paren
            (%[ RuleId.pp rid, $", "
              , PP.list (map (fn {id, renaming} =>
                                 %[PP.pair(Id.pp id, Subst.pp renaming)])
                            parents)
              , $", ", Subst.pp unifier])
   end

   datatype t = T of
      { f : F.neg
      , unlabel : F.unlabel
      , global : Ants.t
      , seqs : (Seq.t * Node.t * Atoms.t) TableQ.t
      , rules : (RuleInput.t * Fragment.t * Atoms.t) TableR.t
      , parentRules : RuleId.set TableQ.t
      }

   fun reconstruct (T { seqs, rules, ... }, goalId) =
      let
         (* val _ = Log.trace (fn () => &[$"Proof db", %[\\, pp db]]) *)
         fun seq id = case TableQ.find seqs id of
            SOME (_, Node.Initial p, _) => p
          | SOME (_, Node.Derived (Parents {rid, parents, unifier}), _) =>
            let
               val ps = List.map (fn {id, renaming} => SC.apply(seq id, renaming)) parents
               val (_, p, _) = case TableR.find rules rid of
                  NONE => failwith ("Impossible: no id for rule "
                                    ^ RuleId.toString rid ^ " in table")
                | SOME p => p
            in
               SC.apply (Fragment.sc (p, ps), unifier)
            end
          | NONE => failwith ("Impossible: no id for seq "
                              ^ Id.toString id ^ " in table")
      in
         (* The fresh params in the initial rule fragments are unfixed,
            while those in the proof search substitutions are fixed.
            Just unfix everything after the reconstruction. *)
         SC.unfix (seq goalId)
      end

   fun pp (t as T { seqs, rules, parentRules, ... }) =
      let
         val items = TableQ.toListi seqs
         fun item (id, (q, node, ats)) =
            let
               val sc = reconstruct (t, id)
               val (nd, ctx) = SC.nd {norm = Fun.id} sc
               val ctx = &(map (fn (l, r) => %[Label.pp l, $" : ", Rel.pp r]) ctx)
            in
               PP.paren (&[ %[Id.pp id, $":"]
                          , %[\\, &[ Seq.ppNoId q
                                   , Node.pp node
                                   , Atoms.pp ats
                                   , SC.pp sc
                                   , ND.pp nd
                                   , ctx ]]])
            end
         val seqs = &( List.map item items )
         val items = TableR.toListi rules
         fun item (id, (_, p, ats)) =
            PP.paren (&[ %[ RuleId.pp id, $","]
                       , %[\\, &[ Fragment.pp p
                                , Atoms.pp ats ]]])
         val rules = &( List.map item items )
         val items = TableQ.toListi parentRules
         fun item (id, ps) =
            PP.paren (%[ Id.pp id, $", ",
                        %(PP.punctuate PP.comma
                             (map RuleId.pp (Set.toList ps))) ])
         val parentRules = &( List.map item items )
      in
         &[ %[ $"seqs        : ", seqs]
          , %[ $"rules       : ", rules]
          , %[ $"parentRules : ", parentRules] ]
      end

   val reconstruct = fn (db, goalId) =>
      let in
         (* Log.trace (fn () => &[$"Proof db", %[\\, pp db]]); *)
         reconstruct (db, goalId)
      end

   fun create { f, global } =
      let
         exception NotFound
         val unlabel = F.unlabel f
         val seqs = TableQ.create (!Parameters.Proof.table, NotFound)
         val rules = TableR.create (!Parameters.Proof.table, NotFound)
         val parentRules = TableQ.create (!Parameters.Proof.table, NotFound)
      in
         T { f=f, unlabel=unlabel, global=global, seqs=seqs, rules=rules
           , parentRules=parentRules }
      end

   fun addRule (T { rules, ... }, r) =
      let
         val p = RuleInput.proof r
      in
         TableR.insertExn rules (RuleInput.id r, (r, p, RuleInput.atoms r))
      end

   fun checkSeq (t as T { unlabel, global, ...}, q, _) =
      let
         val id = Seq.id q
         val ants = Seq.ants q
         val cons = Seq.cons q
         val sc = reconstruct (t, id)
         val (nd, ctx) = SC.nd {norm = Fun.id} sc
         val erase = fn
            Left p => F.pos F.erase p
          | Right p => F.neg F.erase p
         (* fun ppCtx1 ctx = &(map (fn (l, r) => %[ Label.pp l, $" : ", Rel.pp r]) ctx) *)
         val ctx' = List.mapPartial (fn (l, r) =>
            if Ants.mem (r, ants) orelse Ants.mem (r, global) then
               SOME (l, erase (unlabel r))
            else
               NONE) ctx
         (* fun ppCtx2 ctx = &(map (fn (l, r) => %[ Label.pp l, $" : ", imogen.Formula.pp r]) ctx) *)
         val f = case cons of
            Cons.Xi => imogen.Formula.Bot
          | Cons.P r => erase (unlabel r)
      in
         (* Log.trace (fn () => *)
         (*    &[ $"Proof.checkSeq: " *)
         (*     , %[\\, &[ %[$"seq : ", Seq.pp q] *)
         (*              , %[$"node: ", Node.pp node] *)
         (*              , %[$"sc  : ", SC.pp sc] *)
         (*              , %[$"nd  : ", ND.pp nd] *)
         (*              , %[$"gl  : ", imogen.Formula.pp f] *)
         (*              (\* , %[$"ctx1: ", ppCtx1 ctx] *\) *)
         (*              , %[$"ctx2: ", ppCtx2 ctx']]]]); *)
         ND.check {eq = Term.eq, ctx = ctx', term = nd, form = f}
      end

   fun addSeq (t as T { seqs, rules, parentRules, ... }, q, node) =
      let
         val _ = Log.debug (fn () =>
            &[ $"Proof.addSeq: "
             , %[\\, &[ Seq.pp q
                      , Node.pp node]]])
         val id = Seq.id q
         val qats = Seq.atoms q
         val ps = case node of
            Node.Initial _ => Set.empty
          | Node.Derived (Parents {rid, parents, ...}) =>
            let
               fun f ({id, renaming=_}, acc) =
                  Set.union (acc, TableQ.findExn parentRules id)
            in
               Set.add (foldl f Set.empty parents, rid)
            end
         val atoms = case node of
            Node.Initial _ =>
            let in
               (* (forall X. p) -> p has a free variable in its proof term.
                  it gets closed later during proof cleanup, but I wonder if
                  we should close it now so we can check this constraint. *)
               (* assert *)
               (*    (fn () => Atoms.subset (SC.atoms sc, qats)) *)
               (*    (fn () => &[ $"Proof.addSeq: atoms mismatch" *)
               (*               , %[\\, &[Atoms.pp qats, Atoms.pp (SC.atoms sc)]]]); *)
               qats
            end
          | Node.Derived (Parents {rid, parents, unifier}) =>
            let
               val ats = Subst.img unifier
               fun ok () =
                  let
                     val (_, _, rats) = TableR.findExn rules rid
                     fun mfn {id, renaming} =
                        let
                           val (_, _, qats) = TableQ.findExn seqs id
                        in
                           Subst.applyA (qats, renaming)
                        end
                     val qats = map mfn parents
                     val ats' = Atoms.unions (rats :: qats)
                  in
                     assert
                        (fn () => Atoms.subset (Subst.dom unifier, ats'),
                         fn () =>
                            &[ $"Proof.addSeq: domain mismatch"
                             , %[\\, &[ %[$"atoms  : ", Atoms.pp ats']
                                      , %[$"unifier: ", Subst.pp unifier]
                                      , pp t]]])
                   ; asserts (fn () => Atoms.eq (ats, Subst.img unifier), "Proof.addSeq: image mismatch")
                   ; true
                  end
            in
               asserts (ok, "Proof.atoms.ok")
             ; ats
            end
      in
         assert
            (fn () => Atoms.subset (qats, atoms),
             fn () =>
                &[ $"Proof.addSeq: atom mismatch"
                 , %[\\, &[ Atoms.pp atoms
                          , Seq.pp q
                          , pp t ]]])
       ; TableQ.insertExn seqs (id, (q, node, atoms))
       ; TableQ.insertExn parentRules (id, ps)
       ; uassert (fn () => checkSeq (t, q, node))
      end

   fun parentRules (T { parentRules, ... }, id) = TableQ.findExn parentRules id

   val () = noWarnUnused (pp, parentRules)
end

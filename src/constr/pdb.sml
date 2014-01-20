
structure PDB :> PDB = struct

   structure T = Id.Table
   structure T' = RuleId.Table
   structure P = Fragment

   open General
   open PP.Ops

   val debug = false
   val _ = if debug then printl "Warning!  Proof debugging is on!" else ()

   structure Node = struct
      datatype t =
         Initial of Fragment.t
       | Derived of
         (RuleId.t * {renaming:Subst.t}) * (Id.t * {renaming:Subst.t}) * Subst.t
       | Contract of Id.t * Subst.t * {renaming:Subst.t}
       | ContractRule of RuleId.t * Subst.t * {renaming:Subst.t}
      val pp = fn
         Initial f => P.pp f
       | Derived ((rid, {renaming=t1}), (id, {renaming=t2}), t) =>
         %[RuleId.pp rid, \, Subst.pp t1, $", ", Id.pp id, \, Subst.pp t2, $",", \, Subst.pp t]
       | Contract (qid, t, {renaming}) => %[Id.pp qid, \, Subst.pp t, \, Subst.pp renaming]
       | ContractRule (rid, t, {renaming}) => %[RuleId.pp rid, \, Subst.pp t, \, Subst.pp renaming]
   end

   exception PDB

   datatype t = T of
      { seqs: (Node.t * Atoms.t) Id.Table.t
      , rules: (Node.t * Atoms.t) RuleId.Table.t }
   type printable = t

   fun pp (T {seqs, rules}) =
      let
         val qs = T.pp (fn (id, (p, ats)) => PP.tuple [Id.pp id, Node.pp p, Atoms.pp ats]) seqs
         val rs = T'.pp (fn (id, (p, ats)) => PP.tuple [RuleId.pp id, Node.pp p, Atoms.pp ats]) rules
      in
         &[ %[$"Seqs  : ", qs]
          , %[$"Rules : ", rs] ]
      end

   fun create () =
      T { seqs = T.create (!Parameters.Proof.table, PDB)
        , rules = T'.create (!Parameters.Proof.table, PDB) }

   fun reconstruct (T {seqs, rules}, goalId) =
      let
         fun seq id = case fst (T.findExn seqs id) of
            Node.Initial (P.Leaf p) => p
          | Node.Initial (P.Node _) => raise Impossible
          | Node.Derived ((rid, {renaming=rt}), (qid, {renaming=qt}), t) =>
            let
               val q = SC.apply (seq qid, qt)
               val r = Fragment.mapr (fn p => SC.apply (p, rt)) (rule rid)
            in
               case r of
                  P.Leaf _ => raise Impossible
                | P.Node f =>
                  case f q of
                     P.Node _ => raise Impossible
                   | P.Leaf p => SC.apply (p, t)
            end
          | Node.Contract (qid, t, {renaming}) =>
            SC.apply (seq qid, Subst.compose (t, renaming))
          | Node.ContractRule _ => raise Impossible
         and rule id = case fst (T'.findExn rules id) of
            Node.Initial f => f
          | Node.Derived ((rid, {renaming=rt}), (qid, {renaming=qt}), t) =>
            let
               val q = SC.apply (seq qid, qt)
               val r = Fragment.mapr (fn p => SC.apply (p, rt)) (rule rid)
            in
               case r of
                  P.Leaf _ => raise Impossible
                | P.Node f => Fragment.mapr (fn p => SC.apply (p, t)) (f q)
            end
          | Node.Contract _ => raise Impossible
          | Node.ContractRule (rid, t, {renaming}) =>
            Fragment.mapr (fn p => SC.apply (p, Subst.compose (t, renaming))) (rule rid)
      in
         seq goalId
      end

   fun seq (db as T {seqs, rules, ...}, id, node) =
      let
         val atoms =
            if not debug then Atoms.empty else
            case node of
               Node.Initial sc => Fragment.atoms sc
             | Node.Contract (id, theta, {renaming}) =>
               let
                  val ats = snd (T.findExn seqs id)
                  val t = Subst.compose (theta, renaming)
               in
                  asserts (fn () => Atoms.eq (Subst.dom t, ats), "1")
                ; Subst.img t
               end
             | Node.ContractRule _ => raise Impossible
             | Node.Derived ((rid, {renaming=tr}), (qid, {renaming=tq}), t) =>
               let
                  val qats = snd (T.findExn seqs qid)
                  val () = assert (fn () => Atoms.eq (Subst.dom tq, qats), fn () => &[$"2", pp db])
                  val rats = snd (T'.findExn rules rid)
                  val () = asserts (fn () => Atoms.eq (Subst.dom tr, rats), "3")
                  val img = Atoms.union (Subst.img tq, Subst.img tr)
                  val () = asserts (fn () => Atoms.eq (img, Subst.dom t), "4")
               in
                  Subst.img t
               end
      in
         T.insertExn seqs (id, (node, atoms))
      end

   fun rule (db as T {seqs, rules, ...}, id, node) =
      let
         val atoms =
            if not debug then Atoms.empty else
            case node of
               Node.Initial sc => Fragment.atoms sc
             | Node.ContractRule (id, theta, {renaming}) =>
               let
                  val ats = snd (T'.findExn rules id)
                  val t = Subst.compose (theta, renaming)
               in
                  assert (fn () => Atoms.eq (Subst.dom t, ats), fn () => &[$"5", Node.pp node, pp db])
                ; Subst.img t
               end
             | Node.Contract _ => raise Impossible
             | Node.Derived ((rid, {renaming=tr}), (qid, {renaming=tq}), t) =>
               let
                  val qats = snd (T.findExn seqs qid)
                  val () = asserts (fn () => Atoms.eq (Subst.dom tq, qats), "6")
                  val rats = snd (T'.findExn rules rid)
                  val () = assert (fn () => Atoms.eq (Subst.dom tr, rats),
                                   fn () => &[$"7", Node.pp node, pp db])
                  val img = Atoms.union (Subst.img tq, Subst.img tr)
                  val () = asserts (fn () => Atoms.eq (img, Subst.dom t), "8")
               in
                  Subst.img t
               end
      in
         T'.insertExn rules (id, (node, atoms))
      end

   val () = noWarnUnused (fn _ : printable => (pp))
end

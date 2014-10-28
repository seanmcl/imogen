
structure PDB :> PDB = struct

   structure T = Id.Table
   structure T' = RuleId.Table
   structure P = Fragment

   open General
   open PP.Ops

   structure Node = struct
      datatype t =
         Initial of Fragment.t
       | Derived of RuleId.t * Id.t
      val pp = fn
         Initial f => P.pp f
       | Derived (rid, id) => PP.pair (RuleId.pp rid, Id.pp id)
   end

   exception PDB

   datatype t = T of
      { seqs: Node.t Id.Table.t
      , rules: Node.t RuleId.Table.t }
   type printable = t

   fun pp (T {seqs, rules}) =
      let
         val qs = T.pp (fn (id, p) => PP.pair (Id.pp id, Node.pp p)) seqs
         val rs = T'.pp (fn (id, p) => PP.pair (RuleId.pp id, Node.pp p)) rules
      in
         &[ %[$"Seqs  : ", qs]
          , %[$"Rules : ", rs] ]
      end

   fun create () =
      T { seqs = T.create (!Parameters.Proof.table, PDB)
        , rules = T'.create (!Parameters.Proof.table, PDB) }

   fun seq (T {seqs, ...}, id, node) = T.insertExn seqs (id, node)
   fun rule (T {rules, ...}, id, node) = T'.insertExn rules (id, node)

   fun reconstruct (T {seqs, rules}, goalId) =
      let
         (* val proofs = Id.Table.create (!Parameters.Proof.table, NotFound) *)
         fun seq id =
            (* case T.find proofs id of *)
            (*    SOME pf => pf *)
            (*  | NONE => *)
            let
               val res = case T.findExn seqs id of
                  Node.Initial (P.Leaf p) => p
                | Node.Initial (P.Node _) => raise Impossible
                | Node.Derived (rid, qid) => rule [seq qid] rid
            in
               (* Id.Table.insert proofs (id, res); *)
               res
            end
         and rule seqs id = case T'.findExn rules id of
            Node.Initial f => P.sc (f, seqs)
          | Node.Derived (rid, qid) => rule (seq qid :: seqs) rid
      in
         seq goalId
      end

   val () = noWarnUnused (fn _ : printable => (pp))
end

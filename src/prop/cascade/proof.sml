
structure Proof :> Proof = struct
   open General
   open PP.Ops

   structure Set = RuleId.Set

   structure Rule = struct
      structure Id = RuleId
      type id = Id.t
   end

   structure Node = struct
      datatype t =
         Initial of SC.t
       | Derived of Rule.id * Id.t list
      val pp = fn
         Initial _ => $"Init"
       | Derived (rid, qids) =>
         PP.paren (%[Rule.Id.pp rid, $", ", PP.bracket (%(PP.commas (map Id.pp qids)))])
   end

   structure TableQ = Id.Table
   structure TableR = Rule.Id.Table

   datatype t = T of
      { seqs : Node.t TableQ.t
      , rules : Fragment.t TableR.t
      , parents : RuleId.set TableQ.t }
   type printable = t

   fun create () =
      let
         exception NotFound
         val seqs = TableQ.create (!Parameters.Proof.table, NotFound)
         val rules = TableR.create (!Parameters.Proof.table, NotFound)
         val parents = TableQ.create (!Parameters.Proof.table, NotFound)
      in
         T { seqs = seqs, rules = rules, parents = parents }
      end

   fun rule (T { rules, ... }, id, p) = TableR.insertExn rules (id, p)

   fun seq (T { seqs, parents, ... }, id, node) =
      let
         val ps = case node of
            Node.Initial _ => Set.empty
          | Node.Derived (rid, qids) =>
            let
               fun f (qid, acc) = Set.union (acc, TableQ.findExn parents qid)
            in
               Set.add (foldl f Set.empty qids, rid)
            end
      in
         TableQ.insertExn seqs (id, node)
       ; TableQ.insertExn parents (id, ps)
      end

   fun parents (T { parents, ... }, id) = TableQ.findExn parents id

   fun pp (T { seqs, rules, parents }) =
      let
         val items = TableQ.toListi seqs
         fun item (id, node) =
            PP.paren (%[ Id.pp id, $", ", Node.pp node ])
         val seqs = &( List.map item items )
         val items = TableR.toListi rules
         fun item (id, p) =
            PP.paren (%[ Rule.Id.pp id, $", ", Fragment.pp p ])
         val rules = &( List.map item items )
         val items = TableQ.toListi parents
         fun item (id, ps) =
            PP.paren (%[ Id.pp id, $", ", %(PP.punctuate PP.comma
                                               (map RuleId.pp (Set.toList ps))) ])
         val parents = &( List.map item items )
      in
         &[ %[ $"seqs   : ", seqs]
          , %[ $"rules  : ", rules]
          , %[ $"parents: ", parents] ]
      end

   fun reconstruct (T { seqs, rules, ... }, goalId) =
      let
         fun seq id = case TableQ.find seqs id of
            SOME (Node.Initial p) => p
          | SOME (Node.Derived (rid, qids)) =>
            let
               val ps = List.map seq qids
               val p = case TableR.find rules rid of
                  NONE => failwith ("Impossible: no id for rule " ^ RuleId.toString rid ^ " in table")
                | SOME p => p
            in
               Fragment.sc (p, ps)
            end
          | NONE => failwith ("Impossible: no id for seq " ^ Id.toString id ^ " in table")
      in
         seq goalId
      end

   val () = noWarnUnused (fn _ : printable => pp, parents)
end

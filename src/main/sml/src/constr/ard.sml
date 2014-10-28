
structure ARD :> ARD = struct
   open General
   open PP.Ops

   structure Index = RuleIndex

   datatype t = D of
      { concls: Rule.t Index.t
      , hyps: unit Index.t }

   fun size (D {concls,...}) = Index.size concls

   fun ppDebug (D {concls, hyps}) =
      &[ %[$"concls : ", Index.pp concls]
       , %[$"hyps   : ", Index.pp hyps]
       ]

   fun pp (db as D {concls,...}) n =
      let
         val list = Index.toList concls
         val n = case n of NONE => valOf Int.maxInt | SOME n => n
         val bot = %[\\, &(List.separate \ (map Rule.pp (List.take' (list, n))))]
         val n = size db
      in
         &[ %[$"Active rules: ", PP.int n]
          , if n > 0 then bot else ~ ]
      end

   fun create {entails, global} =
      D { concls = Index.create {entails = entails, global = global, pp = Rule.pp}
        , hyps = Index.create {entails = entails, global = global, pp=PP.unit} }

   fun find (D {concls, ...}, id) = Index.findExn (concls, id)

   fun insert (D {concls, hyps}, r) =
      let
         val id = Rule.id r
         val hyp =
            let
               val (ants, cons) = Rule.firstHyp r
            in
               Seq.new (CFormula.Top, ants, cons)
            end
         val concl =
            let
               val (ants, cons) = Rule.concl r
            in
               Seq.new (Rule.constr r, ants, cons)
            end
      in
         Index.insert (concls, id, concl, r)
       ; Index.insert (hyps, id, hyp, ())
      end

   fun remove (D {concls, hyps}, ids) =
      let
         fun rem1 id =
            if not (Index.member (concls, id)) then () else
            let in
               Log.debug (fn () => %[$"Removing active rule: ", RuleId.pp id])
             ; Index.remove (concls, id)
             ; Index.remove (hyps, id)
            end
      in
         RuleId.Set.app rem1 ids
      end

   fun matches (D {hyps, ...}, seq) = Index.matches(hyps, seq)

   fun subsumed (D {concls, ...}, seq) = Index.subsumed (concls, seq)

   val () = noWarnUnused (ppDebug)
end

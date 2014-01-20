
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

   fun create () =
      D { concls = Index.create {global=NONE, pp=Rule.pp}
        , hyps = Index.create {global=NONE, pp=PP.unit} }

   fun find (D {concls, ...}, id) = Index.findExn (concls, id)

   fun insert (D {concls, hyps}, r) =
      let
         val hyp = Rule.firstHyp r
         val id = Rule.id r
         val concl = Rule.concl r
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

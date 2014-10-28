
structure ASD :> ASD = struct
   open General
   open PP.Ops

   datatype t = D of unit Index.t

   fun create {entails, global} =
      D (Index.create {entails = entails, global = global, pp = PP.unit})

   fun size (D ind) = Index.size ind

   fun find (D ind, id) = fst (Index.findiExn (ind, id))

   (* Remove sequents from the database *)
   val remove : t * Id.set -> unit = fn
      (D ind, ids) =>
      let
         fun remove1 id =
            (* It's ok if it's not a member, as we delete a given sequent
               from both active and kept *)
            if not (Index.member (ind, id))
            then ()
            else
               let in
                  (* ; Debug.pp (fn () => %[$(Util.pad "Removing active seq"), Seq.Id.pp id]) *)
                  Index.remove (ind, id)
               end
      in
         Id.Set.app remove1 ids
      end

   fun subsumed (D ind, seq) = Index.subsumed (ind, seq)

   fun insert (D ind, seq) =
      Index.insert (ind, Seq.id seq, seq, ())

   fun subsumes (D ind, seq) = Index.subsumes (ind, seq)

   fun matches (D ind, rule) =
      let
         val (ants, cons) = Rule.firstHyp rule
         val seq = Seq.new (CFormula.Top, ants, cons)
      in
         Index.matches (ind, seq)
      end

   fun pp (db as D index) n =
      let
         val n = case n of NONE => valOf Int.maxInt | SOME n => n
         val seqs = List.take' (Index.listSeqs index, n)
         val bot =
            &[ %[\\, &(map (fn r => &[~, Seq.pp r]) (List.take' (seqs, n)))]
             , ~
             ]
         val n = size db
      in
         &[ %[ $"Active seqs: ", PP.int n]
          , if n > 0 then bot else ~ ]
      end
end


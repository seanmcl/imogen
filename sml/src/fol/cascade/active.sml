
structure Active :> Active = struct
   open PP.Ops

   datatype t = D of unit Index.t

   fun create {global} = D (Index.create {global=SOME global, pp = PP.unit})

   fun size (D ind) = Index.size ind

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
                  Log.trace (fn () => %[$"Removing subsumed active seq: ", Id.pp id])
                ; Index.remove (ind, id)
               end
      in
         Id.Set.app remove1 ids
      end

   fun subsumed (D ind, seq) = Index.subsumed (ind, seq)

   fun insert (db as D ind, seq) =
      let in
         if !Parameters.Db.useBackwardSubsumption
         then remove (db, subsumed (db, seq)) else ()
       ; Index.insert (ind, Seq.id seq, seq, ())
      end

   fun subsumes (D ind, seq) = Index.subsumes (ind, seq)

   fun pp (D index) n =
      let
         val n = case n of NONE => valOf Int.maxInt | SOME n => n
         val items = Index.listSeqs index
      in
         &(map Seq.pp (List.take' (items, n)))
      end
end


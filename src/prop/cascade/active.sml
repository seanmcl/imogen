
structure Active :> Active = struct
   open PP.Ops

   datatype t = D of { ind : unit Index.t, maxSize : int ref}

   fun create () = D { ind = Index.create (), maxSize = ref 0 }

   fun size (D {ind, ...}) = Index.size ind

   fun maxSize (D { maxSize, ... }) = !maxSize

   (* Remove sequents from the database *)
   val remove : t * Id.set -> unit = fn
      (D {ind, ...}, ids) =>
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

   fun subsumed (D {ind, ...}, seq) = Index.subsumed (ind, seq)

   fun insert (db as D {ind, maxSize}, seq) =
      let in
         if !Parameters.Db.useBackwardSubsumption
         then remove (db, subsumed (db, seq)) else ()
       ; Index.insert (ind, Seq.id seq, seq, ())
       ; if size db > !maxSize then maxSize := size db else ()
      end

   fun subsumes (D {ind, ...}, seq) = Index.subsumes (ind, seq)

   fun pp (D {ind, ...}) n =
      let
         val n = case n of NONE => valOf Int.maxInt | SOME n => n
         val items = Index.listSeqs ind
      in
         &(map Seq.pp (List.take' (items, n)))
      end
end


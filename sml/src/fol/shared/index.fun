
functor IndexFn (Index: TermIndex) : Index
   where type Id.t = Index.Id.t
     and type Id.Set.t = Index.Id.Set.t = struct

   structure Id = Index.Id
   structure Cons = Seq.Cons
   structure Ants = Seq.Ants
   structure S = Id.Set

   open General
   open PP.Ops
   open Cons.Ops

   exception NotFound

   structure VarIndex :> sig
      type t
      include Printable where type printable = t
      val create: unit -> t
      val insert: t * Id.t -> unit
      val delete: t * Id.t -> unit
      (* val clear: t -> unit *)
      val set : t -> Id.set
   end = struct
      structure S = Id.HashSet
      type t = S.t
      type printable = t
      fun create () = S.create (!Parameters.Index.table)
      val insert = S.add
      val delete = S.delete
      (* val clear = S.clear *)
      fun set s = Id.hashSet s
      fun pp (s:printable) = PP.cat (map Id.pp (S.toList s))
   end

   datatype 'a t = D of
      { seqs       : (Seq.t * 'a) Id.Table.t
      , antsIndex  : Index.t
      , consIndex  : Index.t
      , varIndex   : VarIndex.t
      , size       : int ref
      , global     : Ants.t option
      , pp         : 'a -> PP.t
      }

   fun app f (D { seqs, ... }) = Id.Table.app f seqs

   fun fold f init (D { seqs, ... }) = Id.Table.fold f init seqs

   fun pp (D {seqs, antsIndex, consIndex, varIndex, pp=f, ...}) =
      let
         val seqsp = Id.Table.pp (fn (_, (seq, x)) => %[Seq.pp seq, f x]) seqs
      in
         &[ %[$"seqs      : ", seqsp]
          , %[$"antsIndex : ", Index.pp antsIndex]
          , %[$"consIndex : ", Index.pp consIndex]
          , %[$"varIndex  : ", VarIndex.pp varIndex] ]
      end

   fun member (D {seqs, ...}, id) = Id.Table.inDomain seqs id

   fun create {global, pp} =
      D { seqs = Id.Table.create (!Parameters.Index.table, NotFound)
        , antsIndex = Index.create ()
        , consIndex = Index.create ()
        , varIndex = VarIndex.create ()
        , size = ref 0
        , global = global
        , pp = pp }

   fun size (D { size, ...}) = !size

   fun isEmpty i = size i = 0

   fun toList (D {seqs, ...}) = List.map snd (Id.Table.toList seqs)

   fun listSeqs (D {seqs, ...}) = List.map fst (Id.Table.toList seqs)

   fun findi (D {seqs, ...}, id) = Id.Table.find seqs id
   fun findiExn (D {seqs, ...}, id) = Id.Table.findExn seqs id

   fun find (db, id) = Option.map snd (findi (db, id))

   fun getSeq (D {seqs, ...}, id) = fst (Id.Table.findExn seqs id)

   fun findExn x = case find x of
      NONE => raise Impossible
    | SOME x => x

   fun toListi (D {seqs, ...}) = Id.Table.toList seqs

   (* ----------------------------------------------------------------------- *)

   fun insert ( ind as D {seqs, antsIndex, consIndex, varIndex, size, ...}
              , id : Id.t, seq, data) =
      let
         (* val _ = Log.trace (fn () => %[$"Index.insert: ", Seq.pp seq]) *)
         fun insFn index rel =
            Index.insert (index, Rel.toTerm rel, id)
            handle exn as Fail msg => (PP.ppl (&[$"Error: ", $msg, %[\\, pp ind]]); raise exn)
         val antsFn = insFn antsIndex
         val consFn = insFn consIndex
      in
         (* Insert into seqs *)
         if Id.Table.inDomain seqs id then
            let in
               failwith "Index.insert: already present"
            end
         else let in
            Id.Table.insertExn seqs (id, (seq, data))
          (* Insert into antsIndex *)
          ; Ants.app antsFn (Seq.ants seq)
          (* Insert into consIndex or varIndex *)
          ; case Seq.cons seq of
               Xi => VarIndex.insert (varIndex, id)
             | P rel => consFn rel
          ; Ref.incr size
         end
      end

   fun remove ( D {seqs, antsIndex, consIndex, varIndex, size, ...}
              , id) =
      let
      (* val _ = Log.trace (fn () => %[$"Index.remove: ", Id.pp id]) *)
      in
         case Id.Table.find seqs id of
            NONE => ()
          | SOME (seq, _) =>
            let
               fun remFn index rel =
                  Index.delete (index, Rel.toTerm rel, id)
               val antsFn = remFn antsIndex
            in
               (* Delete from seqs *)
               ignore (Id.Table.remove seqs id)
             (* Delete from antsIndex *)
             ; Ants.app antsFn (Seq.ants seq)
             (* Delete from consIndex or varIndex *)
             ; case Seq.cons seq of
                  Xi => VarIndex.delete (varIndex, id)
                | P rel => remFn consIndex rel
             ; Ref.decr size
            end
      end

   (* ----------------------------------------------------------------------- *)

   (* For backward subsumption:

   G1 |- g1 <= G2 |- g2

   if

   G1 <= G2 and g1 = X or g1 <= g2

   Since we don't store the antecedents as sets, we need to return
   an overapproximation.

   if g1 = X, then G1 is nonempty.  Thus, we can return all
   the instances of the elements of G1

   if g1 <> X then we return all instances of g1 on the right.
   *)
   val instances : 'a t * Seq.t -> S.t =
      fn (D {consIndex, antsIndex, ...}, seq) =>
         let
            val ants = Seq.ants seq
            fun foldFn (a_rel, cands) =
               S.intersection (Index.instances (antsIndex, Rel.toTerm a_rel), cands)
            fun antsInter base = Ants.fold foldFn base ants
         in
            case Seq.cons seq of
               Cons.Xi => let in
                  case Ants.hd ants of
                     NONE =>
                     (* Found the goal.  Don't bother doing subsumption *)
                     S.empty
                   | SOME rel =>
                     let
                        val base = Index.instances (antsIndex, Rel.toTerm rel)
                     in
                        antsInter base
                     end
               end
             | Cons.P crel =>
               let
                  val cinsts = Index.instances (consIndex, Rel.toTerm crel)
               in
                  case Ants.hd ants of
                     NONE => cinsts
                   | SOME rel =>
                     let
                        val base = Index.instances (antsIndex, Rel.toTerm rel)
                     in
                        S.intersection (cinsts, antsInter base)
                     end
               end
         end

   val subsumed : 'a t * Seq.t -> S.t = fn
      (db as D {global,...}, seq) =>
      let
         val candidates = instances (db, seq)
         (* val _ = Debug.pp (fn () => %[$(Util.pad "Subsumed candidates"), PP.int (S.size candidates)]) *)
         fun filtFn id = Seq.subsumes (seq, getSeq (db, id), {global=global})
      in
         S.filter filtFn candidates
      end

   (* ----------------------------------------------------------------------- *)

   (* For forward subsumption:

   G1 |- g1 <= G2 |- g2

   if

   G1 <= G2 and g1 = X or g1 <= g2

   if g2 = X, then G2 is nonempty.  Thus, we can return all
   the generalizations of the elements of G2, intersected
   with the sequents with X on the right.

   if g2 <> X then we return all generalizations of g2 or X on the right
   intersected
   *)
   fun general (D {antsIndex, consIndex, varIndex, ...}, seq) =
      let in
         case Seq.cons seq of
            Cons.P crel =>
            (* ants could be empty to subsume *)
            Index.general (consIndex, Rel.toTerm crel)
          | Cons.Xi =>
            let
               val antsCandidates =
                  let
                     fun foldFn (a_rel, cands) =
                        S.union (Index.general (antsIndex, Rel.toTerm a_rel), cands)
                  in
                     Ants.fold foldFn S.empty (Seq.ants seq)
                  end
            in
               (* ants must be nonempty *)
               S.intersection (VarIndex.set varIndex, antsCandidates)
            end
      end

   val subsumes : 'a t * Seq.t -> bool = fn
      (db as D {global, ...}, seq) =>
      let
         val candidates = general (db, seq)
         (* val _ = Log.trace (fn () => %[$(Util.pad "Subsumes candidates"), Id.Set.pp candidates]) *)
         fun existsFn id =
            let
               (* val _ = PP.ppl (%[$"DB: ", pp db]) *)
               val q = getSeq (db, id)
            in
               Seq.subsumes (q, seq, {global=global})
            end
         val res = S.exists existsFn candidates
      in
         res
      end

   fun removeSubsumed (ind, seq) =
      Id.Set.app (fn id => remove (ind, id)) (subsumed (ind, seq))

   fun insertRemoveSubsumed (ind, id, seq, data) =
      if subsumes (ind, seq) then () else
      let in
         removeSubsumed (ind, seq)
       ; insert (ind, id, seq, data)
      end

   (* ----------------------------------------------------------------------- *)

   (* For rule matching. *)
   fun unifiable (D {antsIndex, consIndex, varIndex, ...}, seq) =
      let
         fun candFn (a_rel, cands) =
            S.union (Index.unifiable (antsIndex, Rel.toTerm a_rel), cands)
         val antsCandidates = Ants.fold candFn S.empty (Seq.ants seq)
      in
         case Seq.cons seq of
            Cons.P crel =>
            S.union ( Index.unifiable (consIndex, Rel.toTerm crel),
                     S.intersection (VarIndex.set varIndex, antsCandidates))
          | Cons.Xi =>
            (* Ants is not empty, and at least one ant must match *)
            antsCandidates
      end

   val matches = unifiable

   val () = noWarnUnused (pp, fold, matches, findExn)
end

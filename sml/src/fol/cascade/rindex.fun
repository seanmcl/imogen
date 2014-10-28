
functor RIndexFn (Index: TermIndex where type Id.t = Id.t
                                     and type Id.Set.t = Id.set) : RIndex = struct
   structure Cons = Seq.Cons
   structure Ants = Seq.Ants
   structure S = Id.Set

   open General
   open PP.Ops
   open Cons.Ops

   exception NotFound

   val name = Index.name
   val debug = false

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
      { seqs       : (RSeq.t * 'a) Id.Table.t
      , antsIndex  : Index.t
      , consIndex  : Index.t
      , varIndex   : VarIndex.t
      , size       : int ref
      , global     : Ants.t
      , atoms      : Atoms.t
      , pp         : 'a -> PP.t
      }

   fun invariant (D { seqs, antsIndex, consIndex, varIndex, size, ... }) =
      if not debug then () else
      let in
         asserts (fn () => Id.Table.size seqs = !size, "size")
       ; asserts (fn () => Id.Set.all (Id.Table.inDomain seqs)
                              (Index.items antsIndex), "ants")
       ; asserts (fn () => Id.Set.all (Id.Table.inDomain seqs)
                              (Index.items consIndex), "cons")
       ; asserts (fn () => Id.Set.all (Id.Table.inDomain seqs)
                              (VarIndex.set varIndex), "var")
      end

   (* fun undefinedCons (D { varIndex, ... }) = VarIndex.set varIndex *)

   (* fun clear (D { seqs, antsIndex, consIndex, varIndex, size, ...}) = *)
   (*    let in *)
   (*       Id.Table.clear seqs *)
   (*     ; Index.clear antsIndex *)
   (*     ; Index.clear consIndex *)
   (*     ; VarIndex.clear varIndex *)
   (*     ; size := 0 *)
   (*    end *)

   fun app f (D { seqs, ... }) = Id.Table.app f seqs

   fun fold f init (D { seqs, ... }) = Id.Table.fold f init seqs

   fun pp (D {seqs, antsIndex, consIndex, varIndex, pp, ...}) =
      let
         val seqsp = Id.Table.pp (fn (_, (seq, x)) => %[RSeq.pp seq, pp x]) seqs
      in
         &[ %[$"seqs      : ", seqsp]
          , %[$"antsIndex : ", Index.pp antsIndex]
          , %[$"consIndex : ", Index.pp consIndex]
          , %[$"varIndex  : ", VarIndex.pp varIndex] ]
      end

   fun ppSeqs (D {seqs, ...}) =
      &[ %[$"rindex: ", PP.int (Id.Table.size seqs)]
       , %[\\, Id.Table.pp (fn (_, (seq, _)) => RSeq.pp seq) seqs]]

   fun member (D {seqs, ...}, id) = Id.Table.inDomain seqs id

   fun create {global, atoms, pp} =
      D { seqs = Id.Table.create (!Parameters.Index.table, NotFound)
        , antsIndex = Index.create ()
        , consIndex = Index.create ()
        , varIndex = VarIndex.create ()
        , size = ref 0
        , global = global
        , atoms = atoms
        , pp = pp }

   fun size (D { size, ...}) = !size

   fun isEmpty i = size i = 0

   fun toList (D {seqs, ...}) = List.map snd (Id.Table.toList seqs)

   fun listSeqs (D {seqs, ...}) = List.map fst (Id.Table.toList seqs)

   fun find (D {seqs, ...}, id) = Option.map snd (Id.Table.find seqs id)

   fun getSeq (D {seqs, ...}, id) = fst (Id.Table.findExn seqs id)

   fun findExn x = case find x of
      NONE => raise Impossible
    | SOME x => x

   fun toListi (D {seqs, ...}) = Id.Table.toList seqs

   (* ----------------------------------------------------------------------- *)

   fun insert (db as D {seqs, antsIndex, consIndex, varIndex, size, ...}, rseq, data) =
      let
         val seq = RSeq.seq rseq
         val id = Seq.id seq
         fun insFn index rel = Index.insert (index, Rel.toTerm rel, id)
         val antsFn = insFn antsIndex
         val consFn = insFn consIndex
      in
         (* Insert into seqs *)
         Id.Table.insertExn seqs (id, (rseq, data))
       (* Insert into antsIndex *)
       ; Ants.app antsFn (Seq.ants seq)
       (* Insert into consIndex or varIndex *)
       ; case Seq.cons seq of
            Xi => VarIndex.insert (varIndex, id)
          | P rel => consFn rel
       ; Ref.incr size
       ; invariant db
      end

   val remove : 'a t * Id.t -> unit =
      fn (db as D {seqs, antsIndex, consIndex, varIndex, size, ...}, id) =>
         case Id.Table.find seqs id of
            NONE => raise NotFound
          | SOME (rseq, _) =>
            let
               val seq = RSeq.seq rseq
               fun remFn index rel = Index.delete (index, Rel.toTerm rel, id)
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
             ; invariant db
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
   val instances : 'a t * RSeq.t -> S.t =
      fn (D {consIndex, antsIndex, ...}, rseq) =>
         let
            val seq = RSeq.seq rseq
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

   val subsumed : 'a t * RSeq.t -> S.t = fn
      (db as D {global, atoms, ...}, rseq) =>
      let
         val candidates = instances (db, rseq)
         (* val _ = Debug.pp (fn () => %[$(Util.pad "Subsumed candidates"), PP.int (S.size candidates)]) *)
         fun filtFn id = RSeq.subsumes (rseq, getSeq (db, id), {global=global, atoms=atoms})
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
   fun general (D {antsIndex, consIndex, varIndex, ...}, rseq) =
      let
         val seq = RSeq.seq rseq
      in
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

   val subsumes : 'a t * RSeq.t -> bool = fn
      (db as D {global, atoms, ...}, rseq) =>
      let
         val candidates = general (db, rseq)
         (* val _ = Log.trace (fn () => %[$"Subsumes candidates: ", PP.int (S.size candidates)]) *)
         fun existsFn id =
            RSeq.subsumes (getSeq (db, id), rseq, {global=global, atoms=atoms})
      in
         S.exists existsFn candidates
      end

   fun removeSubsumed (ind, rseq) =
      let in
         Id.Set.app (fn id => remove (ind, id)) (subsumed (ind, rseq))
       ; invariant ind
      end

   fun insertRemoveSubsumed (ind, rseq, data) =
      if subsumes (ind, rseq) then () else
      let in
         removeSubsumed (ind, rseq)
       ; insert (ind, rseq, data)
       ; invariant ind
      end

   (* ----------------------------------------------------------------------- *)

   (* For rule matching. *)
   fun unifiable (D {antsIndex, consIndex, varIndex, ...}, rseq) =
      let
         val seq = RSeq.seq rseq
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

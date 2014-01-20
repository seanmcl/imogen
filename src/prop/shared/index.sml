
functor IndexFn (Id : Id) :> Index
   where type Id.t = Id.t
     and type Id.Set.t = Id.Set.t = struct

   structure Id = Id
   structure Cons = Seq.Cons
   structure Ants = Seq.Ants
   structure S = Id.Set

   open General
   open PP.Ops
   open Cons.Ops

   exception NotFound

   (* Sequents with empty consequent. *)
   structure VarIndex : sig
      type t
      include Printable where type printable = t
      val create : unit -> t
      val insert : t * Id.t -> unit
      val delete : t * Id.t -> unit
      val clear : t -> unit
      val set : t -> Id.set
      val hashSet : t -> Id.hash_set
   end = struct
      structure S = Id.HashSet
      type t = S.t
      type printable = t
      fun create () = S.create (!Parameters.Index.table)
      val insert = S.add
      val delete = S.delete
      val clear = S.clear
      fun set s = Id.hashSet s
      fun hashSet s = s
      fun pp (s:printable) = PP.cat (map Id.pp (S.toList s))
   end

   datatype 'a t = D of
      { seqs      : (Seq.t * 'a) Id.Table.t
      , antsIndex : Id.hash_set Pred.Table.t
      , consIndex : Id.hash_set Pred.Table.t
      , varIndex  : VarIndex.t
      , size      : int ref }
   type 'a printable1 = 'a t

   fun undefinedCons (D { varIndex, ... }) = VarIndex.set varIndex

   fun clear (D { seqs, antsIndex, consIndex, varIndex, size }) =
      let in
         Id.Table.clear seqs
       ; Pred.Table.clear antsIndex
       ; Pred.Table.clear consIndex
       ; VarIndex.clear varIndex
       ; size := 0
      end

   fun app f (D { seqs, ... }) = Id.Table.app f seqs

   fun fold f init (D { seqs, ... }) = Id.Table.fold f init seqs

   fun pp f (D {seqs, antsIndex, consIndex, varIndex, ...} : 'a printable1) =
      let
         val seqsp = Id.Table.pp (fn (_, (seq, x)) => %[Seq.pp seq, f x]) seqs
         val antsIndexp = Pred.Table.pp
                             (fn (rel, s) => PP.pair (Pred.pp rel, Id.HashSet.pp s)) antsIndex
         val consIndexp = Pred.Table.pp
                             (fn (rel, s) => PP.pair (Pred.pp rel, Id.HashSet.pp s)) consIndex
         val varIndexp = VarIndex.pp varIndex
      in
         &[ %[$"seqs      : ", seqsp]
          , %[$"antsIndex : ", antsIndexp]
          , %[$"consIndex : ", consIndexp]
          , %[$"varIndex  : ", varIndexp] ]
      end

   fun member (D {seqs, ...}, id) = Id.Table.inDomain seqs id

   fun create () =
      D { seqs = Id.Table.create (!Parameters.Index.table, NotFound)
        , antsIndex = Pred.Table.create (!Parameters.Index.table, NotFound)
        , consIndex = Pred.Table.create (!Parameters.Index.table, NotFound)
        , varIndex = VarIndex.create ()
        , size = ref 0 }

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

   fun insert (D {seqs, antsIndex, consIndex, varIndex, size, ...}, id, seq, data) =
      let
         fun insFn index rel = case Pred.Table.find index rel of
            NONE => Pred.Table.insertExn index (rel, Id.HashSet.singleton id)
          | SOME s => Id.HashSet.add (s, id)
         val antsFn = insFn antsIndex
         val consFn = insFn consIndex
      in
         (* Insert into seqs *)
         Id.Table.insertExn seqs (id, (seq, data))
       (* Insert into antsIndex *)
       ; Ants.app antsFn (Seq.ants seq)
       (* Insert into consIndex or varIndex *)
       ; case Seq.cons seq of
            Xi => VarIndex.insert (varIndex, id)
          | P rel => consFn rel
       ; Ref.incr size
      end

   val remove : 'a t * Id.t -> unit = fn
      (D {seqs, antsIndex, consIndex, varIndex, size, ...}, id) =>
      case Id.Table.find seqs id of
         NONE => raise NotFound
       | SOME (seq, _) =>
         let
            fun remFn index lab = case Pred.Table.find index lab of
               NONE => raise NotFound
             | SOME s =>
               let in
                  Id.HashSet.delete (s, id)
                ; if Id.HashSet.isEmpty s
                  then ignore (Pred.Table.remove index lab)
                  else ()
               end
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

   (* G |- A.  If A = X then we return true iff there is a sequent with cons X
      and G' <= G.  If A <> X then we return true iff there is a sequent with
      cons X or cons A and G' <= G. *)
   fun subsumes (db as D {consIndex, varIndex, ...}, seq) =
      let
         fun exFn id = Seq.subsumes (getSeq (db, id), seq)
      in
         (* Only check the var sequents that have no more antecedents than
            the incoming sequent *)
         Id.HashSet.exists exFn (VarIndex.hashSet varIndex) orelse
         case Seq.cons seq of
            (* If A = X then we are done *)
            Xi => false
          (* Otherwise sequents G' |- A could subsume *)
          | P rel =>
            case Pred.Table.find consIndex rel of
               NONE => false
             | SOME s => Id.HashSet.exists exFn s
      end

   (* G ⊢ A matches G' ⊢ A' if one of the following conditions holds


      1) A = A' ≠ Ξ
      2) A = A' = Ξ and G' ∩ G ≠ ·
      3) A = Ξ, A' ≠ Ξ, and G' ∩ G ≠ ·
      4) A' = Ξ, A ≠ Ξ, and G' ∩ G ≠ ·

      This reduces to the three cases:

      1) A = A' ≠ Ξ
      2) A = Ξ, G' ∩ G ≠ ·
      3) A ≠ Ξ, A' = Ξ, G' ∩ G ≠ ·

      Thus matching is symmetric.
   *)
   val matches : 'a t * Seq.t -> Id.set = fn
      (D {consIndex, antsIndex, varIndex, ...}, seq) =>
      let
         (* All sequents with an antecedent in G *)
         val antsMatches =
            let
               fun foldFn (ant, ids) = case Pred.Table.find antsIndex ant of
                  NONE => ids
                | SOME s => Id.HashSet.fold (fn (x, ids) => S.add (ids, x)) ids s
            in
               Ants.fold foldFn S.empty (Seq.ants seq)
            end
         val xiMatches = VarIndex.set varIndex
      in
         case Seq.cons seq of
            P rel =>
            let
               (* case1: A = A' ≠ Ξ *)
               val case1 = case Pred.Table.find consIndex rel of
                  NONE => S.empty
                | SOME s => Id.hashSet s
               (* case3: A ≠ Ξ, A' = Ξ, G' ∩ G ≠ · *)
               val case3 = S.intersection (antsMatches, xiMatches)
            in
               S.union (case1, case3)
            end
          | Xi =>
            (* case2: A = Ξ, G' ∩ G ≠ · *)
            antsMatches
      end


   (* There is some question about how best to do the instance (backward
      subsumption) check.  The difficulty lies in testing that one set of
      antecedents is a subset of another.

      Possibilities

      1) For each antecedent, grab the set of sequent ids that have that as an
         antecedent, then do a big intersection over all those sets.

      2) Choose the smallest set of sequent ids having one of the antecedents.
         Go through that set linerly by looking up the actual sequent
         corresponding to the id and checking whether the antecedents are a
         subset.

      Clearly if one of the sets is small but the others are large, 2 is
      superior.  2 would also be better if there are a large number of sets that
      are relatively small.

      1 would be better if all the sets are equally large, but the intersection
      is small.  Moreover, you don't need to store the full sequents.  It's also
      a bit nicer.  There is no translating back and forth from sequents and
      ids, lists and sets.

      We'll implement 2.

      Define G ⊢ A ≤ G' ⊢ A' iff A ≤ A' and G ⊆ G'

      We must return all G' ⊢ A' such that G ⊢ A ≤ G' ⊢ A'

      This is complicated by whether A or A' is Ξ.  If A = Ξ then all A' ≤ A,
      so we should check G ≤ G' first.  Once we have those sequents, we
      can either return them all, in the case A = Ξ, or filter by A' = A <> Ξ.

      The only problem with this method is, what if G is empty?  We could still
      have A = Ξ because even though · |- Ξ is a contradiction, we could have
      contradictory globalized antecedents.  But in that case, · |- Ξ subsumes
      the goal.  Thus, it will shortly be chosen to create a proof, and in any
      case, it isn't subsumed anyway!  Thus, we can assume that if G = · then
      A ≠ Ξ.  In that case, we just return all sequents G' ⊢ A
   *)
   val subsumed : 'a t * Seq.t -> Id.set = fn
      (db as D {consIndex, ...}, seq) =>
      let
         (* Get the set with smallest cardinality in the antsIndex that shares
            an antecedent with ants.  This isn't totally trivial because it
            might be the case that no antecedent in ants is in the index.  In
            this case, we should return the empty set. *)
         val grabSmallest : 'a t * Seq.ants -> Id.set = fn
            (D {antsIndex, ...}, ants) =>
            let
               fun foldFn (ant, (set, size)) = case Pred.Table.find antsIndex ant of
                  NONE => (set, size)
                | SOME set' =>
                  let
                     val size' = Id.HashSet.size set'
                  in
                     if size' < size
                     then (Id.hashSet set', size')
                     else (set, size)
                  end
               val (set, _) = Ants.fold foldFn (S.empty, 10000000) ants
            in
               set
            end
      in
         (* First check whether we're in the case where G = · *)
         if Ants.isEmpty (Seq.ants seq) then
            case Seq.cons seq of
               Xi =>
               (* Found the goal.  Don't bother doing subsumption *)
               S.empty
             | P rel =>
               case Pred.Table.find consIndex rel of
                  NONE => S.empty
                | SOME ids => Id.hashSet ids
         (* Otherwise, get all sequents such that G' ⊆ G *)
         else
            let
               val ids = grabSmallest (db, Seq.ants seq)
               fun filtFn id = Seq.subsumes (seq, getSeq (db, id))
            in
               S.filter filtFn ids
            end
      end

   fun removeSubsumed (ind, seq) =
      S.app (fn id => remove (ind, id)) (subsumed (ind, seq))

   fun insertRemoveSubsumed (ind, id, seq, data) =
      if subsumes (ind, seq) then () else
      let in
         removeSubsumed (ind, seq)
       ; insert (ind, id, seq, data)
      end

   val () = noWarnUnused (pp, fold, matches, findExn)
end

structure Index = IndexFn(Id)
structure RuleIndex = IndexFn(RuleId)

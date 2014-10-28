
functor PathIndexFn (Id : Id) : TermIndex
   where type Id.t = Id.t
     and type Id.Set.t = Id.Set.t = struct
   structure Id = Id
   structure P = PathFn (Id)
   open General

   type t = P.t ref
   type printable = t

   fun create () = ref P.empty
   fun clear t = t := P.empty

   val name = "Path"

   fun items (ref t) = P.items t
   fun insert (trie' as ref trie, t, x) = trie' := P.insert (trie, t, x);
   fun delete (trie' as ref trie, t, x) = trie' := P.delete (trie, t, x)
   fun size (ref trie) = P.size trie
   fun contains (ref trie, x) = P.contains (trie, x)
   fun pp (ref trie : printable) = P.pp trie

   fun variants (ref trie, t) = P.variants (trie, t)
   fun unifiable (ref trie, t) = P.unifiable (trie, t)
   fun general (ref trie, t) = P.general (trie, t)
   fun instances (ref trie, t) = P.instances (trie, t)

   val () = noWarnUnused (size, contains, variants, items, clear)
end

structure PathIndex = PathIndexFn (Id)
structure RulePathIndex = PathIndexFn (RuleId)

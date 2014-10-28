
structure HashTable : HashTable = struct
   structure H = HashTable
   open H

   type ('a, 'b) t = ('a, 'b) H.hash_table

   val toList = H.listItems
   val toListi = H.listItemsi
   val size = H.numItems
   val create = H.mkTable
   fun pp f tbl = PP.vcat (List.map f (toListi tbl))
   val findExn = H.lookup

   local
      exception Nope
   in
      fun all f tbl =
         let in
            H.appi (fn x => if f x then () else raise Nope) tbl
          ; true
         end handle Nope => false
   end
   val replace = H.insert
   fun insertExn t (k, v) =
      if H.inDomain t k then raise (Fail "HashTable.insert")
      else H.insert t (k, v)
end

functor HashTableFn (Key : HashKey) : MonoHashTable where type key = Key.t = struct
   structure T =
      HashTableFn
         (struct
            open Key
            type hash_key = t
            val hashVal = hash
            val sameKey = eq
            val () = ignore (pp) (* MLton warning *)
         end)

   open T
   type 'a t = 'a hash_table
   type key = Key.hash_key
   val toList = listItems
   val toListi = listItemsi
   val size = numItems
   val create = mkTable
   val findExn = lookup
   fun pp f tbl = PP.vcat (List.map f (toListi tbl))
   fun update t f key = insert t (key, f (find t key))
   val replace = T.insert
   fun insertExn t (k, v) =
      if T.inDomain t k then raise (Fail "HashTable.insert")
      else T.insert t (k, v)

   local
      exception Nope
   in
      fun all f tbl =
         let in
            appi (fn x => if f x then () else raise Nope) tbl
          ; true
         end handle Nope => false
   end
end

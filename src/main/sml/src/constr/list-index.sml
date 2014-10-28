
functor ListIndexFn (Id : Id) : TermIndex = struct
   structure Id = Id
   open General
   open PP.Ops

   val name = "LinkedList"

   type t = (Rel.t * Id.t) list ref
   type printable = t

   fun clear t = t := []

   fun create () = ref []

   fun size t = length (!t)

   fun contains (t, x) = List.exists (fn (_, x') => Id.eq (x, x')) (!t)

   fun items (ref l) = Id.Set.ofList (map snd l)

   fun pp t = &(map (fn (t, x) => PP.pair (Rel.pp t, Id.pp x)) (!t))

   (* type ctx = (Var.t * Var.t) list * (Param.t * Param.t) list *)

   fun insert (ind, t, x) =
      let in
         (* Log.trace (fn () => %[$"ListIndex.insert: ", Term.pp t, $", ", Id.pp x]); *)
         ind := (t, x) :: !ind
      end

   fun delete (ind, t, x) =
      let in
         (* Log.trace (fn () => %[$"ListIndex.delete: ", pp ind, Term.pp t, $", ", Id.pp x]); *)
         case
            List.findRem
               (fn (t', x') => CUnif.variant (t, t') andalso Id.eq (x, x'))
               (!ind)
         of NONE => ()
          | SOME (_, ind') => ind := ind'
      end

   fun comb f (ind, t) =
      foldl (fn ((t', id), acc) =>
                if f (t', t) then Id.Set.add(acc, id) else acc)
         Id.Set.empty
         (!ind)

   val variants = comb CUnif.variant
   val instances = comb CUnif.instance
   val unifiable = comb CUnif.unifiable
   val general = comb CUnif.general
end

structure ListIndex = ListIndexFn(Id)
structure RuleListIndex = ListIndexFn(RuleId)

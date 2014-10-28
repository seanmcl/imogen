
functor ListIndexFn (Id : Id) : TermIndex = struct
   structure Id = Id
   structure T = Term
   open General
   open PP.Ops

   val name = "LinkedList"

   type t = (Term.t * Id.t) list ref
   type printable = t

   fun clear t = t := []

   fun create () = ref []

   fun size t = length (!t)

   fun contains (t, x) = List.exists (fn (_, x') => Id.eq (x, x')) (!t)

   fun items (ref l) = Id.Set.ofList (map snd l)

   fun pp t = &(map (fn (t, x) => PP.pair (Term.pp t, Id.pp x)) (!t))

   type ctx = (Var.t * Var.t) list * (Param.t * Param.t) list

   (* Don't rename fixed parameters.  They remain unchanged during rule
      matching. *)
   val rec rename : Term.t * ctx -> Term.t * ctx =
      fn (t, ctx as (vbnds, pbnds)) => case t of
         T.Var x =>
         let in
            case List.genAssoc Var.eq (x, vbnds) of
               SOME v => (T.Var v, ctx)
             | NONE =>
               let
                  val v = Var.next ()
               in
                  (T.Var v, (((x, v) :: vbnds), pbnds))
               end
         end
       | T.Param a =>
         let in
            if Param.isFixed a then (t, ctx) else
            case List.genAssoc Param.eq (a, pbnds) of
               SOME b => (T.Param b, ctx)
             | NONE =>
               let
                  val b = Param.next ()
               in
                  (T.Param b, (vbnds, (a, b) :: pbnds))
               end
         end
       | T.Fn (f, ts) =>
         let
            fun foldFn (t, (ts, ctx)) =
               let
                  val (t, ctx) = rename (t, ctx)
               in
                  (t :: ts, ctx)
               end
            val (ts, ctx) = foldr foldFn ([], ctx) ts
         in
            (T.Fn (f, ts), ctx)
         end

   fun insert (ind, t, x) =
      let in
         (* PP.ppl (%[$"ListIndex.insert: ", Term.pp t, $", ", Id.pp x]); *)
         ind := (fst (rename (t, ([], []))), x) :: !ind
      end

   fun delete (ind, t, x) =
      let in
         (* Log.trace (fn () => %[$"ListIndex.delete: ", pp ind, Term.pp t, $", ", Id.pp x]); *)
         case
            List.findRem
               (fn (t', x') => Unif.variant (t, t') andalso Id.eq (x, x'))
               (!ind)
         of NONE => ()
          | SOME (_, ind') => ind := ind'
      end

   fun comb f (ind, t) =
      foldl (fn ((t', id), acc) =>
                if f (t', t) then Id.Set.add(acc, id) else acc)
         Id.Set.empty
         (!ind)

   val variants = comb Unif.variant
   val instances = comb Unif.instance
   val unifiable = comb Unif.unifiable
   val general = comb Unif.general
end

structure ListIndex = ListIndexFn(Id)
structure RuleListIndex = ListIndexFn(RuleId)

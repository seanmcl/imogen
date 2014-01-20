
functor PathFn (Id : Id) :> Path
   where type Id.t = Id.t
     and type Id.Set.t = Id.Set.t = struct
   open General
   open PP.Ops
   structure Id = Id
   structure S = Id.Set
   exception NotFound

   (* ----------------------------------------------------------------------- *)

   structure Sym = struct
      datatype t =
         Var
       | Param
       | Func of Func.t

      val compare = fn
         (Var, Var) => EQUAL
       | (Var, _) => LESS
       | (_, Var) => GREATER
       | (Param, Param) => EQUAL
       | (Param, _) => LESS
       | (_, Param) => GREATER
       | (Func f1, Func f2) => Func.compare (f1, f2)

      val pp = fn
         Var => $"X"
       | Param => $"a"
       | Func f => Func.pp f

      structure Map = OrdMapFn (struct
         type t = t
         val compare = compare
         val pp = pp
         val () = noWarnUnused (pp)
      end)
   end
   datatype sym = datatype Sym.t
   structure M = Sym.Map

   (* ----------------------------------------------------------------------- *)

   (* Tries *)
   datatype t =
      Leaf of Id.set
    | Node of t list M.t

   type printable = t

   val empty = Node M.empty

   val rec pp : printable -> PP.t = fn
      Leaf s => %[$"Leaf: ", S.pp s]
    | Node m =>
      %[$"Node: ", M.ppVert (fn (sym, l) => %[Sym.pp sym, \, &(map pp l)]) m]

   val rec items = fn
      Leaf s => s
    | Node m =>
      M.foldl (fn (tries, s) => S.unions (s :: map items tries)) S.empty m

   fun contains (trie, x) = S.mem (items trie, x)

   fun size trie = S.size (items trie)

   (* ----------------------------------------------------------------------- *)

   val rec insert = fn
      (Leaf _, _, _) => raise Impossible
    | (Node m, t, item) =>
      case t of
         Term.Var _ =>
         let in
            case M.find (m, Var) of
               SOME [Leaf s] =>
               Node (M.replace (m, Var, [Leaf (S.add (s, item))]))
             | NONE =>
               Node (M.insert (m, Var, [Leaf (S.singleton item)]))
             | _ => raise Impossible
         end
       | Term.Param _ =>
         let in
            case M.find (m, Param) of
               SOME [Leaf s] =>
               Node (M.replace (m, Param, [Leaf (S.add (s, item))]))
             | NONE =>
               Node (M.insert (m, Param, [Leaf (S.singleton item)]))
             | _ => raise Impossible
         end
       | Term.Fn (f, []) =>
         let in
            case M.find (m, Func f) of
               SOME [Leaf s] =>
               Node (M.replace (m, Func f, [Leaf (S.add (s, item))]))
             | NONE =>
               Node (M.insert (m, Func f, [Leaf (S.singleton item)]))
             | _ => raise Impossible
         end
       | Term.Fn (f, args) =>
         let
            val n = length args
         in
            case M.find (m, Func f) of
               SOME tries =>
               let
                  val _ = assert' (fn () => length tries = n)
                  val tries' =
                     List.map2
                        (fn (trie, arg) => insert (trie, arg, item))
                        (tries, args)
               in
                  Node (M.replace (m, Func f, tries'))
               end
             | NONE =>
               let
                  val empties = List.tabulate (n, fn _ => Node M.empty)
               in
                  insert (Node (M.insert (m, Func f, empties)), t, item)
               end
         end

   (* ----------------------------------------------------------------------- *)

   (* Assumes the term to delete is present *)

   val rec isEmpty = fn
      Leaf s => S.isEmpty s
    | Node m => M.all (List.all isEmpty) m

   val rec delete = fn
      (Leaf _, _, _) => raise Impossible
    | (map as Node m, t, item) =>
      case t of
         Term.Var _ =>
         let in
            case M.find (m, Var) of
               SOME [Leaf s] =>
               let
                  val s' = S.remove (s, item)
                     handle NotFound =>
                        let in
                           (* Debug.pp (fn () => &[%[$"couldn't find ", S.ppItem item, $" in ", S.pp s], *)
                           (* $"must be in another branch... ignoring..."]); *)
                           s
                        end
               in
                  if S.isEmpty s'
                  then Node (fst (M.removeExn (m, Var)))
                  else Node (M.replace (m, Var, [Leaf s']))
               end
             | _ => map (* Deleted earlier *)
         end
       | Term.Param _ =>
         let in
            case M.find (m, Param) of
               SOME [Leaf s] =>
               let
                  val s' = S.remove (s, item)
                     handle NotFound =>
                        let in
                           (* Debug.pp (fn () => &[%[$"couldn't find ", S.ppItem item, $" in ", S.pp s], *)
                           (* $"must be in another branch... ignoring..."]); *)
                           s
                        end
               in
                  if S.isEmpty s'
                  then Node (fst (M.removeExn (m, Param)))
                  else Node (M.replace (m, Param, [Leaf s']))
               end
             | _ => map (* Deleted earlier *)
         end
       | Term.Fn (f, []) =>
         let in
            case M.find (m, Func f) of
               SOME [Leaf s] =>
               let
                  val s' = S.remove (s, item)
                     handle NotFound =>
                        let in
                           (* Debug.pp (fn () => &[%[$"couldn't find ", S.ppItem item, $" in ", S.pp s], *)
                           (* $"must be in another branch... ignoring..."]); *)
                           s
                        end
               in
                  if S.isEmpty s'
                  then Node (fst (M.removeExn (m, Func f)))
                  else Node (M.replace (m, Func f, [Leaf s']))
               end
             | _ =>
               let in
                  (* Debug.pp (fn () => &[%[$"I couldn't find ", S.ppItem item] *)
                  (* , $" it must have been deleted earlier..." *)
                  (* ]); *)
                  map
               end
         end
       | Term.Fn (f, args) =>
         let
            val n = length args
         in
            case M.find (m, Func f) of
               SOME tries =>
               let
                  val _ = assert' (fn () => length tries = n)
                  val tries' =
                     List.map2
                        (fn (trie, arg) => delete (trie, arg, item))
                        (tries, args)
               in
                  if List.all isEmpty tries'
                  then Node (fst (M.removeExn (m, Func f)))
                  else Node (M.replace (m, Func f, tries'))
               end
             | NONE => (* Deleted earlier *)
               map
         end

   (* ----------------------------------------------------------------------- *)

   val rec variants = fn
      (Node m, Term.Var _) =>
      let in
         case M.find (m, Var) of
            NONE => S.empty
          | SOME [Leaf s] => s
          | _ => raise Impossible
      end
    | (Node m, Term.Param _) =>
      let in
         case M.find (m, Param) of
            NONE => S.empty
          | SOME [Leaf s] => s
          | _ => raise Impossible
      end
    | (Node m, Term.Fn (f, [])) =>
      let in
         case M.find (m, Func f) of
            NONE => S.empty
          | SOME [Leaf s] => s
          | _ => raise Impossible
      end
    | (Node m, Term.Fn (f, args)) =>
      let in
         case M.find (m, Func f) of
            NONE => S.empty
          | SOME tries =>
            let in
               assert' (fn () => length args = length tries)
             ; S.intersections (List.map2 variants (tries, args))
            end
      end
    | _ => raise Impossible

   (* ----------------------------------------------------------------------- *)

   val rec instances = fn
      (node, Term.Var _) => items node
    | (Node m, Term.Param _) =>
      let in
         case M.find (m, Param) of
            NONE => S.empty
          | SOME [Leaf s] => s
          | _ => raise Impossible
      end
    | (Node m, Term.Fn (f, [])) =>
      let in
         case M.find (m, Func f) of
            NONE => S.empty
          | SOME [Leaf s] => s
          | _ => raise Impossible
      end
    | (Node m, Term.Fn (f, args)) =>
      let in
         case M.find (m, Func f) of
            NONE => S.empty
          | SOME tries =>
            let in
               assert' (fn () => length args = length tries)
             ; S.intersections (List.map2 instances (tries, args))
            end
      end
    | _ => raise Impossible

   (* ----------------------------------------------------------------------- *)

   val rec unifiable = fn
      (node, Term.Var _) => items node
    | (Node m, Term.Param _) =>
      let
         val params = case M.find (m, Param) of
            NONE => S.empty
          | SOME [Leaf s] => s
          | _ => raise Impossible
         val vars = case M.find (m, Var) of
            NONE => S.empty
          | SOME [Leaf s] => s
          | _ => raise Impossible
      in
         S.union (params, vars)
      end
    | (Node m, Term.Fn (f, [])) =>
      let
         val fs = case M.find (m, Func f) of
            NONE => S.empty
          | SOME [Leaf s] => s
          | _ => raise Impossible
         val vars = case M.find (m, Var) of
            NONE => S.empty
          | SOME [Leaf s] => s
          | _ => raise Impossible
      in
         S.union (fs, vars)
      end
    | (Node m, Term.Fn (f, args)) =>
      let
         val fs = case M.find (m, Func f) of
            NONE => S.empty
          | SOME tries =>
            let in
               assert' (fn () => length args = length tries)
             ; S.intersections (List.map2 unifiable (tries, args))
            end
         val vars = case M.find (m, Var) of
            NONE => S.empty
          | SOME [Leaf s] => s
          | _ => raise Impossible
      in
         S.union (fs, vars)
      end
    | _ => raise Impossible

   (* ----------------------------------------------------------------------- *)

   val rec general = fn
      (Node m, Term.Var _) =>
      let in
         case M.find (m, Var) of
            NONE => S.empty
          | SOME [Leaf s] => s
          | _ => raise Impossible
      end
    | (Node m, Term.Param _) =>
      let
         val params = case M.find (m, Param) of
            NONE => S.empty
          | SOME [Leaf s] => s
          | _ => raise Impossible
         val vars = case M.find (m, Var) of
            NONE => S.empty
          | SOME [Leaf s] => s
          | _ => raise Impossible
      in
         S.union (params, vars)
      end
    | (Node m, Term.Fn (f, [])) =>
      let
         val fs = case M.find (m, Func f) of
            NONE => S.empty
          | SOME [Leaf s] => s
          | _ => raise Impossible
         val vars = case M.find (m, Var) of
            NONE => S.empty
          | SOME [Leaf s] => s
          | _ => raise Impossible
      in
         S.union (fs, vars)
      end
    | (Node m, Term.Fn (f, args)) =>
      let
         val fs = case M.find (m, Func f) of
            NONE => S.empty
          | SOME tries =>
            let in
               assert' (fn () => length args = length tries)
             ; S.intersections (List.map2 general (tries, args))
            end
         val vars = case M.find (m, Var) of
            NONE => S.empty
          | SOME [Leaf s] => s
          | _ => raise Impossible
      in
         S.union (fs, vars)
      end
    | _ => raise Impossible

end

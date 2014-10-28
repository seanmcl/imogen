
(* See notes/{simp,valid} for the algorithms here. *)

functor EntailsFn (val logic : Logic.t) : sig
   val f : Util.entails
end = struct
        structure C = CFormula

        open General
        open PP.Ops

        structure Graph :> sig
           type t
           include Printable where type printable = t
           val init: t
           val extend: t -> (Term.t * Term.t) -> t
           val add: t * Term.t -> t
           val hasEdge: t -> (Term.t * Term.t) -> bool
           val worlds : t -> Term.t list
           val cformula : CFormula.t -> t
           val mem : t * Term.t -> bool
        end = struct
           structure M = Term.Map
           structure S = Term.Set
           val axioms = Logic.axioms logic
           val refl = List.mem (Logic.Axiom.M, axioms)
           val sym = List.mem (Logic.Axiom.B, axioms)
           val trans = List.mem (Logic.Axiom.A4, axioms)

           datatype t = T of
              { wlds : Term.set
              , kids : Term.set Term.map
              , pnts : Term.set Term.map }
           type printable = t

           fun pp (T {wlds, kids, pnts} : printable) =
              &[ %[$"wlds: ", Term.Set.pp wlds]
               , %[$"kids: ", Term.Map.ppHoriz (fn (t, s) => PP.pair (Term.pp t, Term.Set.pp s)) kids]
               , %[$"pnts: ", Term.Map.ppHoriz (fn (t, s) => PP.pair (Term.pp t, Term.Set.pp s)) pnts]]

           fun add (T {wlds, kids, pnts}, w) : t =
              let
                 val wlds = S.add (wlds, w)
                 val s = if refl then S.singleton w else S.empty
                 val cfn = fn
                    NONE => SOME s
                  | SOME s' => SOME (S.union (s, s'))
                 val kids = M.change cfn (kids, w)
                 val pnts = M.change cfn (pnts, w)
              in
                 T { wlds = wlds, kids = kids, pnts = pnts }
              end

           val empty =
              T { wlds = S.empty, kids = M.empty, pnts = M.empty }

           val init = add (empty, Term.Fn (Func.Modal.init, []))
           fun worlds (T { wlds, ...}) = S.toList wlds
           fun mem (T {wlds, ...}, w) = S.mem (wlds, w)
           fun descendants (T {kids, ...}, w) = M.findExn (kids, w)
           fun ancestors (T {pnts, ...}, w) = M.findExn (pnts, w)

           val extend =
              let
                 fun ext1 ((w1, w2), T {wlds, kids, pnts}) =
                    let
                       fun cfn w = fn
                          NONE => raise Impossible
                        | SOME s => SOME (S.add (s, w))
                       val kids = M.change (cfn w2) (kids, w1)
                       val pnts = M.change (cfn w1) (pnts, w2)
                    in
                       T { wlds = wlds, kids = kids, pnts = pnts }
                    end
                 fun ext t (w1, w2) =
                    let
                       val t = add (t, w1)
                       val t = add (t, w2)
                       fun edges (w1, w2) =
                          if trans then
                             let
                                val ps = w1 :: S.toList (ancestors (t, w1))
                                val ks = w2 :: S.toList (descendants (t, w2))
                             in
                                List.allPairs Fun.id (ps, ks)
                             end
                          else [(w1, w2)]
                       val es1 = edges (w1, w2)
                       val es2 = if sym then edges (w2, w1) else []
                       val g = foldl ext1 t (es1 @ es2)
                    in
                       (* Log.trace (fn () => *)
                       (*    &[ %[$"extend by ", PP.pair (Term.pp w1, Term.pp w2)] *)
                       (*     , %[\\, &[pp t, pp g]]]); *)
                       g
                    end
              in
                 ext
              end

           fun cformula c =
              let
                 fun ffn (f, g) = case f of
                    C.Atom r =>
                    let in
                       case Rel.dest r of
                          (p, [l, r]) =>
                          if Pred.eq (p, Pred.Modal.le) then extend g (l, r) else
                          failwith "bad atom in graph"
                        | _ => failwith "bad atom or args in graph"
                    end
                  | C.Top => g
                  | _ => failwith "bad branch in graph"
              in
                 foldl ffn init (C.conjuncts c)
              end

           fun hasEdge (g as T {kids, ...}) (w1, w2) =
              mem (g, w1) andalso mem (g, w2) andalso
              S.mem (M.findExn (kids, w1), w2)

           val () = noWarnUnused (pp, mem)
        end
        structure G = Graph

        val inconsistent = fn
           C.Bot => true
         | _ => false

        val valid =
           let
              open C.Export
              fun destAtm r = case Rel.dest r of
                 (p, [w, w']) =>
                 if Pred.eq (p, Pred.Modal.le) then
                    (w, w')
                 else failwith "Illegal pred in constraint"
               | _ => raise Impossible
              fun v g = fn
                 Top => true
               | Bot => false
               | And (t1, t2) => v g t1 andalso v g t2
               | Ex ((x, _), t) =>
                 List.exists (fn w => v g (C.apply1 (t, (x, w)))) (G.worlds g)
               | Hole => failwith "Valid: constraint has a hole."
               | Atom r => G.hasEdge g (destAtm r)
               | All ((w, _), t) => v (G.add (g, Term.Var w)) t
               | Imp (r, t) =>
                 let
                    val (w1, w2) = destAtm r
                 in
                    v (G.extend g (w1, w2)) t
                 end
           in
              v
           end

        (* CR: This is inadequate to say the least. *)
        val f =
           let
              open C.Export
              fun destAtm r = case Rel.dest r of
                 (p, [w, w']) =>
                 if Pred.eq (p, Pred.Modal.le) then
                    (w, w')
                 else failwith "Illegal pred in constraint"
               | _ => raise Impossible
              fun e (f1, g) f2 =
                 case f1 of
                    Top => valid g f2
                  | Atom r => valid (G.extend g (destAtm r)) f2
                  | _ =>
                    case f2 of
                       Top => true
                     | Bot => inconsistent f1
                     | And (t1, t2) => e (f1, g) t1 andalso e (f1, g) t2
                     | Ex ((x, _), t) =>
                       List.exists (fn w => e (f1, g) (C.apply1 (t, (x, w)))) (G.worlds g)
                     | Hole => failwith "Entails: constraint has a hole."
                     | Atom r => G.hasEdge g (destAtm r)
                     | Imp (r, t) => e (f1, G.extend g (destAtm r)) t
                     | All ((x, _), t) => e (f1, G.add (g, Term.Var x)) t
           in
              fn {entailer, entailed, global} =>
                 let
                    val g = Graph.cformula global
                    val _ = Log.trace (fn () =>
                       &[ $"Entails"
                        , %[\\, &[ %[$"entailer: ", C.pp entailer]
                                 , %[$"entailed: ", C.pp entailed]
                                 , %[$"global  : ", C.pp global]
                                 , %[$"graph   : ", Graph.pp g]]]])
                    val res = e (entailer, g) entailed
                 in
                    Log.trace (fn () =>
                       &[ $"Entails"
                        , %[\\, &[ %[$"entailer: ", C.pp entailer]
                                 , %[$"entailed: ", C.pp entailed]
                                 , %[$"global  : ", C.pp global]
                                 , %[$"graph   : ", Graph.pp g]
                                 , %[$"res     : ", PP.bool res]]]]);
                    res
                 end
           end

     end


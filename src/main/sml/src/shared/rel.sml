
structure Rel :> Rel = struct
   open General
   open PP.Ops

   type t = Pred.t * Term.t list
   type collectable = t
   type comparable = t
   type showable = t
   type fixable = t
   type printable = t
   type parseable = t
   type atoms = Atoms.t

   val zero = (Pred.zero, [])
   val one = (Pred.one, [])
   val top = (Pred.top, [])

   fun isConstr (p, _) = Pred.isConstr p

   val parse = fn
      Parse.Rel.R (p, ts) =>
      let
         val ts = map Term.parse ts
      in
         (Pred.parse p, ts)
      end

   fun paramSubst ((p, ts), av) = (p, map (fn t => Term.paramSubst (t, av)) ts)

   val ofString = parse o Parse.Rel.ofString

   fun sign (p, _) = Pred.sign p

   fun make r = r

   val dest = Fun.id

   val pred = fst

   fun funcs (_, ts) = Func.Set.unions (map Term.funcs ts)

   fun toString ((r, ts) : showable) =
      Pred.toString r
         ^ "(" ^ String.concat (List.separate ", " (map Term.toString ts)) ^ ")"

   val compare = Order.lexOrder Pred.compare (Order.listOrder Term.compare)

   fun pp ((p, ts) : printable) =
      if Pred.isInfix p
      then case ts of
              [x, y] => %[ Term.pp x, \, $(Pred.toString p), \, Term.pp y ]
            | _ => let in
               PP.ppl (&[ $"Infix relation with wrong number of arguments!"
                        , Pred.pp p
                        , %%(map Term.pp ts)
                        ])
             ; failwith "Rel.pp"
            end
      else
         %[ Pred.pp p,
           if length ts = 0 then PP.empty else
           PP.paren (%(PP.punctuate ($", ") (map Term.pp ts)))]

   structure C =
      CollectableFn
         ( type t = collectable
          val compare = compare
          val pp = pp )
   open C

   structure C =
      ComparableFn
         ( type t = comparable
          val compare = compare )
   open C

   fun fix (p, ts) : fixable = (p, map Term.fix ts)
   fun fix' xps (p, ts) = (p, map (Term.fix' xps) ts)
   fun unfix (p, ts) = (p, map Term.unfix ts)

   fun freeze (p, ts) = (p, map Term.freeze ts)

   fun thaw fs (p, ts) = (p, map (Term.thaw fs) ts)

   fun apply1 ((r, args), tx) = (r, map (fn t => Term.apply1 (t, tx)) args)

   fun apply ((r, args), theta) = (r, map (fn t => Subst.apply (t, theta)) args)

   fun applySet (s, theta) = Set.map (fn r => apply (r, theta)) s

   val rename : t * Subst.t -> t * Subst.t =
      fn ((r, ts), theta) =>
         let
            fun foldFn (t, (ts, theta)) =
               let
                  val (t', theta') = Subst.renameTerm (t, theta)
               in
                  ((t'::ts), theta')
               end
            val (ts', theta') = foldr foldFn ([], theta) ts
         in
            ((r, ts'), theta')
         end

   val toTerm : t -> Term.t =
      fn (p, ts) =>
         let
            val f = Pred.toFunc p
         in
            Term.Fn (f, ts)
         end

   fun atoms (_, ts) = Term.atomsl ts

   fun eq' f ((p1, ts1), (p2, ts2)) =
      Pred.eq (p1, p2) andalso List.all2 f (ts1, ts2)

   val () = noWarnUnused (fn _ : parseable * atoms => eq, toString, applySet, ofString)
end

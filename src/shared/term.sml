
structure Term :> Term = struct
   structure P = Parse
   open General
   open PP.Ops

   datatype t =
      Var of Var.t
    | Param of Param.t
    | Fn of Func.t * t list

   type comparable = t
   type showable = t
   type fixable = t
   type printable = t
   type parseable = t
   type atoms = Atoms.t

   type eq = t * t
   type eqs = eq list

   val isVar = fn
      Var _ => true
    | _ => false

   val isUnfixedVar = fn
      Var x => not (Var.isFixed x)
    | _ => false

   val getVarExn = fn
      Var x => x
    | _ => failwith "getVarExn"

   val rec parse = fn
      P.Term.Var x => Var (Var.parse x)
    | P.Term.Param x => Param (Param.parse x)
    | P.Term.App (f, ts) =>
      Fn (Func.ofString (P.Func.toString f), map parse ts)

   fun ofString s : parseable = parse (P.Term.ofString s)

   val compare : comparable * comparable -> order =
      let
         val num = fn
            Var _ => 0
          | Param _ => 1
          | Fn _ => 2
         fun comp (t1, t2) =
            case Int.compare (num t1, num t2) of
               EQUAL =>
               let in
                  case (t1, t2) of
                     (Var v1, Var v2) => Var.compare (v1, v2)
                   | (Param p1, Param p2) => Param.compare (p1, p2)
                   | (Fn f1, Fn f2) => lexord (f1, f2)
                   | _ => failwith "Term.compare"
               end
             | ord => ord
         and lexord x = Order.lexOrder Func.compare (Order.listOrder comp) x
      in
         comp
      end

   val rec vars = fn
      Var v => Var.Set.singleton v
    | Param _ => Var.Set.empty
    | Fn (_, ts) => foldr Var.Set.union Var.Set.empty (map vars ts)

   val rec funcs = fn
      Fn (f, ts) => Func.Set.add (Func.Set.unions (map funcs ts), f)
    | _ => Func.Set.empty

   val rec params = fn
      Var _ => Param.Set.empty
    | Param a => Param.Set.singleton a
    | Fn (_, ts) => Param.Set.unions (map params ts)

   val rec eq = fn
      (Var v, Var v') => Var.eq (v, v')
    | (Param p, Param p') => Param.eq (p, p')
    | (Fn (f, ts), Fn (f', ts')) => Func.eq (f, f') andalso List.all2 eq (ts, ts')
    | _ => false

   val rec fix = fn
      Var v => Var (Var.fix v)
    | Param a => Param (Param.fix a)
    | Fn (f, ts) => Fn (f, map fix ts)

   fun fix' atoms t = case t of
      Var x => if Atoms.mem (atoms, Left x) then Var (Var.fix x) else t
    | Param a => if Atoms.mem (atoms, Right a) then Param (Param.fix a) else t
    | Fn (f, ts) => Fn (f, map (fix' atoms) ts)

   fun unfix t = case t of
      Var v => if Var.isFixed v then Var (Var.unfix v) else t
    | Param a => if Param.isFixed a then Param (Param.unfix a) else t
    | Fn (f, ts) => Fn (f, map unfix ts)

   val rec size = fn
      Var _ => 1
    | Param _ => 1
    | Fn (_, ts) => foldr (fn (t, acc) => size t + acc) 1 ts

   fun freeze t = case t of
      Param a => Fn (Func.ofParam a, [])
    | Var _ => t
    | Fn (f, ts) => Fn (f, map freeze ts)

   fun thaw fs t = case t of
      Fn (f, []) =>
      let in
         if Func.Set.mem (fs, f)
         then Param (Func.toParam f)
         else t
      end
    | Fn (f, ts) => Fn (f, map (thaw fs) ts)
    | _ => t

   fun apply1 (t', (x, t)) = case t' of
      Var x' => if Var.eq (x, x') then t else t'
    | Param _ => t'
    | Fn (f, ts) => Fn (f, map (fn t' => apply1 (t', (x, t))) ts)

   fun paramSubst (t, (a, x)) = case t of
      Var _ => t
    | Param a' => if Param.eq (a, a') then Var x else t
    | Fn (f, ts) => Fn (f, map (fn t => paramSubst (t, (a, x))) ts)

   (* ----------------------------------------------------------------------- *)
   (*  Printing                                                               *)
   (* ----------------------------------------------------------------------- *)

   val rec toString : showable -> string = fn
      Var v => Var.toString v
    | Param a => Param.toString a
    | Fn (f, ts) =>
      String.concat
         ([ Func.toString f, "(" ]
          @ List.separate ", " (map toString ts)
             @ [")"])

   val rec isNat : t -> bool = fn
      Fn (f, [n]) => Func.toString f = "succ" andalso isNat n
    | Fn (f, []) => Func.toString f = "0"
    | Var _ => true
    | _ => false

   (* Assumes a nat *)
   val ppNat =
      let
         fun ppNat k = fn
            Fn (_, []) => PP.int k
          | Fn (_, [n]) => ppNat (k+1) n
          | Var v => %[PP.int k, $" + ", Var.pp v]
          | _ => failwith "Term.ppNat"
      in
         ppNat 0
      end

   (* NB: [1,2,3,X] ≠ [1,2,3|X]
      1 : 2 : 3 : X : nil
      1 : 2 : 3 : X *)

   val isNil : t -> bool = fn
      Fn (f, []) => Func.toString f = "nil"
    | _ => false

   val rec isList : t -> bool = fn
      Fn (f, [_, l]) => Func.toString f = "cons" andalso isList l
    | Fn (f, []) => Func.toString f = "nil"
    | Var _ => true
    | _ => false

   val rec destList : t -> t list * t = fn
      Fn (_, [x, l]) => let val (xs, c) = destList l in (x :: xs, c) end
    | x => ([], x)

   val rec atoms = fn
      Var x => Atoms.singleton(Left x)
    | Param x => Atoms.singleton(Right x)
    | Fn (_, ts) =>
      foldl (fn (t, e) => Atoms.union (atoms t, e))
         Atoms.empty ts

   fun atomsl ts =
      foldl (fn (t, e) => Atoms.union(atoms t, e)) Atoms.empty ts

   val prec = fn
      Var _ => P.Func.maxPrec
    | Param _ => P.Func.maxPrec
    | Fn (f, _) =>
      case Func.fixity f of
         NONE => Func.maxPrec
       | SOME (p, _) => p

   val rec pp = fn
      Var v => Var.pp v
    | Param p => Param.pp p
    | t as Fn (f, ts) =>
      if isNat t then ppNat t else
      if isList t then ppList t else
      let
         val pf = Func.pp f
      in
         case Func.fixity f of
            NONE =>
            let in
               case ts of
                  [] => pf
                | _ => %[pf, PP.paren (%(PP.punctuate ($", ") (map pp ts)))]
            end
          | SOME (n, assoc) =>
            case ts of
               [t1, t2] =>
               let
                  val n1 = prec t1
                  val n2 = prec t2
                  val pt1 = if n1 < n
                            then PP.paren (pp t1)
                            else if n1 = n
                            then if assoc = Func.Right
                                 then PP.paren (pp t1)
                                 else pp t1
                            else pp t1
                  val pt2 = if n2 < n
                            then PP.paren (pp t2)
                            else if n2 = n
                            then if assoc = Func.Left
                                 then PP.paren (pp t2)
                                 else pp t2
                            else pp t2
               in
                  %[ pt1, \, pf, \, pt2 ]
               end
             | _ => failwith' (%%[$"Term.pp", Func.pp f])
      end

   (* Assumes a list *)
   and ppList = fn l =>
      let
         val (xs, x) = destList l
      in
         if isNil x
         then PP.bracket (%(map pp xs))
         else PP.bracket (%(PP.punctuate ($", ") (map pp xs) @ [$" | ", pp x]))
      end

   structure Key = struct
      type t = t
      val compare = compare
      val pp = pp
   end

   structure C = ComparableFn (Key)
   open C
   structure C = CollectableFn (Key)
   open C

   val () = noWarnUnused (fn _ : printable * fixable => ())
end

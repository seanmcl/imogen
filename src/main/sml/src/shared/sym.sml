
structure Sym = struct
   open General
   open PP.Ops

   type t = int

   type func = t
   type param = t
   type pred = t
   type collectable = t
   type comparable = t
   type creatable = t
   type fixable = t
   type funcable = t
   type hashable = t
   type intable = t
   type parsable = t
   type predable = t
   type showable = t
   type paramable = t
   type printable = t
   type parseable = t

   val ofInt = Fun.id
   val toInt = Fun.id
   val num = toInt
   val ofPred = Fun.id
   val toPred = Fun.id
   val ofFunc = Fun.id
   val toFunc = Fun.id
   val ofParam = Fun.id
   val toParam = Fun.id

   local
      val ctr = ref 1
   in
      fun numSyms () = !ctr
      fun next () =
         let
            val x = !ctr
         in
            Ref.incr ctr
          ; x
         end

      fun reset () = ctr := 1
   end

   fun isFixed x = x < 0

   fun fix x = if x < 0 then x else Int.~ x

   fun unfix x = if x < 0 then Int.~ x else x

   fun compare (x, y) =
      let in
         assert (fn () => not (x = Int.~ y), fn () => $"fix mismatch")
       ; Int.compare (x, y)
      end

   fun eq (x, y) =
      let in
         assert (fn () => not (x = Int.~ y), fn () => $"fix mismatch")
       ; x = y
      end

   val pp = PP.int

   val hash = Word.fromInt

   structure Table = Int.Table
   type 'a table = 'a Table.t

   structure HashSet = Int.HashSet
   type hash_set = HashSet.t
end

signature ParseArg = sig
   type t
   include Parseable where type parseable = t
   include Showable where type showable = t
   val prefix : string
   val isFixed : t -> bool
end

signature Parse = sig
   type t = Sym.t
   type parse
   include Collectable
   include Comparable
   val ofString : string -> t
   val toString : t -> string
   val parse : parse -> t
   val data : t -> parse
   val insert : t * parse -> unit
   val mem : t -> bool
   val pp : t -> PP.t
end

functor ParseFn (V : ParseArg) : Parse = struct
   open Sym
   open PP.Ops

   type parse = V.t

   val stringTable : int String.Table.t =
      String.Table.create (Parameters.Parse.symbolTableSize, NotFound)

   val intTable : V.t Int.Table.t =
      Table.create (Parameters.Parse.symbolTableSize, NotFound)

   val parseData : t -> V.t option = fn p =>
      Table.find intTable (if p > 0 then p else Int.~ p)

   fun mem n = Table.inDomain intTable n

   fun data t = case Int.Table.find intTable t of
      NONE => failwith "no data"
    | SOME d => d

   fun insert (t, p) =
      case Table.find intTable t of
         SOME _ => failwith "ins"
       | NONE =>
         let
            fun freshen (s, n) =
               let
                  val s = case n of NONE => s | SOME n => s ^ Int.toString n
                  val n = case n of NONE => SOME 0 | SOME n => SOME (n+1)
               in
                  if String.Table.inDomain stringTable s
                  then freshen (s, n)
                  else s
               end
            val name = freshen (V.toString p, NONE)
         in
            String.Table.insertExn stringTable (name, t)
          ; Table.insertExn intTable (t, p)
         end

   fun parse p =
      let
         val name = V.toString p
         fun get t = if V.isFixed p then fix t else t
      in
         case String.Table.find stringTable name of
            SOME t => get t
          | NONE =>
            let
               val t = next ()
            in
               insert (t, p)
             ; get t
            end
      end

   val ofString = parse o V.ofString

   fun toString n =
      let
         val fixed = n < 0
         val n' = if fixed then Int.~ n else n
         val suffix = if fixed then "#" else ""
      in
         case Int.Table.find intTable n' of
            SOME s => V.toString s ^ suffix
          | NONE => V.prefix ^ Int.toString n' ^ suffix
      end

   val pp = $ o toString

   fun reset () =
      let in
         Sym.reset ()
       ; String.Table.clear stringTable
       ; Int.Table.clear intTable
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
end

structure Param :> sig
   include Param
   include Intable where type intable = t
end = struct
   open Sym
   structure P =
      ParseFn (struct
                  val prefix = "@a"
                  open Parse.Param
               end)
   open P
   fun invariant (s, {fresh}) =
      let
         fun afn p =
            if isFixed p then true else
            not (Set.mem (fresh, fix p))
      in
         assert (fn () => Set.all isFixed fresh, fn () => $"fresh is unfixed");
         assert (fn () => Set.all afn s, fn () => $"unfixed fresh param")
      end
end

structure Var :> sig
   include Var where type param = Param.t
   include Intable where type intable = t
end = struct
   open Sym
   structure P =
      ParseFn (struct
                  open Parse.Var
                  val prefix = "x"
               end)
   open P
   type param = Param.t
   val toParam = Param.ofInt o toInt
   val ofParam = ofInt o Param.toInt
end

structure imogen.Atom :> imogen.Atom
   where type Var.t = Var.t
     and type Param.t = Param.t = struct
   open General
   structure Var = Var
   structure Param = Param
   structure Base = struct
      type t = (Var.t, Param.t) Either.t
      type printable = t
      type showable = t
      type collectable = t
      type comparable = t
      type eqable = t
      type fixable = t
      val compare = Either.compare Var.compare Param.compare
      fun eq (k1, k2) = compare (k1, k2) = EQUAL
      fun map fx fa = fn
         Left x => fx x
       | Right a => fa a
      val fix = map (Left o Var.fix) (Right o Param.fix)
      val unfix = map (Left o Var.unfix) (Right o Param.unfix)
      val pp = map Var.pp Param.pp
      val toString = map Var.toString Param.toString
   end
   open Base
   structure C = ComparableFn(Base)
   open C
   structure C = CollectableFn(open Base open C)
   open C
end

structure Func : sig
   include Func where type param = Param.t
   include Intable where type intable = t
   structure P : Parse
end = struct
   open Sym
   structure Func = Parse.Func
   structure P =
      ParseFn (struct
                  val prefix = "f"
                  fun isFixed _ = false
                  open Func
               end)
   open P

   type prec = Func.prec
   val maxPrec = Func.maxPrec

   datatype assoc = Left | Right

   type param = Param.t
   val toParam = Param.ofInt o toInt
   val ofParam = ofInt o Param.toInt

   structure Modal = struct
      val init = P.parse (Func.Id "e")
      val star = P.parse (Func.Id Unicode.star)
   end

   structure Linear = struct
      val eps = P.parse (Func.Id Unicode.epsilon)
      val times = P.parse (Func.Id "*")
   end

   structure Ordered = struct
      val eps = P.parse (Func.Id "e")
      val times = P.parse (Func.Id "*")
      val inj = P.parse (Func.Id "i")
   end

   fun fixity f = case P.toString f of
      "*" => SOME (5, Right)
    | s => NONE

end

structure Pred :> sig
   include Pred where type func = Func.t
   include Intable where type intable = t
end = struct
   structure D = Domain
   structure U = Unicode
   open Sym
   structure Pred = Parse.Pred
   structure P =
      ParseFn (struct
                  val prefix = "p"
                  fun isFixed _ = false
                  open Pred
               end)
   open P
   type func = Func.t
   fun ofFunc f = ofInt (Func.toInt f)
   (* val toFunc = Func.ofInt o toInt *)

   (* The predicate symbols that are turned into function symbols
      should have the same name as the predicate.  *)
   fun toFunc p =
      let
         val n = toInt p
         val f = Func.ofInt n
      in
         if P.mem n then
            let
               val d = P.data n
               val name = Pred.toString d
               val d = Parse.Func.Id name
            in
               if Func.P.mem n then () else Func.P.insert (n, d)
             ; f
            end
         else f
      end

   datatype sign = datatype Pred.sign

   fun lift f p = f (P.data p)

   val sign = lift Pred.sign
   val pos = lift Pred.pos
   val neg = lift Pred.neg

   fun isInfix p =
      let
         val s = toString p
      in
         List.mem (s, ["=", U.equiv, U.circf, U.circw, U.circa, U.stimes, U.circdot, "*"])
      end

   val zero = parse (Pred.PosId "0")
   val one = parse (Pred.PosId "1")
   val top = parse (Pred.NegId Unicode.top)

   structure Modal = struct
      val le = parse (Pred.PosId Unicode.le)
      val atw = parse (Pred.PosId Unicode.circw)
      val atf = parse (Pred.PosId Unicode.circf)
      val patom = parse (Pred.PosId "patom")
      val natom = parse (Pred.PosId "natom")
      val here = parse (Pred.PosId "h")
      val poss = parse (Pred.PosId "poss")
   end

   structure Linear = struct
      val star = parse (Pred.PosId Unicode.stimes)
      val atw = parse (Pred.PosId Unicode.circa)
      val atf = parse (Pred.PosId Unicode.circb)
   end

   structure Ordered = struct
      val star = parse (Pred.PosId Unicode.stimes)
      val atw = parse (Pred.PosId Unicode.circw)
      val atf = parse (Pred.PosId Unicode.circf)
   end

   fun classify p =
      if eq (p, Modal.le) then D.Modal
      else D.I

   fun inDomain p = not (Domain.eq (classify p, D.I))

   val ueq = parse (Pred.PosId Unicode.equiv)

   val constrs =
      [ Modal.le ]
   fun isConstr p = List.mem (p, constrs)
end

structure Label :> Label where type pred = Pred.t = struct
   open Sym
   structure P =
      ParseFn (struct
                  val prefix = "H"
                  fun isFixed _ = false
                  open String
               end)
   open P
   val bug = 0
   fun toString n = if n = bug then "BUG" else "H" ^ Int.toString n
   type pred = Pred.t
   val toPred = Pred.ofInt o toInt
   val ofPred = ofInt o Pred.toInt
end

structure Var   : Var   = Var
structure Param : Param = Param
structure imogen.Atom  : imogen.Atom  = imogen.Atom
structure Func  : Func  = Func
structure Pred  : Pred  = Pred
structure Label : Label = Label


structure Types = struct
   open General
   open PP.Ops

   structure Var = struct
      structure Base = struct
         datatype t = V of { id : string, fixed : bool }
         type showable = t
         fun id (V { id, ... }) = id
         fun hash t = String.hash (id t)
         val eq = op=
         fun isFixed (V { fixed, ... }) = fixed
         fun toString (V { id, ... } : showable) = id
         val pp = $ o toString
      end
      open Base
      structure C = HashableFn(Base)
      open C
   end

   structure Param = struct
      structure Base = struct
         datatype t = P of { id : string, fixed : bool }
         type showable = t
         fun id (P { id, ... }) = id
         fun hash t = String.hash (id t)
         val eq = op=
         fun isFixed (P { fixed, ... }) = fixed
         fun toString (P { id, ... } : showable) = id
         val pp = $ o toString
      end
      open Base
      structure C = HashableFn(Base)
      open C
   end

   structure Func = struct
      structure Base = struct
         datatype t = Id of string
         type showable = t
         type prec = int
         val maxPrec = 10
         val eq = op=
         fun hash (Id s) = String.hash s
         val toString = fn
            (Id s) => s
         val pp = $ o toString
      end
      open Base
      structure C = HashableFn(Base)
      open C
      val () = noWarnUnused (fn _ : showable * prec => (Left))
   end

   structure Pred = struct
      structure Base = struct
         datatype t = PosId of string | NegId of string
         type printable = t
         datatype sign = Pos | Neg
         val sign = fn
            NegId _ => Neg
          | _ => Pos
         fun pos t = sign t = Pos
         fun neg t = sign t = Neg
         type showable = t
         val eq = op=
         val hash = fn
            PosId s => Word.+(String.hash s, 0w100)
          | NegId s => Word.+(String.hash s, 0w100)
         val toString = fn
            PosId s => s
          | NegId s => s
         val pp = $ o toString
      end
      open Base
      structure C = HashableFn(Base)
      open C
      val () = noWarnUnused (fn _ : printable * showable => ())
   end

   structure Prec = struct
      val Atom = 90
      val Not = 80
      val And = 70
      val Or  = 60
      val Imp = 50
      val Iff = 40
      val Quant = 30
      val Label = 20
   end

   structure Binop = struct
      datatype t = And | With | Or | Tensor | Iff | Imp | Imp' | Lolli | Lolli'
                 | BiLolli | OrdImp1 | OrdImp2

      val pp = fn
         And => $"And"
       | Tensor => $"Tensor"
       | With => $"With"
       | Imp => $"Imp"
       | Imp' => $"Imp'"
       | Or => $"Or"
       | Iff => $"Iff"
       | Lolli => $"Lolli"
       | Lolli' => $"Lolli'"
       | BiLolli => $"BiLolli"
       | OrdImp1 => $"OrdImp1"
       | OrdImp2 => $"OrdImp2"
   end

   structure Unop = struct
      datatype t = Not | Up | Down | Bang | Box | Dia | UBang

      val pp = fn
         Not => $"Not"
       | Up => $"Up"
       | Down => $"Down"
       | Bang => $"Bang"
       | Box => $"Box"
       | Dia => $"Dia"
       | UBang => $"UBang"
   end

   structure Quant = struct
      datatype t = All | Ex

      val pp = fn
         All => $"All"
       | Ex => $"Ex"
   end

   structure Term = struct
      datatype t = Var of Var.t | Param of Param.t | App of Func.t * t list
      type printable = t
      val rec pp = fn
         Var x => Var.pp x
       | Param x => Param.pp x
       | App (f, []) => Func.pp f
       | App (f, ts) =>
         %[Func.pp f, PP.paren (%(PP.punctuate (%[PP.comma, \ ]) (map pp ts)))]
      val () = noWarnUnused (fn _ : printable => ())
   end

   structure Rel = struct
      datatype t = R of Pred.t * Term.t list
      type printable = t

      fun toTerm (R (p, ts)) =
         case (p, ts) of
            (Pred.PosId s, _) => Term.App (Func.Id s, ts)
          | (Pred.NegId s, []) => Term.Var (Var.V { id = s, fixed = false })
          | (Pred.NegId s, _) => Term.App (Func.Id s, ts)

      val pp = fn
         R (p, []) => Pred.pp p
       | R (p, ts) =>
         %[Pred.pp p, PP.paren (%(PP.punctuate PP.comma (map Term.pp ts)))]
      val () = noWarnUnused (fn _ : printable => ())
   end

   structure Sort = struct
      datatype t = I | Int

      val pp = fn
         I => $"I"
       | Int => $"Int"
   end

   structure Const = struct
      datatype t = True | False | Top | One | Zero
      val pp = fn
         True => $"True"
       | False => $"False"
       | One => $"One"
       | Zero => $"Zero"
       | Top => $"Top"
   end

   structure Formula = struct
      datatype t =
         Binop of Binop.t * t * t
       | Unop of Unop.t * t
       | Quant of Quant.t * (string * Sort.t option) * t
       | Rel of Rel.t
       | Const of Const.t
       | Label of string * t
      type printable = t

      val rec pp = fn
         Binop (oper, f1, f2) => %[Binop.pp oper, PP.pair (pp f1, pp f2)]
       | Unop (oper, f) => %[Unop.pp oper, PP.paren (pp f)]
       | Quant (q, (x, t), f) =>
         let in
            case t of
               NONE => %[Quant.pp q, PP.pair ($x, pp f)]
             | SOME t => %[Quant.pp q, PP.paren (%[$x, Sort.pp t, pp f])]
         end
       | Rel r => Rel.pp r
       | Const c => Const.pp c
       | Label (s, f) => %[$"Label", PP.pair ($s, pp f)]
      val () = noWarnUnused (fn _ : printable => ())
   end

   structure Subst = struct
      datatype one = Var of Var.t * Term.t | Param of Param.t * Param.t
      type t = one list
   end

   structure Ctx = struct
      type t = (Var.t * Sort.t) list
   end

   structure Seq = struct
      datatype t = T of
         { ctx : Ctx.t option
         , constr : Formula.t option
         , ants : Rel.t list
         , cons : Rel.t option }
   end

   structure RSeq = struct
      type t = Seq.t * Subst.t
   end

   structure Rule = struct
      datatype t = T of
         { ctx : Ctx.t option
         , constr : Formula.t option
         , fresh : Param.t list
         , hyps : Seq.t list
         , conc : Seq.t }
   end

   structure Parse = struct
      datatype t =
         Var of Var.t
       | Param of Param.t
       | Func of Func.t
       | Term of Term.t
       | Pred of Pred.t
       | Rel of Rel.t
       | Form of Formula.t
       | Subst of Subst.t
       | Seq of Seq.t
       | RSeq of RSeq.t
       | Rule of Rule.t
   end

end

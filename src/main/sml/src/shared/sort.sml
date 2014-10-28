
structure Sort :> Sort = struct
   structure U = Unicode
   structure P = Parse
   open PP.Ops

   structure Base = struct
      datatype t = I | MWorld | LWorld | LHead | OWorld | OHead
      type printable = t
      type showable = t
      type eqable = t
      val toString = fn
         I => U.iota
       | MWorld => "w"
       | LWorld => "l"
       | LHead => "h"
       | OWorld => "o"
       | OHead => "h"
      val toInt = fn
         I => 1
       | MWorld => 2
       | LWorld => 3
       | LHead => 4
       | OWorld => 5
       | OHead => 6
      val hash = Word.fromInt o toInt
      val pp = $ o toString
      val eq = op=
      val parse = fn
         NONE => I
       | SOME P.Sort.I => I
       | _ => I
      val isInd = fn
         I => true
       | _ => false
      structure Key = struct
         type t = t
         val eq = eq
         val pp = pp
         val hash = hash
      end
      structure H = HashableFn (Key)
      open H
   end
   datatype t = datatype Base.t

   structure Func = struct
      type t = Base.t list * Base.t
      type printable = t
      type eqable = t
      val eq = op=
      fun pp (xs, x) =
         case xs of
            [] => Base.pp x
          | _ =>
            %(PP.punctuate ($" * ") (map Base.pp xs) @ [$" -> ", Base.pp x])
   end

   structure Pred = struct
      type t = Base.t list
      type printable = t
      type eqable = t
      val eq = op=
      fun pp xs =
         case xs of
            [] => $"o"
          | _ => %(PP.punctuate ($" * ") (map Base.pp xs) @ [$" -> o"])
   end

end

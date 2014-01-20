
structure Tokens :> Tokens = struct
   structure P = Parsing

   infixr 4 << >>
   infixr 3 &&
   infix  2 -- ## wth suchthat return guard when
   infixr 1 ||
   open P.Ops

   val isSymbolic = Char.contains "%&#+-/:<=>@\\~`|*.$!^?"
   val isBrack = Char.contains "(){}[]"
   val isSep = Char.contains ","

   fun isIdChar c =
      Char.isAlpha c orelse Char.isDigit c orelse Char.contains "_'" c

   type 'a parser = ('a, char) P.t

   val space : unit parser = P.repeat (P.satisfy Char.isSpace) return ()

   val id : string parser =
      P.satisfy Char.isAlpha
         && P.repeat (P.satisfy isIdChar) wth implode o op::

   (* val id : string parser = *)
   (*    P.repeat1 (P.satisfy isIdChar) wth implode *)

   val int : string parser = P.repeat1 (P.satisfy Char.isDigit) wth implode

   val sym : string parser =
      P.repeat1 (P.satisfy isSymbolic) wth implode
         || P.satisfy isBrack wth Char.toString
         || P.satisfy isSep wth Char.toString

   val tok = space >> P.alt [ id, int, sym ]

   val gen : char Stream.t -> string list = fn s =>
      let
         val stm = Pos.markStream s
      in
         Stream.toList (P.transform tok stm)
      end

   fun lex s = gen (Stream.stringStream s)
end

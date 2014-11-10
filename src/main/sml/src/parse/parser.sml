
structure imogen.Parser : imogen.Parser = struct
   open General

   structure T = Types

   structure LrVals : Imogen_LRVALS =
      ImogenLrValsFun (structure Token = LrParser.Token)

   structure Tokens = LrVals.Tokens

   structure Lexer : LEXER =
      ImogenLexFun (structure Tokens = Tokens)

   structure imogen.Parser =
      Join ( structure LrParser = LrParser
   structure ParserData = LrVals.ParserData
   structure Lex = Lexer )

   structure Gen = struct
      fun parse (dummyToken, lookahead, reader : int -> string) : T.Parse.t =
         let
            val _ = Interface.init_line ()
            val empty = !Interface.line
            val dummyEOF = Tokens.EOF (empty,empty)
            val dummyTOKEN = dummyToken (empty,empty)
            fun invoke lexer =
               let
                  val newLexer = imogen.Parser.Stream.cons (dummyTOKEN, lexer)
               in
                  imogen.Parser.parse ( lookahead, newLexer, Interface.error
                               , Interface.nothing )
               end
            fun loop lexer =
               let
                  val (result, lexer) = invoke lexer
                  val (nextToken, _) = imogen.Parser.Stream.get lexer
               in
                  if imogen.Parser.sameToken (nextToken,dummyEOF)
                  then result
                  else loop lexer
               end
         in
            loop (imogen.Parser.makeLexer reader)
         end

      fun file dummy f =
         let
            val dev = TextIO.openIn f
         in
            parse (dummy, 15, fn i => TextIO.inputN (dev, i))
            before TextIO.closeIn dev
         end

      fun string dummy s =
         let
            fun stringReader s =
               let val next = ref s
               in fn _ => !next before next := ""
               end
         in
            parse (dummy, 15, stringReader s)
         end

      fun stdin dummy =
         parse (dummy, 15, fn i => TextIO.inputN (TextIO.stdIn, i))

      fun parsers (get, dummy) =
         ( get o (file dummy)
         , get o (string dummy)
         , fn () => get (stdin dummy) )
   end

   structure Var = struct
      val get = fn
         T.Parse.Var p => p
       | _ => raise (Fail "parse")
      val dummy = Tokens.PARSE_VAR
      val (_, ofString, _) = Gen.parsers (get, dummy)
   end

   structure Param = struct
      val get = fn
         T.Parse.Param p => p
       | _ => raise (Fail "parse")
      val dummy = Tokens.PARSE_PARAM
      val (_, ofString, _) = Gen.parsers (get, dummy)
   end

   structure Func = struct
      val get = fn
         T.Parse.Func p => p
       | _ => raise (Fail "parse")
      val dummy = Tokens.PARSE_FUNC
      val (_, ofString, _) = Gen.parsers (get, dummy)
   end

   structure Term = struct
      val get = fn
         T.Parse.Term mf => mf
       | _ => raise (Fail "parse")
      val dummy = Tokens.PARSE_TERM
      val (_, ofString, _) = Gen.parsers (get, dummy)
   end

   structure Pred = struct
      val get = fn
         T.Parse.Pred p => p
       | _ => raise (Fail "parse")
      val dummy = Tokens.PARSE_PRED
      val (_, ofString, _) = Gen.parsers (get, dummy)
   end

   structure Rel = struct
      val get = fn
         T.Parse.Rel r => r
       | _ => raise (Fail "parse")
      val dummy = Tokens.PARSE_REL
      val (_, ofString, _) = Gen.parsers (get, dummy)
   end

   structure imogen.Formula = struct
      val get = fn
         T.Parse.Form mf => mf
       | _ => raise (Fail "parse")
      val dummy = Tokens.PARSE_FORMULA
      val (ofFile, ofString, ofStdin) = Gen.parsers (get, dummy)
   end

   structure Subst = struct
      val get = fn
         T.Parse.Subst t => t
       | _ => raise (Fail "parse")
      val dummy = Tokens.PARSE_SUBST
      val (_, ofString, _) = Gen.parsers (get, dummy)
   end

   structure Seq = struct
      val get = fn
         T.Parse.Seq t => t
       | _ => raise (Fail "parse")
      val dummy = Tokens.PARSE_SEQ
      val (_, ofString, _) = Gen.parsers (get, dummy)
   end

   structure RSeq = struct
      val get = fn
         T.Parse.RSeq t => t
       | _ => raise (Fail "parse")
      val dummy = Tokens.PARSE_RSEQ
      val (_, ofString, _) = Gen.parsers (get, dummy)
   end

   structure Rule = struct
      val get = fn
         T.Parse.Rule t => t
       | _ => raise (Fail "parse")
      val dummy = Tokens.PARSE_RULE
      val (_, ofString, _) = Gen.parsers (get, dummy)
   end

   val _ = (Seq.ofString, RSeq.ofString, Rule.ofString)

end

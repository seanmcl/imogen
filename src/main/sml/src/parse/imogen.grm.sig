signature Imogen_TOKENS =
sig
type ('a,'b) token
type svalue
val LINE:  'a * 'a -> (svalue,'a) token
val VDASH:  'a * 'a -> (svalue,'a) token
val SORT_INT:  'a * 'a -> (svalue,'a) token
val DIA:  'a * 'a -> (svalue,'a) token
val BOX:  'a * 'a -> (svalue,'a) token
val UBANG:  'a * 'a -> (svalue,'a) token
val ORD_IMP2:  'a * 'a -> (svalue,'a) token
val ORD_IMP1:  'a * 'a -> (svalue,'a) token
val AMP:  'a * 'a -> (svalue,'a) token
val BANG:  'a * 'a -> (svalue,'a) token
val OTIMES:  'a * 'a -> (svalue,'a) token
val OPLUS:  'a * 'a -> (svalue,'a) token
val BI_LOLLI:  'a * 'a -> (svalue,'a) token
val LEFT_LOLLI:  'a * 'a -> (svalue,'a) token
val LOLLI:  'a * 'a -> (svalue,'a) token
val UP_SHIFT:  'a * 'a -> (svalue,'a) token
val DOWN_SHIFT:  'a * 'a -> (svalue,'a) token
val INT: (string) *  'a * 'a -> (svalue,'a) token
val GE:  'a * 'a -> (svalue,'a) token
val LE:  'a * 'a -> (svalue,'a) token
val GT:  'a * 'a -> (svalue,'a) token
val LT:  'a * 'a -> (svalue,'a) token
val TIMES:  'a * 'a -> (svalue,'a) token
val MINUS:  'a * 'a -> (svalue,'a) token
val PLUS:  'a * 'a -> (svalue,'a) token
val EXISTS:  'a * 'a -> (svalue,'a) token
val FORALL:  'a * 'a -> (svalue,'a) token
val EQ:  'a * 'a -> (svalue,'a) token
val IFF:  'a * 'a -> (svalue,'a) token
val LEFT_IMP:  'a * 'a -> (svalue,'a) token
val IMP:  'a * 'a -> (svalue,'a) token
val NOT:  'a * 'a -> (svalue,'a) token
val OR:  'a * 'a -> (svalue,'a) token
val AND:  'a * 'a -> (svalue,'a) token
val L_ZERO:  'a * 'a -> (svalue,'a) token
val L_ONE:  'a * 'a -> (svalue,'a) token
val TOP:  'a * 'a -> (svalue,'a) token
val FALSE:  'a * 'a -> (svalue,'a) token
val TRUE:  'a * 'a -> (svalue,'a) token
val COLONCOLON:  'a * 'a -> (svalue,'a) token
val SEMICOLON:  'a * 'a -> (svalue,'a) token
val COLON:  'a * 'a -> (svalue,'a) token
val DOT:  'a * 'a -> (svalue,'a) token
val COMMA:  'a * 'a -> (svalue,'a) token
val RBRACK:  'a * 'a -> (svalue,'a) token
val LBRACK:  'a * 'a -> (svalue,'a) token
val RBRACE:  'a * 'a -> (svalue,'a) token
val LBRACE:  'a * 'a -> (svalue,'a) token
val RPAREN:  'a * 'a -> (svalue,'a) token
val LPAREN:  'a * 'a -> (svalue,'a) token
val HASH:  'a * 'a -> (svalue,'a) token
val FUNC_POS_PRED_ID: (string) *  'a * 'a -> (svalue,'a) token
val PARAM_ID: (string) *  'a * 'a -> (svalue,'a) token
val VAR_NEG_PRED_ID: (string) *  'a * 'a -> (svalue,'a) token
val UNOP_PREC:  'a * 'a -> (svalue,'a) token
val EOF:  'a * 'a -> (svalue,'a) token
val PARSE_RULE:  'a * 'a -> (svalue,'a) token
val PARSE_RSEQ:  'a * 'a -> (svalue,'a) token
val PARSE_SEQ:  'a * 'a -> (svalue,'a) token
val PARSE_SUBST:  'a * 'a -> (svalue,'a) token
val PARSE_FORMULA:  'a * 'a -> (svalue,'a) token
val PARSE_REL:  'a * 'a -> (svalue,'a) token
val PARSE_PRED:  'a * 'a -> (svalue,'a) token
val PARSE_TERM:  'a * 'a -> (svalue,'a) token
val PARSE_FUNC:  'a * 'a -> (svalue,'a) token
val PARSE_PARAM:  'a * 'a -> (svalue,'a) token
val PARSE_VAR:  'a * 'a -> (svalue,'a) token
end
signature Imogen_LRVALS=
sig
structure Tokens : Imogen_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end

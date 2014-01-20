structure T = Tokens
structure I = Interface

type pos = I.pos
type svalue = T.svalue
type ('a,'b) token = ('a,'b) T.token
type lexresult= (svalue,pos) token

val error = I.error
val line = I.line
val eof = fn () => T.EOF (!line, !line)
val next_line = I.next_line

val debug = false
fun dprint s = if debug then print (s ^ "\n") else ()

%%

%header (functor ImogenLexFun (structure Tokens: Imogen_TOKENS) : LEXER);
%full

%s COMMENT DONE;

ws=[\t\ ]*;
lc=[_a-z];
uc=[A-Z'];
int=[0-9]+;
num={int}(\.{int})?;
idchars={lc}|{uc}|[0-9'];
var_neg_pred_id={uc}{idchars}*;
param_id=@{lc}{idchars}*;
func_pos_pred_id={lc}{idchars}*;
line=--------+;
%%

<INITIAL>{ws}               => ( lex () );

<INITIAL>"("                => ( T.LPAREN (!line, !line) );
<INITIAL>")"                => ( T.RPAREN (!line, !line) );
<INITIAL>"{"                => ( T.LBRACE (!line, !line) );
<INITIAL>"}"                => ( T.RBRACE (!line, !line) );
<INITIAL>"["                => ( T.LBRACK (!line, !line) );
<INITIAL>"]"                => ( T.RBRACK (!line, !line) );
<INITIAL>","                => ( T.COMMA (!line, !line) );
<INITIAL>"."                => ( T.DOT (!line, !line) );
<INITIAL>"::"               => ( T.COLONCOLON (!line, !line) );
<INITIAL>":"                => ( T.COLON (!line, !line) );
<INITIAL>";"                => ( T.SEMICOLON (!line, !line) );
<INITIAL>"|-"               => ( T.VDASH (!line, !line) );
<INITIAL>{line}             => ( T.LINE (!line, !line) );

<INITIAL>"#"                => ( T.HASH (!line, !line) );
<INITIAL>"true"             => ( T.TRUE (!line, !line) );
<INITIAL>"top"              => ( T.TOP (!line, !line) );
<INITIAL>"⊤"                => ( T.TOP (!line, !line) );
<INITIAL>"false"            => ( T.FALSE (!line, !line) );
<INITIAL>"⊥"                => ( T.FALSE (!line, !line) );
<INITIAL>"0"                => ( T.L_ZERO (!line, !line) );
<INITIAL>"1"                => ( T.L_ONE (!line, !line) );
<INITIAL>"∧"                => ( T.AND (!line, !line) );
<INITIAL>"|"                => ( T.OR (!line, !line) );
<INITIAL>"∨"                => ( T.OR (!line, !line) );
<INITIAL>"~"                => ( T.NOT (!line, !line) );
<INITIAL>"¬"                => ( T.NOT (!line, !line) );
<INITIAL>"->"               => ( T.IMP (!line, !line) );
<INITIAL>"→"                => ( T.IMP (!line, !line) );
<INITIAL>"⊃"                => ( T.IMP (!line, !line) );
<INITIAL>"=>"               => ( T.IMP (!line, !line) );
<INITIAL>"==>"              => ( T.IMP (!line, !line) );
<INITIAL>"⇒"                => ( T.IMP (!line, !line) );
<INITIAL>"<-"               => ( T.LEFT_IMP (!line, !line) );
<INITIAL>"<=="              => ( T.LEFT_IMP (!line, !line) );
<INITIAL>"←"                => ( T.LEFT_IMP (!line, !line) );
<INITIAL>"⊂"                => ( T.LEFT_IMP (!line, !line) );
<INITIAL>"⇐"                => ( T.LEFT_IMP (!line, !line) );
<INITIAL>"<=>"              => ( T.IFF (!line, !line) );
<INITIAL>"<->"              => ( T.IFF (!line, !line) );
<INITIAL>"⇔"                => ( T.IFF (!line, !line) );
<INITIAL>"="                => ( T.EQ (!line, !line) );

<INITIAL>"forall"           => ( T.FORALL (!line, !line) );
<INITIAL>"∀"                => ( T.FORALL (!line, !line) );
<INITIAL>"exists"           => ( T.EXISTS (!line, !line) );
<INITIAL>"∃"                => ( T.EXISTS (!line, !line) );
<INITIAL>"∃"                => ( T.EXISTS (!line, !line) );

<INITIAL>"<"                => ( T.LT (!line, !line) );
<INITIAL>">"                => ( T.GT (!line, !line) );
<INITIAL>"<="               => ( T.LE (!line, !line) );
<INITIAL>"≤"                => ( T.LE (!line, !line) );
<INITIAL>">="               => ( T.GE (!line, !line) );
<INITIAL>"≥"                => ( T.GE (!line, !line) );

<INITIAL>"$"                => ( T.DOWN_SHIFT (!line, !line) );
<INITIAL>"↓"                => ( T.DOWN_SHIFT (!line, !line) );
<INITIAL>"^"                => ( T.UP_SHIFT (!line, !line) );
<INITIAL>"↑"                => ( T.UP_SHIFT (!line, !line) );

<INITIAL>"-o"               => ( T.LOLLI (!line, !line) );
<INITIAL>"⊸"                => ( T.LOLLI (!line, !line) );
<INITIAL>"o-"               => ( T.LEFT_LOLLI (!line, !line) );
<INITIAL>"o-o"              => ( T.BI_LOLLI (!line, !line) );

<INITIAL>"&"                => ( T.AMP (!line, !line) );
<INITIAL>"<+>"              => ( T.OPLUS (!line, !line) );
<INITIAL>"⊕"                => ( T.OPLUS (!line, !line) );
<INITIAL>"*"                => ( T.OTIMES (!line, !line) );
<INITIAL>"<*>"              => ( T.OTIMES (!line, !line) );
<INITIAL>"⊗"                => ( T.OTIMES (!line, !line) );

<INITIAL>">->"              => ( T.ORD_IMP1 (!line, !line) );
<INITIAL>"↣"                => ( T.ORD_IMP1 (!line, !line) );
<INITIAL>"->>"              => ( T.ORD_IMP2 (!line, !line) );
<INITIAL>"↠"                => ( T.ORD_IMP2 (!line, !line) );

<INITIAL>"box"              => ( T.BOX (!line, !line) );
<INITIAL>"□"                => ( T.BOX (!line, !line) );
<INITIAL>"dia"              => ( T.DIA (!line, !line) );
<INITIAL>"◇"                => ( T.DIA (!line, !line) );

<INITIAL>"int"              => ( T.SORT_INT (!line, !line) );

<INITIAL>{param_id}         => ( T.PARAM_ID (yytext, !line, !line) );
<INITIAL>{var_neg_pred_id}  => ( T.VAR_NEG_PRED_ID (yytext, !line, !line) );
<INITIAL>{func_pos_pred_id} => ( T.FUNC_POS_PRED_ID (yytext, !line, !line) );
<INITIAL>"\226\156\182"     => ( T.FUNC_POS_PRED_ID (yytext, !line, !line) );

<INITIAL>"%\."              => ( YYBEGIN DONE; lex () );
<INITIAL>"%"                => ( YYBEGIN COMMENT; lex () );

<INITIAL>\n                 => ( next_line (); lex () );
<COMMENT>\n                 => ( dprint "shift init"; YYBEGIN INITIAL; next_line (); lex () );

<COMMENT>.                  => ( lex () );
<DONE>.                     => ( lex () );
<DONE>\n                    => ( lex () );
<INITIAL>.                  => ( error ("ignoring illegal character" ^ yytext, !line, !line ); lex () );

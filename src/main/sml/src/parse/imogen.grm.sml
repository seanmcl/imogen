functor ImogenLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Imogen_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
(*#line 1.2 "imogen.grm"*)
structure T = Types
val unicode_le = "\226\137\164" (* ≤ : u2264 *)


(*#line 16.1 "imogen.grm.sml"*)
end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\013\000\002\000\012\000\003\000\011\000\004\000\010\000\
\\005\000\009\000\006\000\008\000\007\000\007\000\008\000\006\000\
\\009\000\005\000\010\000\004\000\011\000\003\000\000\000\
\\001\000\012\000\000\000\000\000\
\\001\000\014\000\022\000\016\000\021\000\000\000\
\\001\000\014\000\022\000\016\000\021\000\020\000\020\000\066\000\212\000\
\\067\000\163\000\000\000\
\\001\000\014\000\022\000\016\000\021\000\025\000\098\000\000\000\
\\001\000\014\000\022\000\016\000\048\000\018\000\047\000\029\000\046\000\
\\030\000\045\000\031\000\044\000\032\000\043\000\033\000\042\000\
\\036\000\041\000\041\000\040\000\042\000\039\000\051\000\038\000\
\\052\000\037\000\058\000\036\000\062\000\035\000\063\000\034\000\
\\064\000\033\000\000\000\
\\001\000\014\000\057\000\000\000\
\\001\000\014\000\057\000\015\000\056\000\000\000\
\\001\000\014\000\057\000\015\000\056\000\016\000\055\000\000\000\
\\001\000\014\000\057\000\015\000\056\000\016\000\055\000\019\000\103\000\000\000\
\\001\000\014\000\057\000\015\000\056\000\021\000\073\000\000\000\
\\001\000\014\000\077\000\000\000\
\\001\000\015\000\056\000\000\000\
\\001\000\016\000\055\000\000\000\
\\001\000\019\000\124\000\034\000\089\000\035\000\088\000\037\000\087\000\
\\038\000\086\000\039\000\085\000\053\000\084\000\054\000\083\000\
\\055\000\082\000\057\000\081\000\059\000\080\000\060\000\079\000\
\\061\000\078\000\000\000\
\\001\000\019\000\129\000\000\000\
\\001\000\019\000\138\000\000\000\
\\001\000\020\000\027\000\000\000\
\\001\000\021\000\104\000\034\000\089\000\035\000\088\000\037\000\087\000\
\\038\000\086\000\039\000\085\000\053\000\084\000\054\000\083\000\
\\055\000\082\000\057\000\081\000\059\000\080\000\060\000\079\000\
\\061\000\078\000\000\000\
\\001\000\021\000\107\000\000\000\
\\001\000\022\000\095\000\000\000\
\\001\000\023\000\139\000\000\000\
\\001\000\025\000\109\000\000\000\
\\001\000\037\000\105\000\000\000\
\\001\000\037\000\106\000\000\000\
\\001\000\065\000\137\000\000\000\
\\001\000\066\000\063\000\000\000\
\\001\000\066\000\142\000\000\000\
\\001\000\067\000\061\000\000\000\
\\147\000\000\000\
\\148\000\000\000\
\\149\000\000\000\
\\150\000\000\000\
\\151\000\000\000\
\\152\000\040\000\064\000\000\000\
\\153\000\034\000\089\000\035\000\088\000\037\000\087\000\038\000\086\000\
\\039\000\085\000\053\000\084\000\054\000\083\000\055\000\082\000\
\\057\000\081\000\059\000\080\000\060\000\079\000\061\000\078\000\000\000\
\\154\000\000\000\
\\155\000\000\000\
\\156\000\000\000\
\\157\000\000\000\
\\158\000\000\000\
\\159\000\040\000\064\000\000\000\
\\160\000\000\000\
\\161\000\000\000\
\\162\000\000\000\
\\164\000\000\000\
\\165\000\000\000\
\\166\000\000\000\
\\167\000\000\000\
\\168\000\000\000\
\\169\000\000\000\
\\170\000\024\000\108\000\000\000\
\\171\000\000\000\
\\172\000\000\000\
\\173\000\034\000\089\000\035\000\088\000\037\000\087\000\038\000\086\000\
\\039\000\085\000\053\000\084\000\054\000\083\000\055\000\082\000\
\\057\000\081\000\059\000\080\000\060\000\079\000\061\000\078\000\000\000\
\\174\000\034\000\089\000\057\000\081\000\059\000\080\000\000\000\
\\175\000\034\000\089\000\057\000\081\000\059\000\080\000\000\000\
\\176\000\034\000\089\000\057\000\081\000\059\000\080\000\000\000\
\\177\000\034\000\089\000\035\000\088\000\057\000\081\000\059\000\080\000\000\000\
\\178\000\034\000\089\000\035\000\088\000\037\000\087\000\038\000\086\000\
\\053\000\084\000\054\000\083\000\057\000\081\000\059\000\080\000\
\\060\000\079\000\061\000\078\000\000\000\
\\179\000\034\000\089\000\035\000\088\000\037\000\087\000\038\000\086\000\
\\053\000\084\000\054\000\083\000\057\000\081\000\059\000\080\000\
\\060\000\079\000\061\000\078\000\000\000\
\\180\000\034\000\089\000\035\000\088\000\037\000\087\000\038\000\086\000\
\\053\000\084\000\054\000\083\000\057\000\081\000\059\000\080\000\
\\060\000\079\000\061\000\078\000\000\000\
\\181\000\034\000\089\000\035\000\088\000\037\000\087\000\038\000\086\000\
\\053\000\084\000\054\000\083\000\057\000\081\000\059\000\080\000\
\\060\000\079\000\061\000\078\000\000\000\
\\182\000\034\000\089\000\035\000\088\000\057\000\081\000\059\000\080\000\000\000\
\\183\000\034\000\089\000\035\000\088\000\057\000\081\000\059\000\080\000\000\000\
\\184\000\034\000\089\000\035\000\088\000\037\000\087\000\038\000\086\000\
\\039\000\085\000\053\000\084\000\054\000\083\000\055\000\082\000\
\\057\000\081\000\059\000\080\000\060\000\079\000\061\000\078\000\000\000\
\\185\000\034\000\089\000\035\000\088\000\037\000\087\000\038\000\086\000\
\\039\000\085\000\053\000\084\000\054\000\083\000\055\000\082\000\
\\057\000\081\000\059\000\080\000\060\000\079\000\061\000\078\000\000\000\
\\186\000\034\000\089\000\035\000\088\000\037\000\087\000\038\000\086\000\
\\039\000\085\000\053\000\084\000\054\000\083\000\055\000\082\000\
\\057\000\081\000\059\000\080\000\060\000\079\000\061\000\078\000\000\000\
\\187\000\000\000\
\\188\000\040\000\064\000\000\000\
\\189\000\000\000\
\\190\000\000\000\
\\191\000\000\000\
\\192\000\000\000\
\\193\000\000\000\
\\194\000\000\000\
\\195\000\000\000\
\\196\000\000\000\
\\197\000\000\000\
\\198\000\000\000\
\\199\000\000\000\
\\200\000\000\000\
\\201\000\000\000\
\\202\000\000\000\
\\203\000\000\000\
\\204\000\000\000\
\\204\000\028\000\091\000\000\000\
\\205\000\000\000\
\\206\000\018\000\066\000\000\000\
\\207\000\000\000\
\\208\000\000\000\
\\209\000\000\000\
\\210\000\024\000\065\000\040\000\064\000\000\000\
\\211\000\000\000\
\\212\000\014\000\022\000\016\000\021\000\000\000\
\\212\000\014\000\022\000\016\000\021\000\020\000\020\000\000\000\
\\213\000\000\000\
\\214\000\000\000\
\\215\000\000\000\
\\216\000\018\000\092\000\000\000\
\\217\000\000\000\
\\218\000\024\000\130\000\000\000\
\\219\000\000\000\
\\220\000\000\000\
\\221\000\017\000\094\000\000\000\
\\222\000\000\000\
\\223\000\017\000\093\000\000\000\
\\224\000\024\000\140\000\000\000\
\\225\000\000\000\
\\226\000\015\000\056\000\000\000\
\\227\000\026\000\111\000\000\000\
\\228\000\000\000\
\\229\000\014\000\077\000\000\000\
\\230\000\000\000\
\\231\000\000\000\
\"
val actionRowNumbers =
"\000\000\095\000\095\000\095\000\
\\017\000\005\000\002\000\002\000\
\\008\000\013\000\012\000\006\000\
\\028\000\003\000\039\000\026\000\
\\092\000\088\000\005\000\085\000\
\\087\000\017\000\038\000\037\000\
\\036\000\010\000\005\000\069\000\
\\011\000\035\000\053\000\084\000\
\\083\000\082\000\081\000\079\000\
\\080\000\077\000\076\000\078\000\
\\075\000\074\000\073\000\072\000\
\\071\000\005\000\086\000\034\000\
\\033\000\097\000\032\000\098\000\
\\099\000\096\000\106\000\104\000\
\\031\000\030\000\029\000\020\000\
\\045\000\004\000\008\000\094\000\
\\009\000\018\000\044\000\023\000\
\\024\000\019\000\051\000\048\000\
\\068\000\022\000\112\000\110\000\
\\005\000\005\000\005\000\005\000\
\\005\000\005\000\005\000\005\000\
\\005\000\005\000\005\000\005\000\
\\014\000\005\000\008\000\105\000\
\\103\000\109\000\041\000\042\000\
\\040\000\091\000\093\000\015\000\
\\101\000\089\000\094\000\008\000\
\\012\000\047\000\007\000\005\000\
\\113\000\025\000\062\000\061\000\
\\056\000\057\000\066\000\064\000\
\\060\000\065\000\063\000\059\000\
\\058\000\055\000\070\000\054\000\
\\016\000\021\000\107\000\090\000\
\\008\000\027\000\049\000\050\000\
\\052\000\067\000\111\000\114\000\
\\100\000\095\000\109\000\102\000\
\\004\000\046\000\108\000\043\000\
\\001\000"
val gotoT =
"\
\\001\000\144\000\000\000\
\\011\000\017\000\013\000\016\000\014\000\015\000\015\000\014\000\
\\017\000\013\000\018\000\012\000\000\000\
\\011\000\017\000\013\000\016\000\014\000\015\000\016\000\022\000\
\\017\000\021\000\000\000\
\\011\000\017\000\013\000\016\000\014\000\015\000\017\000\023\000\000\000\
\\020\000\024\000\000\000\
\\005\000\030\000\006\000\029\000\011\000\017\000\012\000\028\000\
\\013\000\027\000\023\000\026\000\000\000\
\\011\000\017\000\013\000\047\000\000\000\
\\011\000\048\000\000\000\
\\007\000\052\000\009\000\051\000\021\000\050\000\024\000\049\000\000\000\
\\007\000\056\000\000\000\
\\009\000\057\000\000\000\
\\024\000\058\000\000\000\
\\000\000\
\\011\000\017\000\013\000\016\000\014\000\015\000\017\000\013\000\
\\018\000\060\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\030\000\006\000\065\000\011\000\017\000\012\000\028\000\
\\013\000\027\000\023\000\026\000\000\000\
\\000\000\
\\000\000\
\\020\000\066\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\070\000\003\000\069\000\009\000\068\000\024\000\067\000\000\000\
\\005\000\030\000\006\000\072\000\011\000\017\000\012\000\028\000\
\\013\000\027\000\023\000\026\000\000\000\
\\000\000\
\\025\000\074\000\026\000\073\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\030\000\006\000\088\000\011\000\017\000\012\000\028\000\
\\013\000\027\000\023\000\026\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\095\000\011\000\017\000\013\000\094\000\000\000\
\\007\000\052\000\009\000\051\000\021\000\097\000\024\000\049\000\000\000\
\\011\000\017\000\013\000\016\000\014\000\098\000\000\000\
\\007\000\052\000\009\000\051\000\021\000\100\000\022\000\099\000\
\\024\000\049\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\025\000\074\000\026\000\108\000\000\000\
\\000\000\
\\005\000\030\000\006\000\110\000\011\000\017\000\012\000\028\000\
\\013\000\027\000\023\000\026\000\000\000\
\\005\000\030\000\006\000\111\000\011\000\017\000\012\000\028\000\
\\013\000\027\000\023\000\026\000\000\000\
\\005\000\030\000\006\000\112\000\011\000\017\000\012\000\028\000\
\\013\000\027\000\023\000\026\000\000\000\
\\005\000\030\000\006\000\113\000\011\000\017\000\012\000\028\000\
\\013\000\027\000\023\000\026\000\000\000\
\\005\000\030\000\006\000\114\000\011\000\017\000\012\000\028\000\
\\013\000\027\000\023\000\026\000\000\000\
\\005\000\030\000\006\000\115\000\011\000\017\000\012\000\028\000\
\\013\000\027\000\023\000\026\000\000\000\
\\005\000\030\000\006\000\116\000\011\000\017\000\012\000\028\000\
\\013\000\027\000\023\000\026\000\000\000\
\\005\000\030\000\006\000\117\000\011\000\017\000\012\000\028\000\
\\013\000\027\000\023\000\026\000\000\000\
\\005\000\030\000\006\000\118\000\011\000\017\000\012\000\028\000\
\\013\000\027\000\023\000\026\000\000\000\
\\005\000\030\000\006\000\119\000\011\000\017\000\012\000\028\000\
\\013\000\027\000\023\000\026\000\000\000\
\\005\000\030\000\006\000\120\000\011\000\017\000\012\000\028\000\
\\013\000\027\000\023\000\026\000\000\000\
\\005\000\030\000\006\000\121\000\011\000\017\000\012\000\028\000\
\\013\000\027\000\023\000\026\000\000\000\
\\000\000\
\\005\000\030\000\006\000\123\000\011\000\017\000\012\000\028\000\
\\013\000\027\000\023\000\026\000\000\000\
\\007\000\052\000\009\000\051\000\021\000\100\000\022\000\124\000\
\\024\000\049\000\000\000\
\\000\000\
\\000\000\
\\009\000\126\000\010\000\125\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\011\000\017\000\013\000\016\000\014\000\129\000\000\000\
\\007\000\052\000\009\000\051\000\021\000\130\000\024\000\049\000\000\000\
\\009\000\131\000\000\000\
\\000\000\
\\002\000\070\000\003\000\132\000\009\000\068\000\024\000\067\000\000\000\
\\005\000\030\000\006\000\133\000\011\000\017\000\012\000\028\000\
\\013\000\027\000\023\000\026\000\000\000\
\\000\000\
\\019\000\134\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\052\000\009\000\051\000\021\000\100\000\022\000\139\000\
\\024\000\049\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\011\000\017\000\013\000\016\000\014\000\015\000\017\000\141\000\000\000\
\\009\000\126\000\010\000\142\000\000\000\
\\000\000\
\\004\000\143\000\011\000\017\000\013\000\094\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 145
val numrules = 85
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit | INT of  (string) | FUNC_POS_PRED_ID of  (string) | PARAM_ID of  (string) | VAR_NEG_PRED_ID of  (string) | varbd_list of  ( ( string * T.Sort.t option )  list) | varbd of  (string*T.Sort.t option) | var of  (T.Var.t) | unop of  (T.Unop.t) | termlist of  (T.Term.t list) | term of  (T.Term.t) | subst of  (T.Subst.one list) | sort of  (T.Sort.t) | seqlist of  (T.Seq.t list) | seq of  (T.Seq.t) | rseq of  (T.RSeq.t) | rule of  (T.Rule.t) | rellist of  (T.Rel.t list) | rel of  (T.Rel.t) | quant of  (T.Quant.t) | pred of  (T.Pred.t) | paramlist of  (T.Param.t list) | param of  (T.Param.t) | id of  (string*{ lower:bool } ) | func of  (T.Func.t) | form of  (T.Formula.t) | const of  (T.Const.t) | cons of  (T.Rel.t option) | bindlist of  (T.Subst.one list) | bind of  (T.Subst.one) | start of  (T.Parse.t)
end
type svalue = MlyValue.svalue
type result = T.Parse.t
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn _ => false
val showTerminal =
fn (T 0) => "PARSE_VAR"
  | (T 1) => "PARSE_PARAM"
  | (T 2) => "PARSE_FUNC"
  | (T 3) => "PARSE_TERM"
  | (T 4) => "PARSE_PRED"
  | (T 5) => "PARSE_REL"
  | (T 6) => "PARSE_FORMULA"
  | (T 7) => "PARSE_SUBST"
  | (T 8) => "PARSE_SEQ"
  | (T 9) => "PARSE_RSEQ"
  | (T 10) => "PARSE_RULE"
  | (T 11) => "EOF"
  | (T 12) => "UNOP_PREC"
  | (T 13) => "VAR_NEG_PRED_ID"
  | (T 14) => "PARAM_ID"
  | (T 15) => "FUNC_POS_PRED_ID"
  | (T 16) => "HASH"
  | (T 17) => "LPAREN"
  | (T 18) => "RPAREN"
  | (T 19) => "LBRACE"
  | (T 20) => "RBRACE"
  | (T 21) => "LBRACK"
  | (T 22) => "RBRACK"
  | (T 23) => "COMMA"
  | (T 24) => "DOT"
  | (T 25) => "COLON"
  | (T 26) => "SEMICOLON"
  | (T 27) => "COLONCOLON"
  | (T 28) => "TRUE"
  | (T 29) => "FALSE"
  | (T 30) => "TOP"
  | (T 31) => "L_ONE"
  | (T 32) => "L_ZERO"
  | (T 33) => "AND"
  | (T 34) => "OR"
  | (T 35) => "NOT"
  | (T 36) => "IMP"
  | (T 37) => "LEFT_IMP"
  | (T 38) => "IFF"
  | (T 39) => "EQ"
  | (T 40) => "FORALL"
  | (T 41) => "EXISTS"
  | (T 42) => "PLUS"
  | (T 43) => "MINUS"
  | (T 44) => "TIMES"
  | (T 45) => "LT"
  | (T 46) => "GT"
  | (T 47) => "LE"
  | (T 48) => "GE"
  | (T 49) => "INT"
  | (T 50) => "DOWN_SHIFT"
  | (T 51) => "UP_SHIFT"
  | (T 52) => "LOLLI"
  | (T 53) => "LEFT_LOLLI"
  | (T 54) => "BI_LOLLI"
  | (T 55) => "OPLUS"
  | (T 56) => "OTIMES"
  | (T 57) => "BANG"
  | (T 58) => "AMP"
  | (T 59) => "ORD_IMP1"
  | (T 60) => "ORD_IMP2"
  | (T 61) => "UBANG"
  | (T 62) => "BOX"
  | (T 63) => "DIA"
  | (T 64) => "SORT_INT"
  | (T 65) => "VDASH"
  | (T 66) => "LINE"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 66) $$ (T 65) $$ (T 64) $$ (T 63) $$ (T 62) $$ (T 61) $$ (T 60) $$ (T 59) $$ (T 58) $$ (T 57) $$ (T 56) $$ (T 55) $$ (T 54) $$ (T 53) $$ (T 52) $$ (T 51) $$ (T 50) $$ (T 48) $$ (T 47) $$ (T 46) $$ (T 45) $$ (T 44) $$ (T 43) $$ (T 42) $$ (T 41) $$ (T 40) $$ (T 39) $$ (T 38) $$ (T 37) $$ (T 36) $$ (T 35) $$ (T 34) $$ (T 33) $$ (T 32) $$ (T 31) $$ (T 30) $$ (T 29) $$ (T 28) $$ (T 27) $$ (T 26) $$ (T 25) $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 3) $$ (T 2) $$ (T 1) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.var var, _, var1right)) :: ( _, ( _, PARSE_VAR1left, _)) :: rest671)) => let val  result = MlyValue.start ((*#line 132.21 "imogen.grm"*) T.Parse.Var var (*#line 542.1 "imogen.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, PARSE_VAR1left, var1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.param param, _, param1right)) :: ( _, ( _, PARSE_PARAM1left, _)) :: rest671)) => let val  result = MlyValue.start ((*#line 133.25 "imogen.grm"*) T.Parse.Param param (*#line 546.1 "imogen.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, PARSE_PARAM1left, param1right), rest671)
end
|  ( 2, ( ( _, ( MlyValue.func func, _, func1right)) :: ( _, ( _, PARSE_FUNC1left, _)) :: rest671)) => let val  result = MlyValue.start ((*#line 134.23 "imogen.grm"*) T.Parse.Func func (*#line 550.1 "imogen.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, PARSE_FUNC1left, func1right), rest671)
end
|  ( 3, ( ( _, ( MlyValue.term term, _, term1right)) :: ( _, ( _, PARSE_TERM1left, _)) :: rest671)) => let val  result = MlyValue.start ((*#line 135.23 "imogen.grm"*) T.Parse.Term term (*#line 554.1 "imogen.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, PARSE_TERM1left, term1right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.pred pred, _, pred1right)) :: ( _, ( _, PARSE_PRED1left, _)) :: rest671)) => let val  result = MlyValue.start ((*#line 136.23 "imogen.grm"*) T.Parse.Pred pred (*#line 558.1 "imogen.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, PARSE_PRED1left, pred1right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.rel rel, _, rel1right)) :: ( _, ( _, PARSE_REL1left, _)) :: rest671)) => let val  result = MlyValue.start ((*#line 137.21 "imogen.grm"*) T.Parse.Rel rel (*#line 562.1 "imogen.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, PARSE_REL1left, rel1right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.form form, _, form1right)) :: ( _, ( _, PARSE_FORMULA1left, _)) :: rest671)) => let val  result = MlyValue.start ((*#line 138.25 "imogen.grm"*) T.Parse.Form form (*#line 566.1 "imogen.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, PARSE_FORMULA1left, form1right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.subst subst, _, subst1right)) :: ( _, ( _, PARSE_SUBST1left, _)) :: rest671)) => let val  result = MlyValue.start ((*#line 139.25 "imogen.grm"*) T.Parse.Subst subst (*#line 570.1 "imogen.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, PARSE_SUBST1left, subst1right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.seq seq, _, seq1right)) :: ( _, ( _, PARSE_SEQ1left, _)) :: rest671)) => let val  result = MlyValue.start ((*#line 140.21 "imogen.grm"*) T.Parse.Seq seq (*#line 574.1 "imogen.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, PARSE_SEQ1left, seq1right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.rseq rseq, _, rseq1right)) :: ( _, ( _, PARSE_RSEQ1left, _)) :: rest671)) => let val  result = MlyValue.start ((*#line 141.23 "imogen.grm"*) T.Parse.RSeq rseq (*#line 578.1 "imogen.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, PARSE_RSEQ1left, rseq1right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.rule rule, _, rule1right)) :: ( _, ( _, PARSE_RULE1left, _)) :: rest671)) => let val  result = MlyValue.start ((*#line 142.23 "imogen.grm"*) T.Parse.Rule rule (*#line 582.1 "imogen.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, PARSE_RULE1left, rule1right), rest671)
end
|  ( 11, ( ( _, ( _, DOT1left, DOT1right)) :: rest671)) => let val  result = MlyValue.cons ((*#line 145.15 "imogen.grm"*) NONE (*#line 586.1 "imogen.grm.sml"*)
)
 in ( LrTable.NT 3, ( result, DOT1left, DOT1right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.rel rel, rel1left, rel1right)) :: rest671)) => let val  result = MlyValue.cons ((*#line 146.15 "imogen.grm"*) SOME rel (*#line 590.1 "imogen.grm.sml"*)
)
 in ( LrTable.NT 3, ( result, rel1left, rel1right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.cons cons, _, cons1right)) :: _ :: ( _, ( MlyValue.rellist rellist, rellist1left, _)) :: rest671)) => let val  result = MlyValue.seq ((*#line 149.25 "imogen.grm"*) T.Seq.T { ctx = NONE, constr = NONE, ants = rellist, cons = cons } (*#line 594.1 "imogen.grm.sml"*)
)
 in ( LrTable.NT 16, ( result, rellist1left, cons1right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.cons cons, _, cons1right)) :: _ :: ( _, ( MlyValue.rellist rellist, _, _)) :: _ :: ( _, ( MlyValue.form form, _, _)) :: ( _, ( _, LBRACE1left, _)) :: rest671)) => let val  result = MlyValue.seq ((*#line 150.44 "imogen.grm"*) T.Seq.T { ctx = NONE, constr = SOME form, ants = rellist, cons = cons } (*#line 598.1 "imogen.grm.sml"*)
)
 in ( LrTable.NT 16, ( result, LBRACE1left, cons1right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.subst subst, _, subst1right)) :: ( _, ( MlyValue.seq seq, seq1left, _)) :: rest671)) => let val  result = MlyValue.rseq ((*#line 153.29 "imogen.grm"*)seq, subst(*#line 602.1 "imogen.grm.sml"*)
)
 in ( LrTable.NT 15, ( result, seq1left, subst1right), rest671)
end
|  ( 16, ( ( _, ( MlyValue.seq seq, seq1left, seq1right)) :: rest671)) => let val  result = MlyValue.seqlist ((*#line 156.29 "imogen.grm"*) [seq] (*#line 606.1 "imogen.grm.sml"*)
)
 in ( LrTable.NT 17, ( result, seq1left, seq1right), rest671)
end
|  ( 17, ( ( _, ( MlyValue.seqlist seqlist, _, seqlist1right)) :: ( _, ( MlyValue.seq seq, seq1left, _)) :: rest671)) => let val  result = MlyValue.seqlist ((*#line 157.29 "imogen.grm"*) seq :: seqlist (*#line 610.1 "imogen.grm.sml"*)
)
 in ( LrTable.NT 17, ( result, seq1left, seqlist1right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.seq seq, _, seq1right)) :: _ :: ( _, ( MlyValue.paramlist paramlist, _, _)) :: _ :: _ :: ( _, ( MlyValue.seqlist seqlist, seqlist1left, _)) :: rest671)) => let val  result = MlyValue.rule ((*#line 161.11 "imogen.grm"*) T.Rule.T { ctx = NONE, constr = NONE, fresh = paramlist
                    , hyps = seqlist, conc = seq } (*#line 614.1 "imogen.grm.sml"*)
)
 in ( LrTable.NT 14, ( result, seqlist1left, seq1right), rest671)
end
|  ( 19, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.bindlist bindlist, _, _)) :: ( _, ( _, LBRACE1left, _)) :: rest671)) => let val  result = MlyValue.subst ((*#line 165.39 "imogen.grm"*) bindlist (*#line 619.1 "imogen.grm.sml"*)
)
 in ( LrTable.NT 19, ( result, LBRACE1left, RBRACE1right), rest671)
end
|  ( 20, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( _, LBRACE1left, _)) :: rest671)) => let val  result = MlyValue.subst ((*#line 166.39 "imogen.grm"*) [] (*#line 623.1 "imogen.grm.sml"*)
)
 in ( LrTable.NT 19, ( result, LBRACE1left, RBRACE1right), rest671)
end
|  ( 21, ( ( _, ( MlyValue.term term, _, term1right)) :: _ :: ( _, ( MlyValue.var var, var1left, _)) :: rest671)) => let val  result = MlyValue.bind ((*#line 169.39 "imogen.grm"*) T.Subst.Var (var, term) (*#line 627.1 "imogen.grm.sml"*)
)
 in ( LrTable.NT 1, ( result, var1left, term1right), rest671)
end
|  ( 22, ( ( _, ( MlyValue.param param2, _, param2right)) :: _ :: ( _, ( MlyValue.param param1, param1left, _)) :: rest671)) => let val  result = MlyValue.bind ((*#line 170.39 "imogen.grm"*) T.Subst.Param (param1, param2) (*#line 631.1 "imogen.grm.sml"*)
)
 in ( LrTable.NT 1, ( result, param1left, param2right), rest671)
end
|  ( 23, ( ( _, ( MlyValue.bind bind, bind1left, bind1right)) :: rest671)) => let val  result = MlyValue.bindlist ((*#line 173.39 "imogen.grm"*) [bind] (*#line 635.1 "imogen.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, bind1left, bind1right), rest671)
end
|  ( 24, ( ( _, ( MlyValue.bindlist bindlist, _, bindlist1right)) :: _ :: ( _, ( MlyValue.bind bind, bind1left, _)) :: rest671)) => let val  result = MlyValue.bindlist ((*#line 174.39 "imogen.grm"*) bind :: bindlist (*#line 639.1 "imogen.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, bind1left, bindlist1right), rest671)
end
|  ( 25, ( ( _, ( MlyValue.const const, const1left, const1right)) :: rest671)) => let val  result = MlyValue.form ((*#line 177.39 "imogen.grm"*) T.Formula.Const const (*#line 643.1 "imogen.grm.sml"*)
)
 in ( LrTable.NT 5, ( result, const1left, const1right), rest671)
end
|  ( 26, ( ( _, ( MlyValue.form form, _, form1right)) :: _ :: ( _, ( MlyValue.FUNC_POS_PRED_ID FUNC_POS_PRED_ID, FUNC_POS_PRED_ID1left, _)) :: rest671)) => let val  result = MlyValue.form ((*#line 178.39 "imogen.grm"*) T.Formula.Label (FUNC_POS_PRED_ID, form)(*#line 647.1 "imogen.grm.sml"*)
)
 in ( LrTable.NT 5, ( result, FUNC_POS_PRED_ID1left, form1right), rest671)
end
|  ( 27, ( ( _, ( MlyValue.form form2, _, form2right)) :: _ :: ( _, ( MlyValue.form form1, form1left, _)) :: rest671)) => let val  result = MlyValue.form ((*#line 179.39 "imogen.grm"*) T.Formula.Binop (T.Binop.And, form1, form2) (*#line 651.1 "imogen.grm.sml"*)
)
 in ( LrTable.NT 5, ( result, form1left, form2right), rest671)
end
|  ( 28, ( ( _, ( MlyValue.form form2, _, form2right)) :: _ :: ( _, ( MlyValue.form form1, form1left, _)) :: rest671)) => let val  result = MlyValue.form ((*#line 180.39 "imogen.grm"*) T.Formula.Binop (T.Binop.With, form1, form2) (*#line 655.1 "imogen.grm.sml"*)
)
 in ( LrTable.NT 5, ( result, form1left, form2right), rest671)
end
|  ( 29, ( ( _, ( MlyValue.form form2, _, form2right)) :: _ :: ( _, ( MlyValue.form form1, form1left, _)) :: rest671)) => let val  result = MlyValue.form ((*#line 181.39 "imogen.grm"*) T.Formula.Binop (T.Binop.Tensor, form1, form2) (*#line 659.1 "imogen.grm.sml"*)
)
 in ( LrTable.NT 5, ( result, form1left, form2right), rest671)
end
|  ( 30, ( ( _, ( MlyValue.form form2, _, form2right)) :: _ :: ( _, ( MlyValue.form form1, form1left, _)) :: rest671)) => let val  result = MlyValue.form ((*#line 182.39 "imogen.grm"*) T.Formula.Binop (T.Binop.Or, form1, form2) (*#line 663.1 "imogen.grm.sml"*)
)
 in ( LrTable.NT 5, ( result, form1left, form2right), rest671)
end
|  ( 31, ( ( _, ( MlyValue.form form2, _, form2right)) :: _ :: ( _, ( MlyValue.form form1, form1left, _)) :: rest671)) => let val  result = MlyValue.form ((*#line 183.39 "imogen.grm"*) T.Formula.Binop (T.Binop.Imp, form1, form2) (*#line 667.1 "imogen.grm.sml"*)
)
 in ( LrTable.NT 5, ( result, form1left, form2right), rest671)
end
|  ( 32, ( ( _, ( MlyValue.form form2, _, form2right)) :: _ :: ( _, ( MlyValue.form form1, form1left, _)) :: rest671)) => let val  result = MlyValue.form ((*#line 184.39 "imogen.grm"*) T.Formula.Binop (T.Binop.Lolli, form1, form2) (*#line 671.1 "imogen.grm.sml"*)
)
 in ( LrTable.NT 5, ( result, form1left, form2right), rest671)
end
|  ( 33, ( ( _, ( MlyValue.form form2, _, form2right)) :: _ :: ( _, ( MlyValue.form form1, form1left, _)) :: rest671)) => let val  result = MlyValue.form ((*#line 185.39 "imogen.grm"*) T.Formula.Binop (T.Binop.OrdImp1, form1, form2) (*#line 675.1 "imogen.grm.sml"*)
)
 in ( LrTable.NT 5, ( result, form1left, form2right), rest671)
end
|  ( 34, ( ( _, ( MlyValue.form form2, _, form2right)) :: _ :: ( _, ( MlyValue.form form1, form1left, _)) :: rest671)) => let val  result = MlyValue.form ((*#line 186.39 "imogen.grm"*) T.Formula.Binop (T.Binop.OrdImp2, form1, form2) (*#line 679.1 "imogen.grm.sml"*)
)
 in ( LrTable.NT 5, ( result, form1left, form2right), rest671)
end
|  ( 35, ( ( _, ( MlyValue.form form2, _, form2right)) :: _ :: ( _, ( MlyValue.form form1, form1left, _)) :: rest671)) => let val  result = MlyValue.form ((*#line 187.39 "imogen.grm"*) T.Formula.Binop (T.Binop.Imp, form2, form1) (*#line 683.1 "imogen.grm.sml"*)
)
 in ( LrTable.NT 5, ( result, form1left, form2right), rest671)
end
|  ( 36, ( ( _, ( MlyValue.form form2, _, form2right)) :: _ :: ( _, ( MlyValue.form form1, form1left, _)) :: rest671)) => let val  result = MlyValue.form ((*#line 188.39 "imogen.grm"*) T.Formula.Binop (T.Binop.Lolli, form2, form1) (*#line 687.1 "imogen.grm.sml"*)
)
 in ( LrTable.NT 5, ( result, form1left, form2right), rest671)
end
|  ( 37, ( ( _, ( MlyValue.form form2, _, form2right)) :: _ :: ( _, ( MlyValue.form form1, form1left, _)) :: rest671)) => let val  result = MlyValue.form ((*#line 189.39 "imogen.grm"*) T.Formula.Binop (T.Binop.Iff, form1, form2) (*#line 691.1 "imogen.grm.sml"*)
)
 in ( LrTable.NT 5, ( result, form1left, form2right), rest671)
end
|  ( 38, ( ( _, ( MlyValue.form form2, _, form2right)) :: _ :: ( _, ( MlyValue.form form1, form1left, _)) :: rest671)) => let val  result = MlyValue.form ((*#line 190.39 "imogen.grm"*) T.Formula.Binop (T.Binop.BiLolli, form1, form2) (*#line 695.1 "imogen.grm.sml"*)
)
 in ( LrTable.NT 5, ( result, form1left, form2right), rest671)
end
|  ( 39, ( ( _, ( MlyValue.form form, _, form1right)) :: _ :: ( _, ( MlyValue.varbd_list varbd_list, _, _)) :: ( _, ( MlyValue.quant quant, quant1left, _)) :: rest671)) => let val  result = MlyValue.form ((*#line 191.39 "imogen.grm"*) foldr (fn (bnd, f) => T.Formula.Quant (quant, bnd, f)) form varbd_list (*#line 699.1 "imogen.grm.sml"*)
)
 in ( LrTable.NT 5, ( result, quant1left, form1right), rest671)
end
|  ( 40, ( ( _, ( MlyValue.form form, _, form1right)) :: ( _, ( MlyValue.unop unop, unop1left, _)) :: rest671)) => let val  result = MlyValue.form ((*#line 192.39 "imogen.grm"*) T.Formula.Unop (unop, form) (*#line 703.1 "imogen.grm.sml"*)
)
 in ( LrTable.NT 5, ( result, unop1left, form1right), rest671)
end
|  ( 41, ( ( _, ( MlyValue.rel rel, rel1left, rel1right)) :: rest671)) => let val  result = MlyValue.form ((*#line 193.39 "imogen.grm"*) T.Formula.Rel rel (*#line 707.1 "imogen.grm.sml"*)
)
 in ( LrTable.NT 5, ( result, rel1left, rel1right), rest671)
end
|  ( 42, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.form form, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = MlyValue.form ((*#line 194.39 "imogen.grm"*) form (*#line 711.1 "imogen.grm.sml"*)
)
 in ( LrTable.NT 5, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 43, ( ( _, ( _, TRUE1left, TRUE1right)) :: rest671)) => let val  result = MlyValue.const ((*#line 197.39 "imogen.grm"*) T.Const.True (*#line 715.1 "imogen.grm.sml"*)
)
 in ( LrTable.NT 4, ( result, TRUE1left, TRUE1right), rest671)
end
|  ( 44, ( ( _, ( _, FALSE1left, FALSE1right)) :: rest671)) => let val  result = MlyValue.const ((*#line 198.39 "imogen.grm"*) T.Const.False (*#line 719.1 "imogen.grm.sml"*)
)
 in ( LrTable.NT 4, ( result, FALSE1left, FALSE1right), rest671)
end
|  ( 45, ( ( _, ( _, TOP1left, TOP1right)) :: rest671)) => let val  result = MlyValue.const ((*#line 199.39 "imogen.grm"*) T.Const.Top (*#line 723.1 "imogen.grm.sml"*)
)
 in ( LrTable.NT 4, ( result, TOP1left, TOP1right), rest671)
end
|  ( 46, ( ( _, ( _, L_ONE1left, L_ONE1right)) :: rest671)) => let val  result = MlyValue.const ((*#line 200.39 "imogen.grm"*) T.Const.One (*#line 727.1 "imogen.grm.sml"*)
)
 in ( LrTable.NT 4, ( result, L_ONE1left, L_ONE1right), rest671)
end
|  ( 47, ( ( _, ( _, L_ZERO1left, L_ZERO1right)) :: rest671)) => let val  result = MlyValue.const ((*#line 201.39 "imogen.grm"*) T.Const.Zero (*#line 731.1 "imogen.grm.sml"*)
)
 in ( LrTable.NT 4, ( result, L_ZERO1left, L_ZERO1right), rest671)
end
|  ( 48, ( ( _, ( _, FORALL1left, FORALL1right)) :: rest671)) => let val  result = MlyValue.quant ((*#line 204.39 "imogen.grm"*) T.Quant.All (*#line 735.1 "imogen.grm.sml"*)
)
 in ( LrTable.NT 11, ( result, FORALL1left, FORALL1right), rest671)
end
|  ( 49, ( ( _, ( _, EXISTS1left, EXISTS1right)) :: rest671)) => let val  result = MlyValue.quant ((*#line 205.39 "imogen.grm"*) T.Quant.Ex (*#line 739.1 "imogen.grm.sml"*)
)
 in ( LrTable.NT 11, ( result, EXISTS1left, EXISTS1right), rest671)
end
|  ( 50, ( ( _, ( _, NOT1left, NOT1right)) :: rest671)) => let val  result = MlyValue.unop ((*#line 208.39 "imogen.grm"*) T.Unop.Not (*#line 743.1 "imogen.grm.sml"*)
)
 in ( LrTable.NT 22, ( result, NOT1left, NOT1right), rest671)
end
|  ( 51, ( ( _, ( _, UP_SHIFT1left, UP_SHIFT1right)) :: rest671)) => let val  result = MlyValue.unop ((*#line 209.39 "imogen.grm"*) T.Unop.Up (*#line 747.1 "imogen.grm.sml"*)
)
 in ( LrTable.NT 22, ( result, UP_SHIFT1left, UP_SHIFT1right), rest671)
end
|  ( 52, ( ( _, ( _, DOWN_SHIFT1left, DOWN_SHIFT1right)) :: rest671)) => let val  result = MlyValue.unop ((*#line 210.39 "imogen.grm"*) T.Unop.Down (*#line 751.1 "imogen.grm.sml"*)
)
 in ( LrTable.NT 22, ( result, DOWN_SHIFT1left, DOWN_SHIFT1right), rest671)
end
|  ( 53, ( ( _, ( _, BANG1left, BANG1right)) :: rest671)) => let val  result = MlyValue.unop ((*#line 211.39 "imogen.grm"*) T.Unop.Bang (*#line 755.1 "imogen.grm.sml"*)
)
 in ( LrTable.NT 22, ( result, BANG1left, BANG1right), rest671)
end
|  ( 54, ( ( _, ( _, UBANG1left, UBANG1right)) :: rest671)) => let val  result = MlyValue.unop ((*#line 212.39 "imogen.grm"*) T.Unop.UBang (*#line 759.1 "imogen.grm.sml"*)
)
 in ( LrTable.NT 22, ( result, UBANG1left, UBANG1right), rest671)
end
|  ( 55, ( ( _, ( _, BOX1left, BOX1right)) :: rest671)) => let val  result = MlyValue.unop ((*#line 213.39 "imogen.grm"*) T.Unop.Box (*#line 763.1 "imogen.grm.sml"*)
)
 in ( LrTable.NT 22, ( result, BOX1left, BOX1right), rest671)
end
|  ( 56, ( ( _, ( _, DIA1left, DIA1right)) :: rest671)) => let val  result = MlyValue.unop ((*#line 214.39 "imogen.grm"*) T.Unop.Dia (*#line 767.1 "imogen.grm.sml"*)
)
 in ( LrTable.NT 22, ( result, DIA1left, DIA1right), rest671)
end
|  ( 57, ( ( _, ( MlyValue.FUNC_POS_PRED_ID FUNC_POS_PRED_ID, FUNC_POS_PRED_ID1left, FUNC_POS_PRED_ID1right)) :: rest671)) => let val  result = MlyValue.pred ((*#line 217.39 "imogen.grm"*) T.Pred.PosId FUNC_POS_PRED_ID (*#line 771.1 "imogen.grm.sml"*)
)
 in ( LrTable.NT 10, ( result, FUNC_POS_PRED_ID1left, FUNC_POS_PRED_ID1right), rest671)
end
|  ( 58, ( ( _, ( MlyValue.VAR_NEG_PRED_ID VAR_NEG_PRED_ID, VAR_NEG_PRED_ID1left, VAR_NEG_PRED_ID1right)) :: rest671)) => let val  result = MlyValue.pred ((*#line 218.39 "imogen.grm"*) T.Pred.NegId VAR_NEG_PRED_ID (*#line 775.1 "imogen.grm.sml"*)
)
 in ( LrTable.NT 10, ( result, VAR_NEG_PRED_ID1left, VAR_NEG_PRED_ID1right), rest671)
end
|  ( 59, ( ( _, ( MlyValue.pred pred, pred1left, pred1right)) :: rest671)) => let val  result = MlyValue.rel ((*#line 221.39 "imogen.grm"*) T.Rel.R (pred, []) (*#line 779.1 "imogen.grm.sml"*)
)
 in ( LrTable.NT 12, ( result, pred1left, pred1right), rest671)
end
|  ( 60, ( ( _, ( _, _, RPAREN1right)) :: _ :: ( _, ( MlyValue.pred pred, pred1left, _)) :: rest671)) => let val  result = MlyValue.rel ((*#line 222.39 "imogen.grm"*) T.Rel.R (pred, []) (*#line 783.1 "imogen.grm.sml"*)
)
 in ( LrTable.NT 12, ( result, pred1left, RPAREN1right), rest671)
end
|  ( 61, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.termlist termlist, _, _)) :: _ :: ( _, ( MlyValue.pred pred, pred1left, _)) :: rest671)) => let val  result = MlyValue.rel ((*#line 223.39 "imogen.grm"*) T.Rel.R (pred, termlist) (*#line 787.1 "imogen.grm.sml"*)
)
 in ( LrTable.NT 12, ( result, pred1left, RPAREN1right), rest671)
end
|  ( 62, ( ( _, ( MlyValue.term term, _, term1right)) :: _ :: ( _, ( MlyValue.rel rel, rel1left, _)) :: rest671)) => let val  result = MlyValue.rel ((*#line 224.39 "imogen.grm"*) T.Rel.R (T.Pred.PosId "=", [T.Rel.toTerm rel, term]) (*#line 791.1 "imogen.grm.sml"*)
)
 in ( LrTable.NT 12, ( result, rel1left, term1right), rest671)
end
|  ( 63, ( ( _, ( MlyValue.rel rel, rel1left, rel1right)) :: rest671)) => let val  result = MlyValue.rellist ((*#line 227.39 "imogen.grm"*) [rel] (*#line 795.1 "imogen.grm.sml"*)
)
 in ( LrTable.NT 13, ( result, rel1left, rel1right), rest671)
end
|  ( 64, ( ( _, ( MlyValue.rellist rellist, _, rellist1right)) :: _ :: ( _, ( MlyValue.rel rel, rel1left, _)) :: rest671)) => let val  result = MlyValue.rellist ((*#line 228.39 "imogen.grm"*) rel :: rellist (*#line 799.1 "imogen.grm.sml"*)
)
 in ( LrTable.NT 13, ( result, rel1left, rellist1right), rest671)
end
|  ( 65, ( rest671)) => let val  result = MlyValue.rellist ((*#line 229.39 "imogen.grm"*) [] (*#line 803.1 "imogen.grm.sml"*)
)
 in ( LrTable.NT 13, ( result, defaultPos, defaultPos), rest671)
end
|  ( 66, ( ( _, ( MlyValue.FUNC_POS_PRED_ID FUNC_POS_PRED_ID, FUNC_POS_PRED_ID1left, FUNC_POS_PRED_ID1right)) :: rest671)) => let val  result = MlyValue.func ((*#line 232.39 "imogen.grm"*) T.Func.Id FUNC_POS_PRED_ID (*#line 807.1 "imogen.grm.sml"*)
)
 in ( LrTable.NT 6, ( result, FUNC_POS_PRED_ID1left, FUNC_POS_PRED_ID1right), rest671)
end
|  ( 67, ( ( _, ( MlyValue.var var, var1left, var1right)) :: rest671)) => let val  result = MlyValue.term ((*#line 235.39 "imogen.grm"*) T.Term.Var var (*#line 811.1 "imogen.grm.sml"*)
)
 in ( LrTable.NT 20, ( result, var1left, var1right), rest671)
end
|  ( 68, ( ( _, ( MlyValue.param param, param1left, param1right)) :: rest671)) => let val  result = MlyValue.term ((*#line 236.39 "imogen.grm"*) T.Term.Param param (*#line 815.1 "imogen.grm.sml"*)
)
 in ( LrTable.NT 20, ( result, param1left, param1right), rest671)
end
|  ( 69, ( ( _, ( MlyValue.func func, func1left, func1right)) :: rest671)) => let val  result = MlyValue.term ((*#line 237.39 "imogen.grm"*) T.Term.App (func, []) (*#line 819.1 "imogen.grm.sml"*)
)
 in ( LrTable.NT 20, ( result, func1left, func1right), rest671)
end
|  ( 70, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.termlist termlist, _, _)) :: _ :: ( _, ( MlyValue.func func, func1left, _)) :: rest671)) => let val  result = MlyValue.term ((*#line 238.39 "imogen.grm"*) T.Term.App (func, termlist) (*#line 823.1 "imogen.grm.sml"*)
)
 in ( LrTable.NT 20, ( result, func1left, RPAREN1right), rest671)
end
|  ( 71, ( ( _, ( MlyValue.term term, term1left, term1right)) :: rest671)) => let val  result = MlyValue.termlist ((*#line 241.39 "imogen.grm"*) [term] (*#line 827.1 "imogen.grm.sml"*)
)
 in ( LrTable.NT 21, ( result, term1left, term1right), rest671)
end
|  ( 72, ( ( _, ( MlyValue.termlist termlist, _, termlist1right)) :: _ :: ( _, ( MlyValue.term term, term1left, _)) :: rest671)) => let val  result = MlyValue.termlist ((*#line 242.39 "imogen.grm"*) term :: termlist (*#line 831.1 "imogen.grm.sml"*)
)
 in ( LrTable.NT 21, ( result, term1left, termlist1right), rest671)
end
|  ( 73, ( ( _, ( _, _, HASH1right)) :: ( _, ( MlyValue.VAR_NEG_PRED_ID VAR_NEG_PRED_ID, VAR_NEG_PRED_ID1left, _)) :: rest671)) => let val  result = MlyValue.var ((*#line 245.39 "imogen.grm"*) T.Var.V { id = VAR_NEG_PRED_ID, fixed = true } (*#line 835.1 "imogen.grm.sml"*)
)
 in ( LrTable.NT 23, ( result, VAR_NEG_PRED_ID1left, HASH1right), rest671)
end
|  ( 74, ( ( _, ( MlyValue.VAR_NEG_PRED_ID VAR_NEG_PRED_ID, VAR_NEG_PRED_ID1left, VAR_NEG_PRED_ID1right)) :: rest671)) => let val  result = MlyValue.var ((*#line 246.39 "imogen.grm"*) T.Var.V { id = VAR_NEG_PRED_ID, fixed = false} (*#line 839.1 "imogen.grm.sml"*)
)
 in ( LrTable.NT 23, ( result, VAR_NEG_PRED_ID1left, VAR_NEG_PRED_ID1right), rest671)
end
|  ( 75, ( ( _, ( _, _, HASH1right)) :: ( _, ( MlyValue.PARAM_ID PARAM_ID, PARAM_ID1left, _)) :: rest671)) => let val  result = MlyValue.param ((*#line 249.39 "imogen.grm"*) T.Param.P { id = PARAM_ID, fixed = true } (*#line 843.1 "imogen.grm.sml"*)
)
 in ( LrTable.NT 8, ( result, PARAM_ID1left, HASH1right), rest671)
end
|  ( 76, ( ( _, ( MlyValue.PARAM_ID PARAM_ID, PARAM_ID1left, PARAM_ID1right)) :: rest671)) => let val  result = MlyValue.param ((*#line 250.39 "imogen.grm"*) T.Param.P { id = PARAM_ID, fixed = false} (*#line 847.1 "imogen.grm.sml"*)
)
 in ( LrTable.NT 8, ( result, PARAM_ID1left, PARAM_ID1right), rest671)
end
|  ( 77, ( ( _, ( MlyValue.param param, param1left, param1right)) :: rest671)) => let val  result = MlyValue.paramlist ((*#line 253.39 "imogen.grm"*) [param] (*#line 851.1 "imogen.grm.sml"*)
)
 in ( LrTable.NT 9, ( result, param1left, param1right), rest671)
end
|  ( 78, ( ( _, ( MlyValue.paramlist paramlist, _, paramlist1right)) :: _ :: ( _, ( MlyValue.param param, param1left, _)) :: rest671)) => let val  result = MlyValue.paramlist ((*#line 254.39 "imogen.grm"*) param :: paramlist (*#line 855.1 "imogen.grm.sml"*)
)
 in ( LrTable.NT 9, ( result, param1left, paramlist1right), rest671)
end
|  ( 79, ( rest671)) => let val  result = MlyValue.paramlist ((*#line 255.39 "imogen.grm"*) [] (*#line 859.1 "imogen.grm.sml"*)
)
 in ( LrTable.NT 9, ( result, defaultPos, defaultPos), rest671)
end
|  ( 80, ( ( _, ( MlyValue.VAR_NEG_PRED_ID VAR_NEG_PRED_ID, VAR_NEG_PRED_ID1left, VAR_NEG_PRED_ID1right)) :: rest671)) => let val  result = MlyValue.varbd ((*#line 258.39 "imogen.grm"*) (VAR_NEG_PRED_ID, NONE) (*#line 863.1 "imogen.grm.sml"*)
)
 in ( LrTable.NT 24, ( result, VAR_NEG_PRED_ID1left, VAR_NEG_PRED_ID1right), rest671)
end
|  ( 81, ( ( _, ( MlyValue.sort sort, _, sort1right)) :: _ :: ( _, ( MlyValue.VAR_NEG_PRED_ID VAR_NEG_PRED_ID, VAR_NEG_PRED_ID1left, _)) :: rest671)) => let val  result = MlyValue.varbd ((*#line 259.39 "imogen.grm"*) (VAR_NEG_PRED_ID, SOME sort) (*#line 867.1 "imogen.grm.sml"*)
)
 in ( LrTable.NT 24, ( result, VAR_NEG_PRED_ID1left, sort1right), rest671)
end
|  ( 82, ( ( _, ( MlyValue.varbd varbd, varbd1left, varbd1right)) :: rest671)) => let val  result = MlyValue.varbd_list ((*#line 262.39 "imogen.grm"*) [varbd] (*#line 871.1 "imogen.grm.sml"*)
)
 in ( LrTable.NT 25, ( result, varbd1left, varbd1right), rest671)
end
|  ( 83, ( ( _, ( MlyValue.varbd_list varbd_list, _, varbd_list1right)) :: ( _, ( MlyValue.varbd varbd, varbd1left, _)) :: rest671)) => let val  result = MlyValue.varbd_list ((*#line 263.39 "imogen.grm"*) varbd :: varbd_list (*#line 875.1 "imogen.grm.sml"*)
)
 in ( LrTable.NT 25, ( result, varbd1left, varbd_list1right), rest671)
end
|  ( 84, ( ( _, ( _, SORT_INT1left, SORT_INT1right)) :: rest671)) => let val  result = MlyValue.sort ((*#line 266.39 "imogen.grm"*) T.Sort.Int (*#line 879.1 "imogen.grm.sml"*)
)
 in ( LrTable.NT 18, ( result, SORT_INT1left, SORT_INT1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.start x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a 
end
end
structure Tokens : Imogen_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun PARSE_VAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(ParserData.MlyValue.VOID,p1,p2))
fun PARSE_PARAM (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(ParserData.MlyValue.VOID,p1,p2))
fun PARSE_FUNC (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(ParserData.MlyValue.VOID,p1,p2))
fun PARSE_TERM (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(ParserData.MlyValue.VOID,p1,p2))
fun PARSE_PRED (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(ParserData.MlyValue.VOID,p1,p2))
fun PARSE_REL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(ParserData.MlyValue.VOID,p1,p2))
fun PARSE_FORMULA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(ParserData.MlyValue.VOID,p1,p2))
fun PARSE_SUBST (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(ParserData.MlyValue.VOID,p1,p2))
fun PARSE_SEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(ParserData.MlyValue.VOID,p1,p2))
fun PARSE_RSEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(ParserData.MlyValue.VOID,p1,p2))
fun PARSE_RULE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(ParserData.MlyValue.VOID,p1,p2))
fun UNOP_PREC (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(ParserData.MlyValue.VOID,p1,p2))
fun VAR_NEG_PRED_ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(ParserData.MlyValue.VAR_NEG_PRED_ID i,p1,p2))
fun PARAM_ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(ParserData.MlyValue.PARAM_ID i,p1,p2))
fun FUNC_POS_PRED_ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(ParserData.MlyValue.FUNC_POS_PRED_ID i,p1,p2))
fun HASH (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(ParserData.MlyValue.VOID,p1,p2))
fun LBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(ParserData.MlyValue.VOID,p1,p2))
fun RBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(ParserData.MlyValue.VOID,p1,p2))
fun LBRACK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(ParserData.MlyValue.VOID,p1,p2))
fun RBRACK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(ParserData.MlyValue.VOID,p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(ParserData.MlyValue.VOID,p1,p2))
fun DOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(ParserData.MlyValue.VOID,p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(ParserData.MlyValue.VOID,p1,p2))
fun SEMICOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(ParserData.MlyValue.VOID,p1,p2))
fun COLONCOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(ParserData.MlyValue.VOID,p1,p2))
fun TRUE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(ParserData.MlyValue.VOID,p1,p2))
fun FALSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(ParserData.MlyValue.VOID,p1,p2))
fun TOP (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(ParserData.MlyValue.VOID,p1,p2))
fun L_ONE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(ParserData.MlyValue.VOID,p1,p2))
fun L_ZERO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(ParserData.MlyValue.VOID,p1,p2))
fun NOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(ParserData.MlyValue.VOID,p1,p2))
fun IMP (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(ParserData.MlyValue.VOID,p1,p2))
fun LEFT_IMP (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(ParserData.MlyValue.VOID,p1,p2))
fun IFF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(ParserData.MlyValue.VOID,p1,p2))
fun FORALL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(ParserData.MlyValue.VOID,p1,p2))
fun EXISTS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 41,(ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 42,(ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 43,(ParserData.MlyValue.VOID,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 44,(ParserData.MlyValue.VOID,p1,p2))
fun LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 45,(ParserData.MlyValue.VOID,p1,p2))
fun GT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 46,(ParserData.MlyValue.VOID,p1,p2))
fun LE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 47,(ParserData.MlyValue.VOID,p1,p2))
fun GE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 48,(ParserData.MlyValue.VOID,p1,p2))
fun INT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 49,(ParserData.MlyValue.INT i,p1,p2))
fun DOWN_SHIFT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 50,(ParserData.MlyValue.VOID,p1,p2))
fun UP_SHIFT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 51,(ParserData.MlyValue.VOID,p1,p2))
fun LOLLI (p1,p2) = Token.TOKEN (ParserData.LrTable.T 52,(ParserData.MlyValue.VOID,p1,p2))
fun LEFT_LOLLI (p1,p2) = Token.TOKEN (ParserData.LrTable.T 53,(ParserData.MlyValue.VOID,p1,p2))
fun BI_LOLLI (p1,p2) = Token.TOKEN (ParserData.LrTable.T 54,(ParserData.MlyValue.VOID,p1,p2))
fun OPLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 55,(ParserData.MlyValue.VOID,p1,p2))
fun OTIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 56,(ParserData.MlyValue.VOID,p1,p2))
fun BANG (p1,p2) = Token.TOKEN (ParserData.LrTable.T 57,(ParserData.MlyValue.VOID,p1,p2))
fun AMP (p1,p2) = Token.TOKEN (ParserData.LrTable.T 58,(ParserData.MlyValue.VOID,p1,p2))
fun ORD_IMP1 (p1,p2) = Token.TOKEN (ParserData.LrTable.T 59,(ParserData.MlyValue.VOID,p1,p2))
fun ORD_IMP2 (p1,p2) = Token.TOKEN (ParserData.LrTable.T 60,(ParserData.MlyValue.VOID,p1,p2))
fun UBANG (p1,p2) = Token.TOKEN (ParserData.LrTable.T 61,(ParserData.MlyValue.VOID,p1,p2))
fun BOX (p1,p2) = Token.TOKEN (ParserData.LrTable.T 62,(ParserData.MlyValue.VOID,p1,p2))
fun DIA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 63,(ParserData.MlyValue.VOID,p1,p2))
fun SORT_INT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 64,(ParserData.MlyValue.VOID,p1,p2))
fun VDASH (p1,p2) = Token.TOKEN (ParserData.LrTable.T 65,(ParserData.MlyValue.VOID,p1,p2))
fun LINE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 66,(ParserData.MlyValue.VOID,p1,p2))
end
end

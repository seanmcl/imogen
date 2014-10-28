
(*
 Use the following template when using parsing functions

 local
   structure P = Parsing
   infixr 4 << >>
   infixr 3 &&
   infix  2 -- ##
   infix  2 wth suchthat return guard when
   infixr 1 || !!
 in

 end
 *)

signature Parsing = sig

   (* Parser with token type 't, result type 'a *)
   type ('a,'t) t

   (* succeed with given value *)
   val succeed : 'a -> ('a,'t) t

   (* fail immediately *)
   val fail : ('a,'t) t

   (* check for end of input *)
   val done : 'a -> ('a,'t) t

   (* admit anything, provided there's something on the input *)
   val any : ('t,'t) t

   (* sequential successful composition of parsers *)
   val -- : ('a,'t) t * ('a -> ('b,'t) t) -> ('b,'t) t

   (* sequential failing composition of parsers *)
   val ## : ('a,'t) t * (Pos.t -> ('a,'t) t) -> ('a,'t) t

   (* grab position *)
   val !! : ('a,'t) t  -> ('a * Pos.t,'t) t

   (* get position *)
   val get : (Pos.t -> ('a, 't) t) -> ('a, 't) t

   (* to handle mutually-recursive parsers *)
   val $ : (unit -> ('a,'t) t) -> ('a,'t) t

   (* to construct a recursive parser *)
   val fix : (('a,'t) t -> ('a,'t) t) -> ('a,'t) t

   (* re-parse same input, given result of first parse *)
   val lookahead : ('a,'t) t -> ('a -> ('b,'t) t) -> ('b,'t) t

   (* parse this stream before reading any other input *)
   val push : ('t * Pos.t) Stream.t -> ('a,'t) t -> ('a, 't) t

   (* parse a stream *)
   val parse : ('a,'t) t -> ('t * Pos.t) Stream.t -> 'a option

   (*
    transform p s
    parses consecutive maximal prefixes of s with p as many times
    as possible, outputting the results as a stream
    *)
   val transform : ('a,'t) t -> ('t * Pos.t) Stream.t -> 'a Stream.t

   (* sequential composition *)
   val &&       : ('a,'t) t * ('b,'t) t -> ('a * 'b,'t) t

   (* alternation *)
   val ||       : ('a,'t) t * ('a,'t) t -> ('a,'t) t

   (* apply function to success value *)
   val wth      : ('a,'t) t * ('a -> 'b) -> ('b,'t) t

   (* succeed only if check on successful is true *)
   val suchthat : ('a,'t) t * ('a -> bool) -> ('a,'t) t

   (* specify success value *)
   val return   : ('b,'t) t * 'a -> ('a,'t) t

   (* apply function to failure position *)
   val guard    : ('a,'t) t * (Pos.t -> 'b) -> ('a,'t) t

   (* n-ary sequential composition *)
   val seq      : ('a,'t) t list -> ('a list,'t) t

   (* n-ary alternation *)
   val alt      : ('a,'t) t list -> ('a,'t) t

   (* ensure that next token satisfies condition, yielding that token *)
   val satisfy  : ('t -> bool) -> ('t,'t) t

   (* succeed only if function returns SOME a *)
   val maybe    : ('t -> 'a option) -> ('a, 't) t

   (* succeed with mapped result if SOME, otherwise fail. *)
   val when     : ('a, 't) t * ('a -> 'b option) -> ('b, 't) t

   (* check for a given token *)
   val literal  : ''t -> (''t,''t) t

   (* check for a given list of tokens *)
   val string   : ''t list -> (''t list,''t) t

   (* check for one of a list of tokens *)
   val oneof    : ''t list -> (''t,''t) t

   (* optional parse, yielding an optional result *)
   val opt      : ('a,'t) t -> ('a option,'t) t

   (* optional parse, with given action on success *)
   val optional : ('a -> 'b) -> 'b -> ('a,'t) t -> ('b,'t) t

   (* zero or more copies *)
   val repeat   : ('a,'t) t -> ('a list,'t) t

   (* one or more *)
   val repeat1  : ('a,'t) t -> ('a list,'t) t

   (* exact number *)
   val repeatn  : int -> ('a, 't) t -> (unit, 't) t

   (* avoid building result *)
   val repeati  : ('a, 't) t -> (unit, 't) t

   (* parse two things, yielding value of first *)
   val first    : ('a,'t) t -> ('b,'t) t -> ('a,'t) t
   val <<       : ('a,'t) t * ('b,'t) t -> ('a,'t) t

   (* ... second *)
   val second   : ('a,'t) t -> ('b,'t) t -> ('b,'t) t
   val >>       : ('a,'t) t * ('b,'t) t -> ('b,'t) t

   (* .... middle of three *)
   val middle   : ('a,'t) t -> ('b,'t) t -> ('c,'t) t -> ('b,'t) t

   (* parse one or more, with given separator between items *)
   val separate : ('a,'t) t -> ('b,'t) t -> ('a list,'t) t

   (* ... zero or more *)
   val separate0: ('a,'t) t -> ('b,'t) t -> ('a list,'t) t

   (* one or more, allowing trailing separator *)
   val separate': ('a,'t) t -> ('b,'t) t -> ('a list,'t) t

   (* nested ts *)
   val join     : (('a,'t) t,'t) t -> ('a,'t) t

   (***** Pre/In/Post-fix utilities *****)

   datatype associativity = Left | Right | Non

   datatype 'a opr =
      Prefix of int * ('a -> 'a)
    | Infix of associativity * int * ('a * 'a -> 'a)
    | Postfix of int * ('a -> 'a)

   datatype 'a fixityitem =
      Atm of 'a
    | Opr of 'a opr

   val parsefixity : ('a fixityitem, 't) t -> ('a,'t) t

   (* Same, but also look for adjacent tokens, combining them with
    * the supplied function and associativity. *)
   val parsefixityadj
      :  ('a fixityitem, 't) t
      -> associativity
      -> ('a * 'a -> 'a)
      -> ('a,'t) t

   (**
    *  Utilities for manipulating intermediate results,
    *
    * ie. (IF >> $exp) && (THEN >> $exp) && (ELSE >> $exp) wth If o flat3
    *)
   val flat3 : 'a * ('b * 'c) -> 'a * 'b * 'c
   val flat4 : 'a * ('b * ('c * 'd)) -> 'a * 'b * 'c * 'd
   val flat5 : 'a * ('b * ('c * ('d * 'e))) -> 'a * 'b * 'c * 'd * 'e
   val flat6 : 'a * ('b * ('c * ('d * ('e * 'f)))) -> 'a * 'b * 'c * 'd * 'e * 'f

   structure Ops : sig
      val && : ('a,'t) t * ('b,'t) t -> ('a * 'b,'t) t
      val || : ('a,'t) t * ('a,'t) t -> ('a,'t) t
      val << : ('a,'t) t * ('b,'t) t -> ('a,'t) t
      val >> : ('a,'t) t * ('b,'t) t -> ('b,'t) t
      val !! : ('a,'t) t  -> ('a * Pos.t,'t) t
      val ## : ('a,'t) t * (Pos.t -> ('a,'t) t) -> ('a,'t) t
      val $ : (unit -> ('a,'t) t) -> ('a,'t) t
      val -- : ('a,'t) t * ('a -> ('b,'t) t) -> ('b,'t) t
      val wth      : ('a,'t) t * ('a -> 'b) -> ('b,'t) t
      val when     : ('a, 't) t * ('a -> 'b option) -> ('b, 't) t
      val suchthat : ('a,'t) t * ('a -> bool) -> ('a,'t) t
      val return   : ('b,'t) t * 'a -> ('a,'t) t
   end

end

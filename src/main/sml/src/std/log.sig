
signature Log = sig
   datatype level = Trace | Debug | Info | Warning | Error | Critical | Nothing
   val level     : unit -> level
   val ofString  : string -> level option
   val setLevel  : level -> unit
   val setLevel' : int -> unit
   val gte       : level * level -> bool
   val msg       : (level -> PP.t option) -> unit
   val trace     : (unit -> PP.t) -> unit
   val debug     : (unit -> PP.t) -> unit
   val info      : (unit -> PP.t) -> unit
   val warning   : (unit -> PP.t) -> unit
   val error     : (unit -> PP.t) -> unit
   val critical  : (unit -> PP.t) -> unit
end

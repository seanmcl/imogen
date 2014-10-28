(*
  There are three kinds of debugging.

  - Assertions.  Turn them off only when in competition mode.
  - Debug messages.
    Turn them off most of the time, but on when you need to see some output
    in a buggy spot.
  - Tracing.
    Generate massive amounts of output on what the code is doing.  For when
    you're totally lost as to what's going wrong

  These must not be refs, so MLton can optimize away all the calls.
*)
signature Debug = sig
   val print_error_on_failure : bool ref
   val failwith : string -> 'a
   val failwith' : PP.t -> 'a

   (*** Assertions ***)
   val assertP : bool ref
   val assert : (unit -> bool) * (unit -> PP.t) -> unit
   val asserts : (unit -> bool) * string -> unit
   val assert' : (unit -> bool) -> unit
   val uassert : (unit -> unit) -> unit

   (*** Tracing ***)

   val traceP : bool ref
   val trace : string -> ('a -> 'b) -> ('a -> 'b)
   val traceArgs
      :  string * ('a -> PP.t) * ('b -> PP.t)
      -> ('a -> 'b)
      -> ('a -> 'b)
end


(* Externally visible aspects of the lexer and parser *)

structure Interface : sig
   type pos
   val line : pos ref
   val init_line : unit -> unit
   val next_line : unit -> unit
   val error : string * pos * pos -> unit
   type arg
   val nothing : arg
end = struct
   type pos = int
   val line = ref 0
   fun init_line () = (line := 1)
   fun next_line () = (line := !line + 1)
   exception Parse
   fun error (errmsg, line, _) =
      let in
         print (String.concat ["Line ", Int.toString line, ": ", errmsg, "\n"])
       ; raise Parse
      end

   type arg = unit
   val nothing : arg = ()
end

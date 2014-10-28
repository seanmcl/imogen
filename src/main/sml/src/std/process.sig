
signature Process = sig
   include OS_PROCESS
   (* Run a program and get stdout. *)
   val run : string -> string
end


structure MLton = struct
   structure Pointer = struct
      type t = unit
   end

   val size: 'a -> int = fn _ => 0

   fun noisyRaise exn = raise exn
end

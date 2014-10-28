
structure Process : Process = struct
   open OS.Process
   local
      val seed = ref (Rand.mkRandom 0w12345 ());
   in
      val rand = fn () =>
         let in
            seed := Rand.random (!seed)
          ; Rand.range (1,100000) (!seed)
         end
   end
   fun tempfile () = "/tmp/sml-tmp-" ^ Int.toString (rand ())
   fun run cmd =
      let
         val file = tempfile ()
         val _ = ignore (system (cmd ^ " > " ^ file))
         val s = TextIO.readFile file
      in
         ignore (system ("rm -f " ^ file))
       ; s
      end
end


structure Univ :> Univ = struct
   type t = exn

   fun 'a embed () =
      let
         exception E of 'a
         val project = fn
            E a => SOME a
          | _ => NONE
      in
         (E, project)
      end
end

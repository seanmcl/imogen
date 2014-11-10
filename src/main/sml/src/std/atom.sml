
structure AtomSplaySet =
   OrdSetFn
      (struct
         type t = imogen.Atom.atom
         val compare = imogen.Atom.compare
         val pp = PP.string o imogen.Atom.toString
      end)

structure AtomSplayMap =
   OrdMapFn
      (struct
         type t = imogen.Atom.atom
         val compare = imogen.Atom.compare
         val pp = PP.string o imogen.Atom.toString
         val () = ignore (pp) (* MLton warning *)
      end)


structure AtomSplaySet =
   OrdSetFn
      (struct
         type t = Atom.atom
         val compare = Atom.compare
         val pp = PP.string o Atom.toString
      end)

structure AtomSplayMap =
   OrdMapFn
      (struct
         type t = Atom.atom
         val compare = Atom.compare
         val pp = PP.string o Atom.toString
         val () = ignore (pp) (* MLton warning *)
      end)

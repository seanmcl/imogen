
structure Signat :> Signat = struct
   structure T = Term
   structure FTab = Func.Table
   structure PTab = Pred.Table
   structure B = Sort.Base

   open General
   open PP.Ops

   datatype t = T of
      { f : (Sort.t list * Sort.t) Func.table
      , p : Sort.t list Pred.table }

   val t =
      let val exn = Fail "Signat.table problem" in
         T { f = FTab.create (Parameters.Parse.symbolTableSize, exn)
           , p = PTab.create (Parameters.Parse.symbolTableSize, exn) }
      end

   fun dest () = case t of T {f, p} => (f, p)
   fun ftab () = fst (dest ())
   fun ptab () = snd (dest ())

   fun pp () =
      let
         val fs = FTab.toListi (ftab ())
         fun ffn (f, s) = %[Func.pp f, $" : ", Sort.Func.pp s]
         val ps = PTab.toListi (ptab ())
         fun pfn (p, s) = %[Pred.pp p, $" : ", Sort.Pred.pp s]
      in
         &(map ffn fs @ map pfn ps)
      end

   structure FP = struct
      type ('a, 'b) t =
         { f : Func.t -> 'a
         , p : Pred.t -> 'b }
      type 'a func = ('a, 'a) t
      fun func {f, p=_} x = f x
      fun pred {f=_, p} x = p x
   end
   open FP

   val mem =
      { f = fn f => FTab.inDomain (ftab ()) f
      , p = fn p => PTab.inDomain (ptab ()) p }

   val find =
      { f = fn f => FTab.find (ftab ()) f
      , p = fn p => PTab.find (ptab ()) p }

   val findExn =
      { f = fn f => FTab.findExn (ftab ()) f
      , p = fn p => PTab.findExn (ptab ()) p }

   val arity =
      { f = fn f => length (fst (func findExn f))
      , p = fn p => length (pred findExn p) }

   val extend =
      let
         val (ftab, ptab) = dest ()
         fun func f s = case FTab.find ftab f of
            SOME s' => if Sort.Func.eq (s, s') then () else
            failwith' (&[ %[$"Signat.extend: func given two sorts: ", Func.pp f]
                        , %[\\, Sort.Func.pp s]
                        , %[\\, Sort.Func.pp s']])
          | NONE => FTab.insertExn ftab (f, s)
         fun pred p s = case PTab.find ptab p of
            SOME s' => if Sort.Pred.eq (s, s') then () else
            failwith' (&[ %[$"Signat.extend: pred given two sorts: ", Pred.pp p]
                        , %[\\, Sort.Pred.pp s]
                        , %[\\, Sort.Pred.pp s']])
          | NONE =>
            let in
               (* PP.ppl (%%[$"Signat.extend:", Pred.pp p, Sort.Pred.pp s]); *)
               PTab.insertExn ptab (p, s)
            end
      in
         { f = func, p = pred }
      end

   fun init () =
      let in
         pred extend Pred.Modal.le [B.MWorld, B.MWorld]
       ; pred extend Pred.Modal.atw [B.I, B.MWorld]
       ; pred extend Pred.Modal.atf [B.I, B.MWorld]
       ; pred extend Pred.Modal.patom [B.I, B.MWorld]
       ; pred extend Pred.Modal.natom [B.I, B.MWorld, B.MWorld]
       ; func extend Func.Modal.init ([], B.MWorld)
       ; func extend Func.Modal.star ([], B.MWorld)
       ; func extend Func.Linear.eps ([], B.LWorld)
       ; func extend Func.Linear.times ([B.LWorld, B.LWorld], B.LWorld)
       ; pred extend Pred.Linear.atw [B.I, B.LWorld]
       ; pred extend Pred.Linear.atf [B.I, B.LHead, B.LWorld]
      end

   val _ = init ()

   fun reset () =
      let in
         FTab.clear (ftab ())
       ; PTab.clear (ptab ())
       ; init ()
      end

end

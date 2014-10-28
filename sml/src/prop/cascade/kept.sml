
structure Kept :> Kept = struct
   open General
   open PP.Ops

   (* We maintain two distinct queues in the database, one based on
      a heuristic notion of priority, and another strict FIFO queue.
      We usually select the next sequent from the priority queue, but
      go back to the FIFO to maintain completeness. *)

   (* Priority queue *)
   structure PQ = HeapFn (struct
      type priority = int
      val compare = Int.compare
   end)

   (* Time queue *)
   structure TQ = HeapFn (struct
      type priority = int
      val compare = Int.compare
   end)

   type entry = Seq.t * PQ.hand * TQ.hand * Proof.Node.t

   datatype t = D of
      { index: entry Index.t
      , prioQueue: Id.t PQ.heap
      , timeQueue: Id.t TQ.heap
      (* Record the number of 'add's *)
      , timeClock: Clock.t
      (* Record the number of 'next's *)
      , depthClock: Clock.t
      (* Switch to time queue after depthInterval 'next's *)
      , depthInterval: int
      , maxSize : int ref }

   fun create () =
      D { index = Index.create ()
        , prioQueue = PQ.empty ()
        , timeQueue = TQ.empty ()
        , timeClock = Clock.new 1
        , depthClock = Clock.new 1
        , depthInterval = !Parameters.Db.depthInterval
        , maxSize = ref 0 }

   fun isEmpty (D {prioQueue, ...}) = PQ.size prioQueue = 0

   fun size (D {index, ...}) = Index.size index

   fun maxSize (D {maxSize, ...}) = !maxSize

   fun subsumed (D {index, ...}, seq) = Index.subsumed (index, seq)

   (* Remove sequents from the database *)
   val remove : t * Id.set -> unit = fn
      (D {index, prioQueue, timeQueue, ...}, ids) =>
      let
         fun remove1 id =
            let in
               if not (Index.member (index, id))
               then ()
               else
                  case Index.find (index, id) of
                     NONE => ()
                   | SOME (_, phand, thand, _) =>
                     let in
                        (* Remove from the index *)
                        Index.remove (index, id)
                      (* Remove from prio queue *)
                      ; PQ.delete prioQueue phand
                      (* Remove from time queue *)
                      ; TQ.delete timeQueue thand
                     end
            end
      in
         Id.Set.app remove1 ids
      end

   fun insert ( t as D {index, timeClock, prioQueue, timeQueue, maxSize, ...}
              , seq, node, prio) =
      let
         val id = Seq.id seq
         (* Insert into prioQueue.  Be sure to negate the prio, as our
            implementation is a min queue and the priorities are given in
            increasing order. *)
         val sizeHand = PQ.insert prioQueue (Int.~ prio) id
         val timeHand = TQ.insert timeQueue (Clock.time timeClock) id
      in
         Clock.tick timeClock
       ; if !Parameters.Db.useBackwardSubsumption
         then remove (t, subsumed (t, seq)) else ()
       ; Index.insert (index, Seq.id seq, seq, (seq, sizeHand, timeHand, node))
       ; if size t > !maxSize then maxSize := size t else ()
      end

   fun next (D {index, depthInterval, depthClock, prioQueue, timeQueue, ...}) =
      let in
         Clock.tick depthClock
       ; if Clock.time depthClock mod depthInterval = 0
         then
            case TQ.min timeQueue of
               NONE => raise Impossible
             | SOME (_, id) =>
               case Index.find (index, id) of
                  NONE => raise Impossible
                | SOME (seq, phand, _, node) =>
                  let in
                     (* remove from index *)
                     Index.remove (index, id)
                   (* remove from prio queue *)
                   ; PQ.delete prioQueue phand
                   ; (seq, node)
                  end
         else
            case PQ.min prioQueue of
               NONE => raise Impossible
             | SOME (_, id) =>
               case Index.find (index, id) of
                  NONE => raise Impossible
                | SOME (seq, _, thand, node) =>
                  let in
                     (* remove from index *)
                     Index.remove (index, id)
                   (* remove from time queue *)
                   ; TQ.delete timeQueue thand
                   ; (seq, node)
                  end
      end

   fun subsumes (D {index, ...}, seq) = Index.subsumes (index, seq)

   fun pp db n =
      let
         val neg = Int.~
         fun list (D {index, prioQueue, ...}) =
            let
               val (seqs, phands, _, _) = List.unzip4 (Index.toList index)
               val (prios, _) = List.unzip (map (fn h => PQ.get prioQueue h) phands)
            in
               List.zip (prios, seqs)
            end
         val list = case n of
            NONE => list db
          | SOME n => List.take' (list db, n)
         fun ppItem (prio, q) = %[Seq.pp q, $" : ", PP.paren (PP.int (neg prio))]
      in
         &(map ppItem list)
      end

end

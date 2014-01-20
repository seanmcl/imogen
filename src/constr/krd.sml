
structure KRD :> KRD = struct
   open General
   open PP.Ops

   structure Index = RuleIndex
   structure Id = RuleId

   (* Min priority queue *)
   structure PQ = HeapFn(struct
                            type priority = int
                            val compare = Int.compare
                         end)

   (* Time queue *)
   structure TQ = HeapFn(struct
                            type priority = int
                            val compare = Int.compare
                         end)

   type entry = Rule.t * PQ.hand * TQ.hand * PDB.Node.t

   datatype t = D of
      { index: entry Index.t
      , prioQueue: Id.t PQ.heap
      , timeQueue: Id.t TQ.heap
      , timeClock: Clock.t
      , depthClock: Clock.t
      , depthInterval: int
      , entails: Util.entails
      , global: CFormula.t
      }

   fun isEmpty (D {prioQueue, ...}) = PQ.size prioQueue = 0

   fun size (D {index, ...}) = Index.size index

   fun pp db num =
      let
         val num = case num of NONE => valOf Int.maxInt | SOME n => n
         fun list (D {index, prioQueue, ...}) =
            let
               val (rules, phands, _, _) = List.unzip4 (Index.toList index)
               val (prios, _) = List.unzip (map (fn h => PQ.get prioQueue h) phands)
            in
               List.zip (prios, rules)
            end
         fun ppItem (prio, q) = %[Rule.pp q, \, PP.int (Int.~ prio)]
         val n = size db
      in
         &[ %[$"Kept rules: ", PP.int n]
          , if n = 0 then PP.empty else
           %[\\, &(List.separate \ (map ppItem (List.take' (list db, num))))]]
      end

   fun create {entails, global} =
      let
         fun pp (r, _, _, _) = Rule.pp r
      in
         D { index = Index.create {entails = entails, global = global, pp = pp}
           , prioQueue = PQ.empty()
           , timeQueue = TQ.empty()
           , timeClock = Clock.new 1
           , depthClock = Clock.new 1
           , depthInterval = !Parameters.Db.depthInterval
           , entails = entails
           , global = global
           }
      end

   fun insert (D {index, prioQueue, timeClock, timeQueue, ...}, rule, node, {prio}) =
      let
         val id = Rule.id rule
         val concl =
            let
               val (ants, cons) = Rule.concl rule
            in
               Seq.new (Rule.constr rule, ants, cons)
            end
         (* Negate the given priority (min queue). *)
         val sizeHand = PQ.insert prioQueue (Int.~ prio) id
         val timeHand = TQ.insert timeQueue (Clock.time timeClock) id
      in
         Clock.tick timeClock
       ; Index.insert (index, id, concl,
                       (rule, sizeHand, timeHand, node))
      end

   (* dequeue. *)
   fun next (D {index, depthClock, prioQueue, timeQueue, depthInterval, ...}) =
      let in
         Clock.tick depthClock
       ; if Clock.time depthClock mod depthInterval = 0 then
            case TQ.min timeQueue of
               NONE => raise Impossible
             | SOME (_, id) =>
               case Index.find(index, id) of
                  NONE => raise Impossible
                | SOME (rule, phand, _, node) =>
                  let in
                     Index.remove(index, id)
                   ; PQ.delete prioQueue phand
                   ; (rule, node)
                  end
         else
            case PQ.min prioQueue of
               NONE => raise Impossible
             | SOME (_, id) =>
               case Index.find(index, id) of
                  NONE => raise Impossible
                | SOME (rule, _, thand, node) =>
                  let in
                     Index.remove(index, id)
                   ; TQ.delete timeQueue thand
                   ; (rule, node)
                  end
      end

   (* Remove sequents from the database *)
   fun remove (D {index, prioQueue, timeQueue, ...}, ids) =
      let
         fun remove1 id =
            if not(Index.member(index, id))
            then ()
            else
               let in
                  Log.debug (fn () => %[$"Removing kept rule: ", Id.pp id])
                ; case Index.find(index, id) of
                     NONE => ()
                   | SOME (_, phand, thand, _) =>
                     let in
                        (* Remove from the index *)
                        Index.remove(index, id)
                      (* Remove from prio queue *)
                      ; PQ.delete prioQueue phand
                      (* Remove from time queue *)
                      ; TQ.delete timeQueue thand
                     end
               end
      in
         Id.Set.app remove1 ids
      end

   fun subsumed (D {index, ...}, seq) = Index.subsumed(index, seq)

end

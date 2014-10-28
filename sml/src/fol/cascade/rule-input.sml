
structure RuleInput :> RuleInput = struct
   structure P = Fragment
   structure PSet = Param.Set

   open General
   open PP.Ops

   datatype t = T of
      { id : RuleId.t
      , hyp : Seq.t
      , hyps : Seq.t list
      , concl : Seq.t
      , fresh : Param.set
      , proof : Fragment.t }

   type printable = t
   type parseable = t

   fun id (T { id, ... }) = id
   fun proof (T { proof, ... }) = proof

   fun rename (T {id, hyp, hyps, concl, fresh, proof }, s) =
      let
         val (hyp, s) = Seq.rename (hyp, s)
         val (hyps, s) = foldr (fn (hyp, (hyps, s)) =>
            let
               val (hyp, s) = Seq.rename (hyp, s)
            in (hyp :: hyps, s)
            end) ([], s) hyps
         val (concl, s) = Seq.rename (concl, s)
         val fresh = PSet.map (fn a => Subst.applyP (a, s)) fresh
         val proof = Fragment.mapr (fn p => SC.apply (p, s)) proof
      in
         (T {id=id, hyp=hyp, hyps=hyps, concl=concl, fresh=fresh, proof=proof }, s)
      end

   fun pp (T { id, hyp, hyps, concl, fresh, ... }) =
      &[ PP.sep (PP.commas (map Seq.ppNoId (hyp :: hyps)))
       , %[ $"---------------------------", Param.Set.pp fresh, $":",
            RuleId.pp id]
       , Seq.ppNoId concl
       ]

   fun atoms (T { hyp, hyps, concl, fresh, ... }) =
      Atoms.unions (Atoms.make (Var.Set.empty, fresh) ::
                     map Seq.atoms (concl :: hyp :: hyps))

   (* parse is for debugging.  Use a dummy proof. *)
   fun parse (Parse.Rule.T { fresh, hyps, conc, ...}) =
      let
         val hyps = map Seq.parse hyps
         val (hyp, hyps) = case hyps of
            [] => raise Impossible
          | hyp :: hyps => (hyp, hyps)
         val concl = Seq.parse conc
         val fresh = PSet.ofList (map Param.parse fresh)
      in
         T { id = RuleId.next ()
           , hyp = hyp
           , hyps = hyps
           , concl = concl
           , fresh = fresh
           , proof = P.Leaf SC.Hole }
      end

   val ofString = parse o Parse.Rule.ofString

   fun ofFocus (Focus.Rule.T {hyps, concl, fresh, proof, ...}) =
      let
         val (hyp, hyps) = case map Seq.ofFocus hyps of
            [] => raise Impossible
          | h :: hs => (h, hs)
         val concl = Seq.ofFocus concl
      in
         T { id = RuleId.next ()
           , hyp = hyp
           , hyps = hyps
           , concl = concl
           , fresh = fresh
           , proof = proof }
      end

   val () = noWarnUnused (fn _ : printable * parseable => ofString)
end

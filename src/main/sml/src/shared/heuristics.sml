
(* The conversion from a formula to a polarized formula adds
   as few shifts as possible.  Here we give other options in order
   to experiment with the search space. *)

structure Heuristics :> Heuristics = struct
   structure T = Transform

   open General

   datatype kind = Equivalent | Instance

   structure Problem = struct
      datatype t = T of
         { formula : PFormula.neg
         , kind : kind
         , maxSecs : int }
      fun pp (T {formula, ...}) = PFormula.neg PFormula.pp formula
      val () = noWarnUnused (pp)
   end

   datatype t = T of
      { name : string
      , f : (PFormula.neg * {maxSecs : int}) -> Problem.t list }

   fun apply (T {f, ...}) args = f args
   fun name (T {name, ...}) = name

   fun single (name, trans) =
      let
         fun f (form, {maxSecs}) =
            [ Problem.T { formula = trans form
                        , kind = Equivalent
                        , maxSecs = maxSecs } ]
      in
         T { name = name, f = f }
      end

   val minimal = single ("minimal", T.minimal)
   val nothing = single ("nothing", Fun.id)
   val posAtoms = single ("pos-atoms", T.posAtoms)
   val negAtoms = single ("neg-atoms", T.negAtoms)
   val singleStep = single ("single-step", T.singleStep)
   val optimize1 = single ("optimize1", T.optimize)

   val optimize =
      let
         val minimalTime = 1
         fun f (form, {maxSecs}) =
            let
               val min = T.minimal form
               val opt = T.optimize form
               val optSecs = maxSecs - minimalTime
            in
               [
                 Problem.T { formula = min
                           , kind = Equivalent
                           , maxSecs = minimalTime }
               , Problem.T { formula = opt
                           , kind = Equivalent
                           , maxSecs = optSecs }]
            end
      in
         T { name = "optimize", f = f }
      end

   val random =
      let
         fun f (form, {maxSecs}) =
            let
               val instanceSecs = 2
               fun f (form, (secs, ps)) =
                  (secs - instanceSecs, Problem.T
                    { formula = form
                    , kind = Instance
                    , maxSecs = instanceSecs } :: ps)
               val (secsLeft, ps) = List.foldr f (maxSecs, []) (T.specialize form)
               val last = Problem.T { formula = T.minimal form
                                    , kind = Equivalent
                                    , maxSecs = secsLeft }
            in
               ps @ [last]
            end
      in
         T { name = "random", f = f }
      end

   val fromString = fn
      "nothing" => SOME nothing
    | "minimal" => SOME minimal
    | "neg-atoms" => SOME negAtoms
    | "pos-atoms" => SOME posAtoms
    | "optimize1" => SOME optimize1
    | "optimize" => SOME optimize
    | "single-step" => SOME singleStep
    | "random" => SOME random
    | _ => NONE
end

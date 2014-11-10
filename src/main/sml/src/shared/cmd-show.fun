
functor CmdShowFn (val summary : string
                   structure Frontend : Frontend) :> Cmd = struct

   structure C = Command
   structure U = CmdUtil
   structure F = PFormula
   structure H = Heuristics
   structure Meta = Parse.Meta

   open General
   open PP.Ops

   fun readme () =
      &[ $summary
       , ~
       , $"Like cat, read from stdin unless a file is passed"
       , ~
       , $"Examples:"
       , %[\\, &[ $"show foo.imo"
                , $"show < foo.imo"
                , $"show <(echo 'a -> a')" ]]]

   structure Args = struct
      val syntax = ref U.Syntax.Basic
      val heuristics = ref H.nothing
      val inline = ref false
   end

   val flags = [ U.Flags.syntax Args.syntax
               , U.Flags.heuristics Args.heuristics
               , U.Flags.parseInline Args.inline ]

   val usageArg = "FILE"

   fun run { anons } =
      let
         val (meta, f) = case (anons, !Args.inline) of
            ([], _) =>
            (* (Meta.ofStdin (), Parse.imogen.Formula.ofStdin ()) *)
            ([], Parse.imogen.Formula.ofStdin ())
          | ([file], false) =>
            (Meta.ofFile file, Parse.imogen.Formula.ofFile file)
          | ([form], true) =>
            (Meta.ofString form, Parse.imogen.Formula.ofString form)
          | _ => failwith "Too many args"
         val f1 = Frontend.parse f
         val f = Frontend.pformula f1
         val ps = H.apply (!Args.heuristics) (f, {maxSecs = !Parameters.Prover.maxSeconds})
         val fs = List.map (fn H.Problem.T { formula, ... } => formula) ps
         val meta = if Meta.nonempty meta then &[Meta.pp meta, ~] else PP.empty
         val fs = &(PP.punctuate PP.space (List.map (F.neg F.pp) fs))
      in
         PP.ppl (&[ meta
                  , $"Input: "
                  , %[\\, Frontend.pp f1]
                  , $"Translated: "
                  , %[\\, fs] ])
       ; OS.Process.success
      end

   val cmd = C.create
                { readme = readme
                , summary = summary
                , usageArg = usageArg
                , flags = flags
                , run = run }

end

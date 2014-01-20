
structure Log :> Log = struct
   datatype level = Trace | Debug | Info | Warning | Error | Critical | Nothing

   val int = fn
      Trace => 60
    | Debug => 50
    | Info => 40
    | Warning => 30
    | Error => 20
    | Critical => 10
    | Nothing => 0

   val ofString = fn
      "trace" => SOME Trace
    | "debug" => SOME Debug
    | "info" => SOME Info
    | "warning" => SOME Warning
    | "error" => SOME Error
    | "critical" => SOME Critical
    | "nothing" => SOME Nothing
    | _ => NONE

   fun gte (l1, l2) = int l1 >= int l2

   val ofInt = fn
      6 => Trace
    | 5 => Debug
    | 4 => Info
    | 3 => Warning
    | 2 => Error
    | 1 => Critical
    | 0 => Nothing
    | _ => Trace

   val level : level ref = ref Nothing
   fun setLevel l = level := l
   fun setLevel' n = level := (ofInt n)
   fun gen l f =
      if gte (!level, l) then
         let
            (* val _ = print "evaling pp object" *)
            val p = f ()
         in
            if PP.is_empty p then ()
            else PP.ppl p
         end
      else ()
   val trace = gen Trace
   val debug = gen Debug
   val info = gen Info
   val warning = gen Warning
   val error = gen Error
   val critical = gen Critical
   val level = fn () => !level
   fun msg f = case f (level ()) of
      NONE => ()
    | SOME x => PP.ppl x
end

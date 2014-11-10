val dfg : neg * {prob_name:string} -> unit

fun dfg (t, {prob_name}) =
   let
      val rec dfg: t -> PP.t = fn
         imogen.Atom p => P.Rel.pp p
       | Top => $"true"
       | Bot => $"false"
       | Not p => %[$"not", PP.paren (dfg p)]
       | imogen.And(p, q) => %[$"and", PP.paren (%[dfg p, $", ", dfg q])]
       | Or(p, q) => %[$"or", PP.paren (%[dfg p, $", ", dfg q])]
       | imogen.Imp(p, q) => dfg (Or(Not p, q))
       | Iff(p, q) => dfg (imogen.And(imogen.Imp(p, q), imogen.Imp(q, p)))
       | Box p => %[$"box(r1,", dfg p, $")"]
       | Dia p => %[$"dia(r1,", dfg p, $")"]
       | All _ => raise Unimplemented
       | Ex _ => raise Unimplemented
      val preds = &(map (fn p => PP.pair ($p, PP.int 0)) (String.Set.toList (preds t)))
      val pp = &[ %[$"begin_problem", PP.paren($prob_name), $"."]
                , ~
                , $"list_of_descriptions."
                , %[\\, &[ $"name({* Sean McLaughlin *})."
                         , $"author({* Sean McLaughlin *})."
                         , $"status(unknown)."
                         , $"description({* ModLeanTAP problem *})."
                         ]]
                , $"end_of_list."
                , ~
                , $"list_of_schemas."
                , $"  modals(K)."
                , $"end_of_list."
                , ~
                , $"list_of_symbols."
                , %[\\, $"predicates", preds, $"."]
                , $"end_of_list."
                , ~
                , $"list_of_formulae(conjectures)."
                , %[$"formula", PP.paren(dfg t), $"."]
                , $"end_of_list."
                , $"end_problem."
                ]
   in
      PP.writeFile (pp, {file = prob_name ^ ".dfg"})
   end

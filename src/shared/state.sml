
structure State :> State = struct
   fun reset () =
      let in
         (*   Var.reset () *)
         (* ; Param.reset () *)
         (* ; Func.reset () *)
         (* ; Pred.reset () *)
         (* ; Label.reset () *)
         Id.reset ()
       ; RuleId.reset ()
       ; SC.reset ()
       ; Signat.reset ()
      end
end

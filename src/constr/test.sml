
structure Test : Test = struct
   open UnitTest.Ops

   (* structure CUnif = CUnifTestFn(CUnif) *)

   val test =
      $("Constr", &[
                    (* CUnif.test *)
                   ])
end

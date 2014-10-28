
functor SimpsonFn (val entails : Util.entails) : Prover = struct
   structure Prover =
      ProverFn
         (structure Frontend = Modal.Simpson
          structure Backend = BackendFn (val entails = entails
                                         val simp = CFormula.noSimp)
          val eq = Term.eq
          val norm = Fun.id)
   open Prover
end

structure Provers :> Provers = struct
   structure Modal = struct
      structure K = SimpsonFn (val entails = Modal.Entails.K.f)
      structure T = SimpsonFn (val entails = Modal.Entails.T.f)
      structure B = SimpsonFn (val entails = Modal.Entails.B.f)
      structure K4 = SimpsonFn (val entails = Modal.Entails.K4.f)
      structure S4 = SimpsonFn (val entails = Modal.Entails.S4.f)
      structure S5 = SimpsonFn (val entails = Modal.Entails.S5.f)

      structure PD = struct
         structure Prover =
            ProverFn
               (structure Frontend = Modal.Davies
                structure Backend = BackendFn (val entails = Modal.Entails.S4.f
                                               val simp = CFormula.noSimp)
                val eq = Term.eq
                val norm = Fun.id)

         open Prover
      end
   end

   structure Linear =
      ProverFn
         (structure Frontend = Linear.Frontend
          structure Backend = BackendFn (val entails = Linear.entails
                                         val simp = Linear.simp)
          val eq = CUnif.eq
          val norm = CUnif.reduce)

   structure Ordered =
      ProverFn
         (structure Frontend = Ordered.Frontend
          structure Backend = BackendFn (val entails = Ordered.entails
                                         val simp = CFormula.noSimp)
          val eq = CUnif.eq
          val norm = Fun.id)

end

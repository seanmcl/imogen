
structure Timer : Timer = struct
   open Timer
   type su = {sys: Time.time, usr: Time.time}

   fun checkCPUTimer' t =
      let
         val {usr, sys} = checkCPUTimer t
      in
         Time.+(usr, sys)
      end
end

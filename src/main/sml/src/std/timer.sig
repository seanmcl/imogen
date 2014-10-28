
signature Timer = sig
   type cpu_timer
   type real_timer
   type su = {sys: Time.time, usr: Time.time}

   val checkCPUTimer: cpu_timer -> su
   val checkCPUTimes: cpu_timer -> {gc: su, nongc: su}
   val checkGCTime: cpu_timer -> Time.time
   val checkRealTimer: real_timer -> Time.time
   val startCPUTimer: unit -> cpu_timer
   val startRealTimer: unit -> real_timer
   val totalCPUTimer: unit -> cpu_timer
   val totalRealTimer: unit -> real_timer

   (* Give the sum of the sys and user time *)
   val checkCPUTimer': cpu_timer -> Time.time
end

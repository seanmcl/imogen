
structure Domain :> Domain = struct
   datatype t = I | Disunif | Modal | Linear | Ordered
   type eqable = t
   val eq = op=
end



signature Domain = sig
   datatype t = I | Disunif | Modal | Linear | Ordered
   include Eqable where type eqable = t
end


structure Fragment :> Fragment = struct
   open General

   datatype t =
      Leaf of SC.t
    | Node of SC.t -> t

   fun mapr f = fn
      Leaf t => Leaf (f t)
    | Node g => Node (mapr f o g)

   fun mapr2 f = fn
      (Leaf t, r) => mapr (fn d => f (t,d)) r
    | (Node g, r) => Node (fn d => mapr2 f (g d, r))

   val rec sc = fn
      Leaf t => t
    | Node f => sc (f SC.Hole)

   val pp = SC.pp o sc

   val atoms = SC.atoms o sc

   fun apply (t, s) = mapr (fn p => SC.apply (p, s)) t

   val rec sc = fn
      (Leaf t, []) => t
    | (Node f, t::ts) => sc (f t, ts)
    | _ => failwith "Fragment.sc"

end

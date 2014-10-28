
structure Frame :> Frame = struct
   open General
   type t = Term.t * Term.t * Term.t
   val star = Pred.Ordered.star
   fun make (w1, h, w2) = Rel.make (star, [w1, h, w2])
   fun dest r = case Rel.dest r of
      (p, [w1, h, w2]) =>
      if Pred.eq (p, star) then (w1, h, w2)
      else failwith "Frame.dest"
    | _ => failwith "Frame.dest"
   fun extend ((w1, h, w2), w) = (w1, h, World.timesT (w2, w))
   val () = noWarnUnused (fn _ : t => (dest))
end

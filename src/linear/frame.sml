
structure Frame :> Frame = struct
   open General
   type t = Term.t * Term.t
   val star = Pred.Linear.star
   fun make (h, w) = Rel.make (star, [h, w])
   fun dest r = case Rel.dest r of
      (p, [h, w]) => if Pred.eq (p, star) then (h, w) else failwith "Frame.dest"
    | _ => failwith "Frame.dest"
   fun extend ((h, w), w') = (h, World.timesT (w, w'))
   val () = noWarnUnused (fn _ : t => (dest))
end

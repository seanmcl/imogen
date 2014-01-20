
structure Logic :> Logic = struct
   open General
   structure Axiom = struct
      datatype t = M | B | A4
   end
   structure A = Axiom

   datatype t = K | T | B | K4 | S4 | S5
   type showable = t
   type parseable = t

   val toString = fn
      K => "K"
    | T => "T"
    | B => "B"
    | K4 => "K4"
    | S4 => "S4"
    | S5 => "S5"

   fun ofString s = case String.downcase s of
      "k" => K
    | "t" => T
    | "B" => B
    | "K4" => K4
    | "S4" => S4
    | "S5" => S5
    | _ => failwith ("Unknown modal logic: " ^ s)

   val axioms = fn
      K => []
    | T => [A.M]
    | B => [A.B]
    | K4 => [A.A4]
    | S4 => [A.M, A.A4]
    | S5 => [A.M, A.B, A.A4]

  val () = noWarnUnused (fn _ : showable * parseable => (toString, ofString))
end

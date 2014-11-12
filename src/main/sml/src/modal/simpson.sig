
signature Simpson = sig
   datatype pos =
      PAtom of Term.t
    | One
    | Zero
    | And of pos * pos
    | Or of pos * pos
    | Dia of pos
    | Ex of Var.t * pos
    | Down of neg
   and neg =
      NAtom of Term.t
    | Top
    | With of neg * neg
    | Imp of pos * neg
    | Iff of neg * neg
    | Box of neg
    | All of Var.t * neg
    | Up of pos
   include Frontend where type t = neg
end

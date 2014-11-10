
signature Davies = sig
   datatype pos =
      PAtom of Term.t
    | One
    | Zero
    | imogen.And of pos * pos
    | Or of pos * pos
    | Box of neg
    | Ex of Var.t * pos
    | Down of neg
   and neg =
      NAtom of Term.t
    | Top
    | With of neg * neg
    | imogen.Imp of pos * neg
    | Not of pos
    | Iff of neg * neg
    | Dia of pos
    | All of Var.t * neg
    | Up of pos
   include Frontend where type t = neg
end

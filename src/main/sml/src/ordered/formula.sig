
signature imogen.Formula = sig
   datatype pos =
      PAtom of Term.t
    | Dot of pos * pos
    | One
    | Sum of pos * pos
    | Zero
    | Down of neg
    | Bang of neg
    | UBang of neg
    | Ex of Var.t * pos
   and neg =
      NAtom of Term.t
    | With of neg * neg
    | Top
    | ImpL of pos * neg (* >-> *)
    | ImpR of pos * neg (* ->> *)
    | Up of pos
    | All of Var.t * neg
   include Frontend where type t = neg
end

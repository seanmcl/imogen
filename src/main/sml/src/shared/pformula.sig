
(* Polarized formulas.  We use linear logic notation.

   P ::= a | a (T1, ..., Tn) | P * P | 1 | P + P | 0 | !N
        | forall [X] : P | ( P )

   N ::= A | A (T1, ..., Tn) | N & N | T | P -o N
       | ^P | exists [X] : N | ( N )
 *)
signature PFormula = sig
   structure Export : sig
      datatype pos =
         PAtom of Rel.t
       | Tensor of pos * pos
       | One
       | Sum of pos * pos
       | Zero
       | Down of neg
       | Ex of (Var.t * Sort.t) * pos
       | PLabel of string * pos

      and neg =
         NAtom of Rel.t
       | With of neg * neg
       | Top
       | Lolli of pos * neg
       | BiLolli of neg * neg
       | Up of pos
       | All of (Var.t * Sort.t) * neg
       | NLabel of string * neg
   end
   datatype pos = datatype Export.pos
   datatype neg = datatype Export.neg

   (* val listAll: (Var.t * Sort.t) list * neg -> neg *)
   val mkLolli: pos list * neg -> neg
   val destLolli: neg -> pos list * neg

   (* val closed: neg -> bool *)
   val formulate: Formula.t -> neg
   val parse: Parse.Formula.t -> neg

   val ofString: string -> neg

   (* Infer the signature for function and predicate symbols. *)
   val signat: neg -> unit

   type ('a, 'b) t = { p : pos -> 'a, n : neg -> 'b }
   type 'a func = ('a, 'a) t
   type transform = (pos, neg) t

   val pos : ('a, 'b) t -> pos -> 'a
   val neg : ('a, 'b) t -> neg -> 'b

   val size : int func
   val map : (Rel.t -> Rel.t) -> transform

   (* ↓↑p, ↑↓n *)
   val shift2 : transform

   (* Make all bound variables distinct.
      ! [X] : p (X) & ? [X] : q (X, X) -->
      ! [X] : p (X) & ? [Y] : q (Y, Y) *)
   val separate: transform

   (* Only the free variables. *)
   val atoms: Atoms.t func

   val preds: {pos:Pred.set, neg:Pred.set} func

   val erase: Formula.t func

   val apply: Subst.t -> transform
   val apply1: Var.t * Term.t -> transform

   val propositional : neg -> bool

   (* Printing *)
   val pp: PP.t func

end

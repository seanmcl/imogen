
(* Natural deduction proof terms for intuitionistic logic.

   Bidirectional proof terms.  These are more economic than
   the basic version, as no type information is needed except
   for ascriptions.

   I ::=
       | (I1, I2)
       | lam (u, I)
       | inl I
       | inr I
       | case E of
                   x => I1
                 | y => I2
       | ()
       | abort E
       | silent
         | let x = E in I
   E ::=
       | u
       | fst E
       | snd E
       | E I
       | I : A
*)

signature ND = sig

   datatype intro =
      Pair of intro * intro
    | Lam of Label.t * intro
    | Inl of intro
    | Inr of intro
    | Case of elim
      * (Label.t * intro)
      * (Label.t * intro)
    | Unit
    | Abort of elim
    | Elim of elim
    | Let of (Label.t * elim) * intro
    | QLam of Var.t * intro
    | Pack of Term.t * intro
    | Unpack of Var.t * Label.t * elim * intro
    | Hole

   and elim =
      Label of Label.t
    | Fst of elim
    | Snd of elim
    | App of elim * intro
    | Ascribe of intro * imogen.Formula.t
    | QApp of elim * Term.t

   type t = intro

   val apply1: t * (Var.t * Term.t) -> t

   (* substitute a variable (avoiding capture) for a parameter *)
   val paramSubst: t * (Param.t * Var.t) -> t

   (* normalize a term
      normalization might fail with exception Check if term is ill-typed *)
   val normalize: t -> t

   (* check a term against a type with a small proof checker
      raise an exn if the term doesn't prove the given type. *)
   val check:
      { eq : Term.t * Term.t -> bool
      , ctx : (Label.t * imogen.Formula.t) list
      , term : t
      , form : imogen.Formula.t } -> unit

   (* label(nd, f): try to use the labels of f as variable names in nd.
      Assumes bound variables are unique. *)
   val label: t * imogen.Formula.t -> t

   val pp: t -> PP.t

   val size : t -> int
end

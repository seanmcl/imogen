
signature Signat = sig
   type ('a, 'b) t
   type 'a func = ('a, 'a) t
   val pp : unit -> PP.t

   val func : ('a, 'b) t -> Func.t -> 'a
   val pred : ('a, 'b) t -> Pred.t -> 'b

   val mem : bool func
   val arity : int func
   val findExn : (Sort.Func.t, Sort.Pred.t) t
   val find : (Sort.Func.t option, Sort.Pred.t option) t

   val extend : (Sort.Func.t -> unit, Sort.Pred.t -> unit) t

   val reset : unit -> unit
end


signature Bool =
sig

include BOOL

val and': bool * bool -> bool
val or': bool * bool -> bool
val compare: bool * bool -> order
val eq: bool * bool -> bool

structure Ops:
             sig
                val && : bool * bool -> bool
                val || : bool * bool -> bool
             end

end

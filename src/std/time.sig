
signature Time = sig
   eqtype t
   val + : t * t -> t
   val - : t * t -> t
   val < : t * t -> bool
   val <= : t * t -> bool
   val > : t * t -> bool
   val >= : t * t -> bool
   val compare: t * t -> order
   val fmt: int -> t -> string
   val fromMicroseconds: LargeInt.int -> t
   val fromMilliseconds: LargeInt.int -> t
   val fromNanoseconds: LargeInt.int -> t
   val fromReal: LargeReal.real -> t
   val fromSeconds: LargeInt.int -> t
   val fromString: string -> t option
   val now: unit -> t
   val scan: (char, 'a) StringCvt.reader -> (t, 'a) StringCvt.reader
   val toMicroseconds: t -> LargeInt.int
   val toMilliseconds: t -> LargeInt.int
   val toNanoseconds: t -> LargeInt.int
   val toReal: t -> LargeReal.real
   val toSeconds: t -> LargeInt.int
   val toString: t -> string
   val zeroTime: t
end

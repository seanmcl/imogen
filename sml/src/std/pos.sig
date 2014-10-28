
(**
 * positions within a file (for reporting errors)
 *
 * when reading a position, it is always returned as a pair
 * of the left edge and right edge, since a position may
 * delimit a range. If a point, then these are equal.
 *
 * if positions are ranges (start,finish):
 *   union ((s,f),(s',f')) = (min (s,s'),max (f,f'))
 *   max   ((s,f),(s',f')) = (max (s,s'),max (f,f'))
 *   min   ((s,f),(s',f')) = (min (s,s'),min (f,f'))
*)
signature Pos = sig

   type t

   val initPos: t
   val min: t * t -> t
   val max: t * t -> t
   val union: t * t -> t
   val toString: t -> string

   val markStream: char Stream.t -> (char * t) Stream.t

   (* alternately, pass in a filename *)
   val markFileStream: string -> char Stream.t -> (char * t) Stream.t

   val markAny: 'a Stream.t -> ('a * t) Stream.t
   val initFilePos: string -> t
   val nextChar: t -> t
   val nextLine: t -> t
   val rightEdge: t -> t

   (* absolute source file tition *)
   val getAbs: t -> int * int

   val getCol: t -> int * int

   (* Line of start position *)
   val getLine: t -> int

end



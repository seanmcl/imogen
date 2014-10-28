
structure General : General = struct
   open Pair
   open Char
   open Array
   open String1
   open General
   open Option
   open List
   open Int
   open Fun
   open Debug
   open WithReturn
   open TextIO
   open Either

   datatype bool = datatype bool
   datatype ref = datatype ref
   datatype list = datatype list
   datatype result = datatype Result.t
   datatype either = datatype Either.t

   (* (\* print should be covered by open TextIO, but when they aren't *)
   (*    here, print does nothing.  A mystery not worth investigating. *\) *)
   (* val print = TextIO.print *)
   (* val printl = TextIO.printl *)

   val not = not
   val op <> = op <>
   val op = = op =

   val assert' = Debug.assert'

   fun list x = [x]

   exception Impossible
   exception Unimplemented
   exception NotFound = LibBase.NotFound

   fun noWarnUnused _ = ()
end


structure Either :> Either = struct

   datatype ('a, 'b) t =
      Left of 'a
    | Right of 'b

   fun either f g e =
      case e of
         Left x => f x
       | Right x => g x

   val leftExn =
      fn Left x => x
       | Right _ => raise Fail "leftExn"

   val rightExn =
      fn Left _ => raise Fail "rightExn"
       | Right x => x

   val rec lefts: ('a, 'b) t list -> 'a list =
      fn [] => []
       | Left x :: xs => x :: lefts xs
       | Right _ :: xs => lefts xs

   val rec rights: ('a, 'b) t list -> 'b list =
      fn [] => []
       | Left _ :: xs => rights xs
       | Right x :: xs => x :: rights xs

   val rec partition: ('a, 'b) t list -> 'a list * 'b list =
      fn [] => ([], [])
       | x :: xs =>
         let
            val (vs, ws) = partition xs
         in
            case x of
               Left v => (v::vs, ws)
             | Right w => (vs, w::ws)
         end

   fun compare lfn rfn = fn
      (Left x, Left y) => lfn (x, y)
    | (Left _, _) => LESS
    | (_, Left _) => GREATER
    | (Right a, Right b) => rfn (a, b)

end

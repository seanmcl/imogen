
structure PP :> PP = struct
   open Lazy.Ops

   fun make_str n c = CharVector.tabulate (n, fn _ => c)
   fun indent n = make_str n #" "
   fun fail () = raise (Fail "Impossible")

   datatype text_details =
      Chr of char
    | Str of string
   val tdstr = fn Chr c => String.str c | Str s => s

   datatype doc =
      Empty
    | Nil_above of doc'
    | Text_beside of text_details * int * doc'
    | Nest of int * doc'
    | Union of doc' * doc'
    | No_doc
    | Beside of doc' * bool * doc'
    | Above of doc' * bool * doc'
   withtype doc' = doc Lazy.t

   datatype mode =
      Page_mode
    | Left_mode
    | One_line_mode

   type style = { mode : mode
                , line_length : int
                , ribbons_per_line : real }

   val rec size = fn
      Empty => 1
    | Nil_above d => 1 + size (!d)
    | Text_beside (_, _, d) => 1 + size (!d)
    | Nest (_, d) => 1 + size (!d)
    | Union _ => fail ()
    | No_doc => 1
    | Beside (d1, _, d2) => 1 + size (!d1) + size (!d2)
    | Above (d1, _, d2) => 1 + size (!d1) + size (!d2)

   val default_line_size = 140

   val style = { line_length = default_line_size, ribbons_per_line = 1.0, mode = Page_mode }

   val space_text = Chr #" "

   val nl_text = Chr #"\n"

   fun sized_text l s = Text_beside (Str s, l, ?Empty)
   val zero_width_text = sized_text 0

   fun nil_beside g t = case t of
      Empty => Empty
    | Nest (_, p) => nil_beside g (!p)
    | _ =>
      if g then Text_beside (space_text, 1, ?t)
      else t

   fun reduce_ab t = case t of
      Above (t', _, q) =>
      let in
         case !t' of
            Empty => !q
          | _ => t
      end
    | Beside (t', _, q) =>
      let in
         case !t' of
            Empty => !q
          | _ => t
      end
    | _ => t

   fun char c = Text_beside (Chr c, 1, ?Empty)

   val space = char #" "
   val semi  = char #";"
   val colon = char #":"
   val comma = char #","
   val equals = char #"="
   val lparen = char #"("
   val rparen = char #")"
   val lbrack = char #"["
   val rbrack = char #"]"
   val lbrace = char #"{"
   val rbrace = char #"}"

   fun text s = Text_beside (Str s, String.size s, ?Empty)

   fun mk_nest k t = case t of
      Nest (k1, p) => mk_nest (k + k1) (!p)
    | No_doc => t
    | Empty => t
    | _ => case k of
              0 => t
            | k => Nest (k, ?t)

   fun mk_union p q = case !p of
      Empty => Empty
    | _ => Union (p, q)

   fun is_empty t = case t of
      Empty => true
    | _ => false

   fun char c = Text_beside (Chr c, 1, ?Empty)

   val beside_ = fn
      (p, _, Empty) => p
    | (Empty, _, q) => q
    | (p, g, q) => Beside (?p, g, ?q)

   val above_ = fn
      (p, _, Empty) => p
    | (Empty, _, q) => q
    | (p, g, q) => Above (?p, g, ?q)

   infix $$ $+$ <-> <+>
   structure Infix = struct
      fun op$$ (p, q) = above_ (p, false, q)
      fun op$+$ (p, q) = above_ (p, true, q)
      fun op<> (p, q) = beside_ (p, false, q)
      fun op<+> (p, q) = beside_ (p, true, q)
   end
   open Infix

   fun int n = text (Int.toString n)
   fun unit () = text "()"
   fun real n = text (Real.toString n)
   fun quote p = char #"'" <> p <> char #"'"
   fun doubleQuote p  = char #"\"" <> p <> char #"\""
   fun paren p = char #"(" <> p <> char #")"
   fun bracket p = char #"[" <> p <> char #"]"
   fun brace p = char #"{" <> p <> char #"}"

   fun punctuate p = fn
      [] => []
    | d :: ds =>
      let
         fun go d' = fn
            [] => [d']
          | e :: es => (d' <> p) :: go e es
      in
         go d ds
      end

   val hcat = reduce_ab o List.foldr (fn (p, q) => beside_ (p, false, q)) Empty
   val hsep = reduce_ab o List.foldr (fn (p, q) => beside_ (p, true, q)) Empty
   val vcat = reduce_ab o List.foldr (fn (p, q) => above_ (p, false, q)) Empty

   fun nil_above_nest g k q : doc = case q of
      Empty => q
    | Nest (k1, q) => nil_above_nest g (k + k1) (!q)
    | _ =>
      if not g andalso k > 0
      then Text_beside (Str (indent k), k, ?q)
      else Nil_above (%(fn () => mk_nest k q))

   fun above_nest (p:doc) g k q = case p of
      No_doc => p
    | Union (p1, p2) => Union ( %(fn () => above_nest (!p1) g k q),
                               %(fn () => above_nest (!p2) g k q) )
    | Empty => mk_nest k q
    | Nest (k1, p) => Nest (k1, %(fn () => above_nest (!p) g (k - k1) q))
    | Nil_above p => Nil_above (%(fn () => above_nest (!p) g k q))
    | Text_beside (s, sl, p) =>
      let
         val k1 = k - sl
         val rest = %(fn () => case !p of
            Empty => nil_above_nest g k1 q
          | p => above_nest p g k1 q)
      in
         Text_beside (s, sl, rest)
      end
    | Above _ => fail ()
    | Beside _ => fail ()

   val reduce_doc =
      let
         fun reduce_doc p = case p of
            Beside (p, g, q) => beside (!p) g (reduce_doc (!q))
          | Above (p, g, q) => above (!p) g (reduce_doc (!q))
          | _ => p

         and beside p g q = case p of
            No_doc => No_doc
          | Union (p1, p2) =>
            Union (%(fn () => beside (!p1) g q), %(fn () => beside (!p2) g q))
          | Empty => q
          | Nest (k, p) => Nest (k, %(fn () => beside (!p) g q))
          | Beside (p1, g1, q1) =>
            if g1 = g then beside (!p1) g1 (beside (!q1) g q)
            else beside (reduce_doc p) g q
          | Above _ => beside (reduce_doc p) g q
          | Nil_above p => Nil_above (%(fn () => beside (!p) g q))
          | Text_beside (s, sl, p) =>
            let
               val rest = %(fn () => case !p of
                  Empty => nil_beside g q
                | p => beside p g q)
            in
               Text_beside (s, sl, rest)
            end

         and above p g q = case p of
            Above (p, g1, q1) => above (!p) g1 (above (!q1) g q)
          | Beside _ => above_nest (reduce_doc p) g 0 (reduce_doc q)
          | _ => above_nest p g 0 (reduce_doc q)
      in
         reduce_doc
      end

   fun one_liner t = case t of
      No_doc => t
    | Empty => t
    | Nil_above _ => No_doc
    | Text_beside (s, sl, p) => Text_beside (s, sl, %(fn () => one_liner (!p)))
    | Nest (k, p) => Nest (k, %(fn () => one_liner (!p)))
    | Union (p, _) => one_liner (!p)
    | Above _ => fail ()
    | Beside _ => fail ()

   val (fsep, fcat) =
      let
         fun fill g = fn
            [] => Empty
          | p :: ps => fill1 g (reduce_doc p) 0 ps

         and fill1 g p k ys = case p of
            No_doc => No_doc
          | Union (p, q) =>
            Union ( %(fn () => fill1 g (!p) k ys),
                   %(fn () => above_nest (!q) false k (fill g ys)) )
          | Empty => mk_nest k (fill g ys)
          | Nest (n, p) => Nest (n, %(fn () => fill1 g (!p) (k - n) ys))
          | Nil_above p => Nil_above (%(fn () => above_nest (!p) false k (fill g ys)))
          | Text_beside (s, sl, p) => Text_beside (s, sl, %(fn () => fill_nb g (!p) (k - sl) ys))
          | Above _ => fail ()
          | Beside _ => fail ()

         and fill_nb g p k ys = case (p, ys) of
            (Nest (_, p), _) => fill_nb g (!p) k ys
          | (Empty, []) => Empty
          | (Empty, y :: ys) =>
            let in
               case y of
                  Empty => fill_nb g Empty k ys
                | _ => fill_nbe g k y ys
            end
          | _ => fill1 g p k ys

         and fill_nbe g k y ys =
            let
               val k1 = if g then k - 1 else k
               fun elide_nest t = case t of
                  Nest (_, d) => !d
                | _ => t
            in
               mk_union
                  (%(fn () => nil_beside g (fill1 g ((elide_nest o one_liner o reduce_doc) y) k1 ys)))
                  (%(fn () => nil_above_nest false k (fill g (y :: ys))))
            end
         val fsep = fill true
         val fcat = fill false
      in
         (fsep, fcat)
      end

   val (sep, cat) =
      let
         fun sep_nb g p k ys = case p of
            Nest (_, p) => sep_nb g (!p) k ys
          | Empty =>
            let val rest = if g then hsep ys else hcat ys in
               mk_union
                  (%(fn () => one_liner (nil_beside g (reduce_doc rest))))
                  (%(fn () => nil_above_nest true k (reduce_doc (vcat ys))))
            end
          | _ => sep1 g p k ys

         and sep1 g p k ys = case p of
            No_doc => No_doc
          | Union (p, q) =>
            Union (%(fn () => sep1 g (!p) k ys),
                   %(fn () => above_nest (!q) false k (reduce_doc (vcat ys))))
          | Empty => mk_nest k (sep_x g ys)
          | Nest (n, p) => Nest (n, %(fn () => sep1 g (!p) (k - n) ys))
          | Nil_above p =>
            Nil_above (%(fn () => above_nest (!p) false k (reduce_doc (vcat ys))))
          | Text_beside (s, sl, p) =>
            Text_beside (s, sl, %(fn () => sep_nb g (!p) (k - sl) ys))
          | Above _ => fail ()
          | Beside _ => fail ()

         and sep_x x = fn
            [] => Empty
          | p :: ps => sep1 x (reduce_doc p) 0 ps

         val sep = sep_x true
         val cat = sep_x false
      in
         (sep, cat)
      end

   fun best mode w0 r p0 = case mode of
      One_line_mode =>
      let
         val rec non_empty_set = fn
            No_doc => false
          | Union _ => true
          | Empty => true
          | Nil_above _ => true
          | Text_beside (_, _, p) => non_empty_set (!p)
          | Nest (_, p) => non_empty_set (!p)
          | Above _ => fail ()
          | Beside _ => fail ()
         fun first p q = if non_empty_set p then p else !q
         val rec get = fn
            Empty => Empty
          | No_doc => No_doc
          | Nil_above p => Nil_above (%(fn () => get (!p)))
          | Text_beside (s, sl, p) => Text_beside (s, sl, %(fn () => get (!p)))
          | Nest (_, p) => get (!p)
          (* Too much work *)
          | Union (p, q) => first (get (!p)) (%(fn () => get (!q)))
          | _ => fail ()
      in
         get p0
      end
    | _ =>
      let
         fun fits n p =
            if n < 0 then false
            else case p of
                    No_doc => false
                  | Empty => true
                  | Nil_above _ => true
                  | Text_beside (_, sl, p) => fits (n - sl) (!p)
                  | _ => fail ()
         fun nicest1 w r sl p q = if fits ((Int.min (w, r)) - sl) p then p else !q
         fun nicest w r p q = nicest1 w r 0 p q
         fun get w = fn
            Empty => Empty
          | No_doc => No_doc
          | Nil_above p => Nil_above (%(fn () => get w (!p)))
          | Text_beside (s, sl, p) => Text_beside (s, sl, %(fn () => get1 w sl (!p)))
          | Nest (k, p) => Nest (k, %(fn () => get (w - k) (!p)))
          | Union (p, q) => nicest w r (get w (!p)) (%(fn () => get w (!q)))
          | _ => fail ()
         and get1 w sl = fn
            Empty => Empty
          | No_doc => No_doc
          | Nil_above p => Nil_above (%(fn () => get (w - sl) (!p)))
          | Text_beside (t, tl, p) => Text_beside (t, tl, %(fn () => get1 w (sl + tl) (!p)))
          | Nest (_, p) => get1 w sl (!p)
          | Union (p, q) => nicest1 w r sl (get1 w sl (!p)) (%(fn () => get1 w sl (!q)))
          | _ => fail ()
      in
         get w0 p0
      end

   (* ----------------------------------------------------------------------- *)
   (*  Display                                                                *)
   (* ----------------------------------------------------------------------- *)

   fun display op<> end_ doc =
      let
         fun lay k t = case t of
            Nest (k1, p) => lay (k + k1) (!p)
          | Empty => end_
          | Above _ => fail ()
          | Beside _ => fail ()
          | No_doc => fail ()
          | Union _ => fail ()
          | Nil_above p => nl_text <> lay k (!p)
          | Text_beside (s, sl, p) => lay1 k s sl p
         and lay1 k s sl p = Str (indent k) <> (s <> lay2 (k + sl) p)
         and lay2 k t = case !t of
            Nil_above p => nl_text <> lay k (!p)
          | Text_beside (s, sl, p) => s <> lay2 (k + sl) p
          | Nest (_, p) => lay2 k p
          | Empty => end_
          | _ => fail ()
         val res = lay 0 doc
      in
         res
      end

   (* Since I don't have a value of type 'a in ocaml, (cant_fail in the Haskell code)
which would warn us when an invariant is broken, we use the dummy 'end_' value *)
   fun easy_display nl_space_text op<> end_ doc =
      let
         fun lay doc no_doc = case doc of
            No_doc => no_doc
          | Union (_, q) => lay (!q) end_
          | Nest (_, p) => lay (!p) no_doc
          | Empty => end_
          | Nil_above p => nl_space_text <> lay (!p) end_
          | Text_beside (s, _, p) => s <> lay (!p) no_doc
          | _ => fail ()
      in
         lay doc end_
      end

   fun full_render the_mode line_length ribbons_per_line op<> end_ doc = case the_mode of
      One_line_mode =>
      easy_display space_text op<> end_ (reduce_doc doc)
    | Left_mode =>
      easy_display nl_text op<> end_ (reduce_doc doc)
    | _ =>
      let
         val ribbon_length = Real.floor (Real.fromInt line_length / ribbons_per_line)
         val doc' = reduce_doc doc
         (* val _ = print "finding best\n" *)
         val best_doc = best the_mode line_length ribbon_length doc'
      (* val _ = print "found best\n" *)
      (* val _ = print "pp-size: " *)
      (* val _ = print (Int.toString (size best_doc)) *)
      (* val _ = print "\n" *)
      in
         display op<> end_ best_doc
      (* easy_display nl_text op<> end_ best_doc *)
      end

   fun str_list (t, l) = case t of
      Chr c => String.str c :: l
    | Str s1 => s1 :: l

   fun list_doc doc rest =
      full_render Page_mode default_line_size 1.5 str_list rest doc

   fun pp doc =
      let
         val l = list_doc doc []
      (* val _ = print ("size: " ^ Int.toString (length l) ^ "\n") *)
      (* val s = String.concat l *)
      (* val _ = print ("size: " ^ Int.toString (String.size s) ^ "\n") *)
      in
         app print l
      end

   fun writeFile (t, {file}) =
      let
         val stm = TextIO.openOut file
         fun print s = TextIO.output (stm, s ^ "\n")
         val l = list_doc t []
      in
         app print l
       ; TextIO.closeOut stm
      end

   fun ppl doc = (pp doc; print "\n")
   fun ppSingleLine doc =
      let
         val l = easy_display nl_text str_list [] doc
      in
         app print l
      end

   fun render_style { mode, line_length, ribbons_per_line } doc =
      full_render
         mode
         line_length
         ribbons_per_line
         str_list [] doc

   (*** Derived ***)

   fun spaces n = text (indent n)

   structure Ops = struct
      val $ = text
      val % = hcat
      val %% = hsep
      val & = vcat
      val ~ = Empty
      val \ = space
      val \\ = spaces 3
   end
   open Ops

   type t = doc
   val string = text

   fun time t = $(Time.toString t)
   fun option _ NONE = $"NONE"
     | option p (SOME x) = %[$"SOME ", p x]
   fun pair (p1, p2) = %[lparen, p1, comma, \, p2, rparen]
   fun bool true = $"true"
     | bool false = $"false"

   (*** UI ***)

   val empty = Empty
   fun nest k p = mk_nest k (reduce_doc p)
   fun hang d1 n d2 = sep [d1, nest n d2]
   val commas = punctuate comma

   fun list l = bracket (%(commas l))
   fun tuple l = paren (%(commas l))

   (*** DEBUG ***)

   fun render doc = List.foldl op^ "" (list_doc doc [])

   val rec toString = fn
      Empty => "Empty"
    | Nil_above d => "Nil_above (" ^ toString (!d) ^ ")"
    | Text_beside (c, n, d) => "Text_beside (" ^ tdstr c ^ ", " ^ Int.toString n ^ ", " ^ toString (!d) ^ ")"
    | Nest (_, d) => "Nest (" ^ toString (!d) ^ ")"
    | Union (d1, d2) => "Union (" ^ toString (!d1) ^ ", " ^ toString (!d2) ^ ")"
    | No_doc => "No_doc"
    | Beside (d1, _, d2) => "Beside (" ^ toString (!d1) ^ ", " ^ toString (!d2) ^ ")"
    | Above (d1, _, d2) => "Above (" ^ toString (!d1) ^ ", " ^ toString (!d2) ^ ")"

   val _ = (render)

end

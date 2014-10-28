open Core.Std

(* -------------------------------------------------------------------------- *)
(*  Util                                                                      *)
(* -------------------------------------------------------------------------- *)

let (quot, rem) = 
  let quot_rem n k = 
    let (d, m) = (n / k, n mod k) in
    if d < 0 && m > 0 then (d+1, m-k)
    else (d, m)
  in
  let quot n k = fst (quot_rem n k) in
  let rem n k = snd (quot_rem n k) in
  quot, rem

let ( ** ) f g x = f (g x)

let indent n = String.make n ' '

(* -------------------------------------------------------------------------- *)
(*  Types                                                                     *)
(* -------------------------------------------------------------------------- *)

type text_details = 
  | Chr of char
  | Str of string
  | Pstr of string

type doc =
  | Empty
  | Nil_above of doc
  | Text_beside of text_details * int * doc
  | Nest of int * doc
  | Union of doc * doc
  | No_doc
  | Beside of doc * bool * doc
  | Above of doc * bool * doc

(* reduced doc, no top-level above or beside *)
type rdoc = doc

type mode = 
  | Page_mode
  | Zig_zag_mode
  | Left_mode
  | One_line_mode

type style = {
  mode : mode;
  line_length : int;
  ribbons_per_line : float;
}

let style = { line_length = 100; ribbons_per_line = 1.5; mode = Page_mode }

let space_text = Chr ' '

let nl_text = Chr '\n'

let nil_above_ p = Nil_above p

let text_beside_ s sl p = Text_beside (s, sl, p)

let sized_text l s = text_beside_ (Str s) l Empty

let zero_width_text = sized_text 0

let rec nil_beside g p = match p with
| Empty -> Empty
| Nest (_, p) -> nil_beside g p
| _ -> 
    if g then text_beside_ space_text 1 p
    else p

let reduce_ab = function
| Above (Empty, _, q) -> q
| Beside (Empty, _, q) -> q
| p -> p

let char c = text_beside_ (Chr c) 1 Empty

let semi  = char ';'
let colon = char ':'
let comma = char ','
let space = char ' '
let equals = char '='
let lparen = char '('
let rparen = char ')'
let lbrack = char '['
let rbrack = char ']'
let lbrace = char '{'
let rbrace = char '}'

let text s = text_beside_ (Str s) (String.length s) Empty

let ptext s = text_beside_ (Pstr s) (String.length s) Empty

let nest_ k p = Nest (k, p)

let union_ p q = Union (p, q)

let rec mk_nest k p = match p with
| Nest (k1, p) -> mk_nest (k + k1) p
| No_doc -> No_doc
| Empty -> Empty
| _ -> match k with
  | 0 -> p
  | k -> nest_ k p

let mk_union p q = match p with
| Empty -> Empty
| _ -> union_ p q

let empty = Empty

let is_empty = function
| Empty -> true
| _ -> false

let char c = text_beside_ (Chr c) 1 Empty

(* -------------------------------------------------------------------------- *)
(*  Vertical and horizontal composition                                       *)
(* -------------------------------------------------------------------------- *)

(* This is Haskell's beside_'.  It's not necessary to have both, and beside_'
   is better for partial application *)
let beside_ g p q = match p, q with
| _, Empty -> p
| Empty, _ -> q
| _ -> Beside (p, g, q)

(* See comment for beside_ *)
let above_ g p q = match p, q with
| _, Empty -> p
| Empty, _ -> q
| _ -> Above (p, g, q)

module Infix = struct 
  let ($$) p q = above_ false p q
  let ($+$) p q = above_ true p q
  let (<>) p q = beside_ false p q
  let (<+>) p q = beside_ true p q
end
open Infix

let int n = text (Int.to_string n)
let float n = text (Float.to_string n)
let quotes p = char '\'' <> p <> char '\''
let double_quotes p  = char '"' <> p <> char '"'
let parens p = char '(' <> p <> char ')'
let brackets p = char '[' <> p <> char ']'
let braces p = char '{' <> p <> char '}'

let punctuate p = function
| [] -> []
| d :: ds -> 
    let rec go d' = function 
    | [] -> [d']
    | e :: es -> (d' <> p) :: go e es
    in 
    go d ds

let hcat = reduce_ab ** List.fold_right ~init:empty ~f:(beside_ false)
let hsep = reduce_ab ** List.fold_right ~init:empty ~f:(beside_ true)
let vcat = reduce_ab ** List.fold_right ~init:empty ~f:(above_ false)

let rec nil_above_nest g k q = match q with
| Empty -> Empty
| Nest (k1, q) -> nil_above_nest g (k + k1) q
| _ -> 
    if not g && k > 0
    then text_beside_ (Str (indent k)) k q
    else nil_above_ (mk_nest k q)

let rec above_nest p g k q = match p with
| No_doc -> No_doc
| Union (p1, p2) -> union_ (above_nest p1 g k q) (above_nest p2 g k q)
| Empty -> mk_nest k q
| Nest (k1, p) -> nest_ k1 (above_nest p g (k - k1) q)
| Nil_above p -> nil_above_ (above_nest p g k q)
| Text_beside (s, sl, p) -> 
    let k1 = k - sl in
    let rest = match p with
    | Empty -> nil_above_nest g k1 q
    | _ -> above_nest p g k1 q
    in
    text_beside_ s sl rest
| Above _ | Beside _ -> failwith "Impossible" 

let rec reduce_doc p = match p with
| Beside (p, g, q) -> beside p g (reduce_doc q)
| Above (p, g, q) -> above p g (reduce_doc q)
| _ -> p

and beside p g q = match p with
| No_doc -> No_doc
| Union (p1, p2) -> union_ (beside p1 g q) (beside p2 g q)
| Empty -> q
| Nest (k, p) -> nest_ k (beside p g q)
| Beside (p1, g1, q1) -> 
    if Bool.equal g1 g then beside p1 g1 (beside q1 g q)
    else beside (reduce_doc p) g q
| Above _ -> beside (reduce_doc p) g q
| Nil_above p -> nil_above_ (beside p g q)
| Text_beside (s, sl, p) -> 
    let rest = match p with
    | Empty -> nil_beside g q
    | _ -> beside p g q
    in
    text_beside_ s sl rest

and above p g q = match p with
| Above (p, g1, q1) -> above p g1 (above q1 g q)
| Beside _ -> above_nest (reduce_doc p) g 0 (reduce_doc q)
| _ -> above_nest p g 0 (reduce_doc q)

(* -------------------------------------------------------------------------- *)
(*  Fill                                                                      *)
(* -------------------------------------------------------------------------- *)

let rec one_liner = function
| No_doc -> No_doc
| Empty -> Empty
| Nil_above _ -> No_doc
| Text_beside (s, sl, p) -> text_beside_ s sl (one_liner p)
| Nest (k, p) -> nest_ k (one_liner p)
| Union (p, _) -> one_liner p
| Above _ | Beside _ -> failwith "Impossible" 

let elide_nest = function
| Nest (_, d) -> d
| d -> d

let rec fill g = function
| [] -> empty
| p :: ps -> fill1 g (reduce_doc p) 0 ps

and fill1 g p k ys = match p with
| No_doc -> No_doc
| Union (p, q) -> union_ (fill1 g p k ys) (above_nest q false k (fill g ys))
| Empty -> mk_nest k (fill g ys)
| Nest (n, p) -> nest_ n (fill1 g p (k - n) ys)
| Nil_above p -> nil_above_ (above_nest p false k (fill g ys))
| Text_beside (s, sl, p) -> text_beside_ s sl (fill_nb g p (k - sl) ys)
| Above _ | Beside _ -> failwith "Impossible" 

and fill_nb g p k ys = match p, ys with
| Nest (_, p), _ -> fill_nb g p k ys
| Empty, [] -> Empty
| Empty, Empty :: ys -> fill_nb g Empty k ys
| Empty, y :: ys -> fill_nbe g k y ys
| _ -> fill1 g p k ys

and fill_nbe g k y ys = 
  let k1 = if g then k - 1 else k in
  nil_beside g (fill1 g ((elide_nest ** one_liner ** reduce_doc) y) k1 ys)

let fsep = fill true
let fcat = fill false

(* -------------------------------------------------------------------------- *)
(*  Separate                                                                  *)
(* -------------------------------------------------------------------------- *)

let rec sep_nb g p k ys = match p with
| Nest (_, p) -> sep_nb g p k ys
| Empty -> 
    let rest = if g then hsep ys else hcat ys in
    mk_union
      (one_liner (nil_beside g (reduce_doc rest)))
      (nil_above_nest true k (reduce_doc (vcat ys)))
| _ -> sep1 g p k ys

and sep1 g p k ys = match p with
| No_doc -> No_doc
| Union (p, q) -> union_ (sep1 g p k ys) (above_nest q false k (reduce_doc (vcat ys)))
| Empty -> mk_nest k (sep_x g ys)
| Nest (n, p) -> nest_ n (sep1 g p (k - n) ys)
| Nil_above p -> nil_above_ (above_nest p false k (reduce_doc (vcat ys)))
| Text_beside (s, sl, p) -> text_beside_ s sl (sep_nb g p (k - sl) ys)
| Above _ | Beside _ -> failwith "Impossible" 

and sep_x x = function
| [] -> empty
| p :: ps -> sep1 x (reduce_doc p) 0 ps

let sep = sep_x true
let cat = sep_x false

let nest k p = mk_nest k (reduce_doc p)
let hang d1 n d2 = sep [d1; nest n d2]

(* -------------------------------------------------------------------------- *)
(*  Selecting the best layout                                                 *)
(* -------------------------------------------------------------------------- *)

let rec non_empty_set = function
| No_doc -> false
| Union _ | Empty | Nil_above _ -> true
| Text_beside (_, _, p) | Nest (_, p) -> non_empty_set p
| Above _ | Beside _ -> failwith "Impossible" 

let first p q = if non_empty_set p then p else q

let rec fits n p = 
  if n < 0 then false 
  else match p with
  | No_doc -> false
  | Empty | Nil_above _ -> true
  | Text_beside (_, sl, p) -> fits (n - sl) p
  | Above _ | Beside _ | Union _ | Nest _ -> failwith "Impossible" 

let nicest1 w r sl p q = if fits ((min w r) - sl) p then p else q
let nicest w r p q = nicest1 w r 0 p q

let display the_mode page_width ribbon_width (<>) end_ doc =
  let gap_width = page_width - ribbon_width in
  let shift = quot gap_width 2 in
  let rec lay k = function
  | Nest (k1, p) -> lay (k + k1) p
  | Empty -> end_
  | Above _ | Beside _ | No_doc _ | Union _ -> failwith "Impossible" 
  | Nil_above p -> nl_text <> lay k p
  | Text_beside (s, sl, p) -> begin match the_mode with
    | Zig_zag_mode -> 
        if k >= gap_width 
        then
          nl_text <> (Str (String.make shift '/') <> (nl_text <> lay1 (k - shift) s sl p))
        else
          nl_text <> (Str (String.make shift '\\') <> (nl_text <> lay1 (k + shift) s sl p))
    | _ -> lay1 k s sl p
    end
  and lay1 k s sl p = Str (indent k) <> (s <> lay2 (k + sl) p)
  and lay2 k = function
  | Nil_above p -> nl_text <> lay k p
  | Text_beside (s, sl, p) -> s <> lay2 (k + sl) p
  | Nest (_, p) -> lay2 k p
  | Empty -> end_
  | Above _ | Beside _ | No_doc _ | Union _ -> failwith "Impossible" 
  in
  lay 0 doc

let best mode w0 r p0 = match mode with
| One_line_mode -> 
    let rec get = function
    | Empty -> Empty
    | No_doc -> No_doc
    | Nil_above p -> nil_above_ (get p)
    | Text_beside (s, sl, p) -> text_beside_ s sl (get p)
    | Nest (_, p) -> get p
    | Union (p, q) -> first (get p) (get q)
    | Above _ | Beside _ -> failwith "Impossible" 
    in
    get p0
| _ -> 
    let rec get w = function
    | Empty -> Empty
    | No_doc -> No_doc
    | Nil_above p -> nil_above_ (get w p)
    | Text_beside (s, sl, p) -> text_beside_ s sl (get1 w sl p)
    | Nest (k, p) -> nest_ k (get (w - k) p)
    | Union (p, q) -> nicest w r (get w p) (get w q)
    | Above _ | Beside _ -> failwith "Impossible" 
    and get1 w sl = function
    | Empty -> Empty
    | No_doc -> No_doc
    | Nil_above p -> nil_above_ (get (w - sl) p)
    | Text_beside (t, tl, p) -> text_beside_ t tl (get1 w (sl + tl) p)
    | Nest (k, p) -> get1 w sl p
    | Union (p, q) -> nicest1 w r sl (get1 w sl p) (get1 w sl q)
    | Above _ | Beside _ -> failwith "Impossible" 
    in
    get w0 p0

let cant_fail () = failwith "Impossible: cant_fail" 

(* Since I don't have a value of type 'a in ocaml, (cant_fail in the Haskell code)
   which would warn us when an invariant is broken, we use the dummy 'end_' value *)
let easy_display : text_details -> (text_details -> 'a -> 'a) -> 'a -> doc -> 'a =
  fun nl_space_text (<>) end_ doc -> 
    let rec lay doc no_doc = match doc with
    | No_doc -> no_doc
    | Union (_, q) -> lay q end_
    | Nest (_, p) -> lay p no_doc
    | Empty -> end_
    | Nil_above p -> nl_space_text <> lay p end_
    | Text_beside (s, _, p) -> s <> lay p no_doc
    | Above _ | Beside _ -> failwith "Impossible" 
    in
    lay doc end_

let full_render : mode -> int -> float -> (text_details -> 'a -> 'a) -> 'a -> doc -> 'a =
  fun the_mode line_length ribbons_per_line (<>) end_ doc -> match the_mode with
  | One_line_mode -> easy_display space_text (<>) end_ (reduce_doc doc)
  | Left_mode -> easy_display nl_text (<>) end_ (reduce_doc doc)
  | _ -> 
      let hacked_line_length = match the_mode with
      | Zig_zag_mode -> Int.max_value
      | _ -> line_length
      in
      let ribbon_length = Float.round_towards_zero_exn (Float.of_int line_length /. ribbons_per_line) in
      let best_doc = best the_mode hacked_line_length ribbon_length (reduce_doc doc) in
      display the_mode line_length ribbon_length (<>) end_ best_doc

let string_txt t s = match t with
| Chr c -> Char.to_string c ^ s
| Str s1 | Pstr s1 -> s1 ^ s

let show_doc doc rest = full_render Page_mode 100 1.5 string_txt rest doc

let render doc = show_doc doc ""

let render_style the_style doc = 
  full_render 
    the_style.mode
    the_style.line_length
    the_style.ribbons_per_line 
    string_txt "" doc

(* Simplified interface to regular expressions.
   Also contains a default instantiation of regular
   expressions (AWK syntax). *)

signature RE = sig
   exception RE of string

   structure R : REGEXP

   (* find regexp matchstring
      finds the first match in matchstring for regexp.
      The result is a function that allows access to
      the numbered subgroups.

      (properly staged) *)
   val find : string -> string -> (int -> string) option

   (* Same, but returns all of the matches. *)
   val findall : string -> string -> (int -> string) list

   (* hasmatch regexp matchstring
      true if matchstring contains any match for the regexp.
      If the regexp matches the empty string, always returns true. *)
   val hasmatch : string -> string -> bool
end

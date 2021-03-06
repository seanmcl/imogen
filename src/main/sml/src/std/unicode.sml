(*
Unicode UTF-8 encodings

\226\129\186 ---> ⁺

x207a : 10000001111010
  226 : 11100010
  129 : 10000001
  186 : 10111010

1110-0010 10-000001 10-111010

x2736 : 10011100110110

1110-0010 10-011100 10-110110

11100010 10011100 10110110
226      156      182

*)
structure Unicode = struct

   val Gamma     = "\206\147"
   val Delta     = "\206\148"
   val Theta     = "\206\152"
   val Lambda    = "\206\155"
   val Xi        = "\206\158"
   val Pi        = "\206\160"
   val Sigma     = "\206\163"
   val Phi       = "\206\166"
   val Psi       = "\206\168"
   val Omega     = "\206\169"

   val alpha     = "\206\177"
   val beta      = "\206\178"
   val gamma     = "\206\179"
   val delta     = "\206\180"
   val epsilon   = "\206\181"
   val zeta      = "\206\182"
   val eta       = "\206\183"
   val theta     = "\206\184"
   val iota      = "\206\185"
   val kappa     = "\206\186"
   val lambda    = "\206\187"
   val mu        = "\206\188"
   val nu_       = "\206\189"
   val xi        = "\206\190"
   val pi        = "\207\128"
   val rho       = "\207\129"
   val varsigma  = "\207\130"
   val sigma     = "\207\131"
   val tau       = "\207\132"
   val upsilon   = "\207\133"
   val phi       = "\207\134"
   val chi       = "\207\135"
   val omega     = "\207\136"

   val uexcl     = "\194\161"     (* ¡ : u00a1 *)
   val neg       = "\194\172"     (* ¬ : u00ac *)
   val cdot      = "\194\183"     (* · : u00b7 *)
   val supPlus   = "\226\129\186" (* ⁺ : u207a *)
   val supMinus  = "\226\129\187" (* ⁻ : u207b *)
   val par       = "\226\133\139" (* ⅋ : u214b *)
   val up        = "\226\134\145" (* ↑ : u2191 *)
   val mapsto    = "\226\134\166" (* ↦ : u21a6 *)
   val down      = "\226\134\147" (* ↓ : u21d3 *)
   val iff       = "\226\135\148" (* ⇔ : u21d4 *)
   val all       = "\226\136\128" (* ∀ : u2200 *)
   val exists    = "\226\136\131" (* ∃ : u2203 *)
   val neq       = "\226\137\160" (* ≠ : u2260 *)
   val equiv     = "\226\137\161" (* ≡ : u2261 *)
   val le        = "\226\137\164" (* ≤ : u2264 *)
   val leq       = le
   val sub       = "\226\138\130" (* ⊂ : u2282 *)
   val sup       = "\226\138\131" (* ⊃ : u2283 *)
   val oplus     = "\226\138\149" (* ⊕ : u2295 *)
   val otimes    = "\226\138\151" (* ⊗ : u2297 *)
   val circdot   = "\226\138\153" (* ⊙ : u2299 *)
   val stimes    = "\226\138\155" (* ⊗ : u229b *)
   val vdash     = "\226\138\162" (* ⊢ : u22a2 *)
   val top       = "\226\138\164" (* ⊤ : u22a4 *)
   val bot       = "\226\138\165" (* ⊥ : u22a5 *)
   val wedge     = "\226\136\167" (* ∧ : u22c0 *)
   val vee       = "\226\136\168" (* ∨ : u22c1 *)
   val circast   = "\226\141\159" (* ⍟ : u235f *)
   val circa     = "\226\147\144" (* ⓐ : u24d0 *)
   val circb     = "\226\147\145" (* ⓑ : u24d1 *)
   val circc     = "\226\147\146" (* ⓒ : u24d2 *)
   val circf     = "\226\147\149" (* ⓕ : u24d5 *)
   val circw     = "\226\147\166" (* ⓦ : u24e6 *)
   val ltriangle = "\226\151\131" (* ◃ : u25c3 *)
   val dia       = "\226\151\135" (* ◇ : u25c7 *)
   val blackdot  = "\226\151\143" (* ● : u25cf *)
   val circ      = "\226\151\166" (* ○ : u25e6 *)
   val bigcirc   = "\226\151\175" (* ◌ : u25ef *)
   val box       = "\226\151\187" (* ◻ : u25fb *)
   val star      = "\226\156\182" (* ✶ : u2736 *)
   val langle    = "\226\159\168" (* ⟨ : u27e8 *)
   val rangle    = "\226\159\169" (* ⟩ : u27e9 *)
   val modelsto  = "\226\164\135" (* ⤇ : u2907 *)

end

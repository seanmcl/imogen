
signature Util = sig
   type entails =
      { entailer : CFormula.t
      , entailed : CFormula.t
      , global:CFormula.t} -> bool

   val line : PP.t
end

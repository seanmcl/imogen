
structure Util :> Util = struct
   open PP.Ops

   type entails =
      { entailer : CFormula.t
      , entailed : CFormula.t
      , global:CFormula.t} -> bool

   val line = $(String.implode (List.replicate (80, #"-")))
end

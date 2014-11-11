-- FIXME: DOC
-- @ Signature 

module Imogen.Ordered 
  ( World
  , Frame
  , Pos(..)
  , Neg(..)
  , solver
  , translate
  , inferSig
  )
where

-- @ Imports

import Imogen.Ordered.Frame (Frame)
import Imogen.Ordered.OL (Pos(..), Neg(..), inferSig)
import Imogen.Ordered.Translate (translate)
import Imogen.Ordered.World (World, solver)

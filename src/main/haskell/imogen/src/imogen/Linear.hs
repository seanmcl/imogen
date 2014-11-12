-- FIXME: DOC
-- @ Signature 

module Imogen.Linear 
  ( World
  , Head
  , Frame
  , Pos(..)
  , Neg(..)
  , solver
  , translate
  , inferSig
  , listLolli
  , (◃), (⊸)
  )
where

-- @ Imports

import Imogen.Linear.Head (Head)
import Imogen.Linear.Frame (Frame, (◃))
import Imogen.Linear.LL (Pos(..), Neg(..), inferSig, (⊸), listLolli)
import Imogen.Linear.Translate (translate)
import Imogen.Linear.World (World, solver)

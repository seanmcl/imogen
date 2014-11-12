
-- @ Pragmas

{-# OPTIONS_GHC -fno-warn-orphans  #-} 

-- PP.Doc is an orphan instance of Monoid.  Don't warn about it.

-- @ Signature

module Imogen.Util.Print 
  ( module Text.PrettyPrint.HughesPJ
  , module Imogen.Util.Print.Print
  ) 
where

-- @ Imports

import Text.PrettyPrint.HughesPJ hiding (render)
import Imogen.Util.Print.Print 


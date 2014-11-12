
-- @ Pragmas

-- Orphan warning for Show instance.  Ignore it.

{-# OPTIONS_GHC -fno-warn-orphans #-} 

-- @ Signature

module Imogen.Util.Parse
  ( module Text.ParserCombinators.Parsec
  , module Text.ParserCombinators.Parsec.Expr
  , module Imogen.Util.Parse.Parse
  ) 
where

-- @ Imports

import Prelude
import Text.ParserCombinators.Parsec hiding (parse)
import Text.ParserCombinators.Parsec.Expr 
import Imogen.Util.Parse.Parse

instance Show Assoc where
  show AssocLeft = "AssocLeft"
  show AssocRight = "AssocRight"
  show AssocNone = "AssocNone"


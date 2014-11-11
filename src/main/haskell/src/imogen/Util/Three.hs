
-- @ Signature

module Imogen.Util.Three
  ( Three(..)
  , opp
  , (|||), (&&&)
  )
where

-- @ Imports

import Prelude 
import qualified Imogen.Util.Lex as Lex
import qualified Imogen.Util.Parse as P
import Imogen.Util.Parse (Parse, (<|>))
import qualified Imogen.Util.Print as PP
import Imogen.Util.Print (Print)

-- @ Three

data Three = Yes
           | No
           | Unsure
  deriving (Eq, Ord, Show)

opp :: Three -> Three
opp Yes = No
opp No = Yes
opp Unsure = Unsure

(|||) :: Three -> Three -> Three
t1 ||| t2 = case (t1, t2) of
  (Yes, _) -> Yes
  (_, Yes) -> Yes
  (Unsure, _) -> Unsure
  (_, Unsure) -> Unsure
  (No, No) -> No

(&&&) :: Three -> Three -> Three
t1 &&& t2 = case (t1, t2) of
  (No, _) -> No
  (_, No) -> No
  (Unsure, _) -> Unsure
  (_, Unsure) -> Unsure
  (Yes, Yes) -> Yes

instance Print Three where
  pPrint = PP.text . show

instance Parse Three where
  parser = (Lex.reserved "Yes" >> return Yes)
       <|> (Lex.reserved "No" >> return No)
       <|> (Lex.reserved "Unsure" >> return Unsure)

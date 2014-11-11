
-- @ Signature

module Imogen.Misc 
  ( -- * Version string
    version
    -- * Prover IO
  , Input(..)
  , Output(..)     
  , isYes
  , isNo
  , three
    -- * Infix parsing and printing
  , consts
  , infixFuncs
  , infixBinopStrings
  , infixBinops
  )
where

-- @ Imports

import Prelude 
import qualified Data.List as List
import Imogen.Atom (Atom(Rel))
import qualified Imogen.Linear as Linear
import qualified Imogen.Pred as Pred
import Imogen.Proof (Proof)
import qualified Imogen.Term as T
import Imogen.Term (Term)
import qualified Imogen.Util.Parse as P
import qualified Imogen.Util.Print as PP
import Imogen.Util.Print (Print(..))
import qualified Imogen.Util.Three as Three
import Imogen.Util.Three (Three)

-- @ Version

-- | Version string.
version :: String
version = "1.0"

-- @ IO 

-- | Input.
data Input = String String | File FilePath
  deriving (Eq, Show)

instance Print Input where 
  pPrint (String s) = PP.text s
  pPrint (File f) = PP.text f

-- | Output.
data Output = Yes Proof
            | No
            | Maybe

isYes :: Output -> Bool
isYes (Yes _) = True
isYes _ = False

isNo :: Output -> Bool
isNo No = True
isNo _ = False

three :: Output -> Three
three (Yes _) = Three.Yes
three No = Three.No
three Maybe = Three.Unsure

-- @ Parsing and Printing

-- | Constant symbols.
consts :: [String]
consts = [ "ε", "ع" ]

{- | 
Infix function symbols, sorted in decreasing order of precedence.
Used by "Imogen.Parse" and "Imogen.Print".  It can't be in one
of those files because of module dependency cycles.
-} 
infixFuncs :: [(String, (Int, P.Assoc))]
infixFuncs = List.sortBy (\(_, (n, _)) (_, (m, _)) -> compare m n) 
  [ ("⋆", (50, P.AssocRight))
  , ("·", (50, P.AssocRight))
  , ("⊛", (45, P.AssocLeft))
  ]

{- | 
Infix symbols can have multiple concrete syntax representations.
e.g. the user can enter equality as be both = and ≡. 
-} 
infixBinopStrings :: [String]
infixBinopStrings = concatMap fst infixBinops

infixBinops :: [([String], Term -> Term -> Atom)]
infixBinops = 
  [ (["=", "≡"], \s t -> Rel (Pred.make "=") [s, t])
  , (["!=", "/="], \s t -> Rel (Pred.make "≠") [s, t])
  , (["<", "≺"], \s t -> Rel (Pred.make "<") [s, t])
  , (["≤", "<="], \s t -> Rel (Pred.make "≤") [s, t])
  , (["◃"], \s t -> T.decode s Linear.◃ T.decode t)
  ]



-- * Signature 

module Imogen.Test.Formula

where

-- * Imports

import Prelude 
import Imogen.PFormula (Neg)
import Imogen.Sig (Σ)
import qualified Imogen.Sig as Sig
import Imogen.Sort (Sort(..))
import qualified Imogen.Util.Print as PP
import qualified Imogen.Util.Parse as P

-- * Tests

sig :: Σ 
sig = Sig.fromList [ ("p", Rel [])
                   , ("q", Rel [])
                   , ("r", Rel [World])
                   , ("P", Rel [])
                   , ("p2", Rel [Frame, Frame])
                   , ("q1", Rel [U])
                   , ("Q", Rel [U])
                   , ("a", PLProp)
                   , ("a'", PLProp)
                   ]

formulas :: [Neg]
formulas = map P.parse 
  [ "⊤"
  , "⊥"
  , "p"
  , "p()"
  , "r(X)"
  , "r(ε)"
  , "r(X)"
  , "P"
  , "p ∧ q"
  , "p ∨ q"
  , "p ⊃ q"
  , "∀ (X : U). p"
  , "q1(c)"
  , "Q(c)"
  , "p ∧ Q(c)"
  , "∃ (X : Frame). p2(X, X)"
  , "r(ε ⋆ ε)"
  , "φ ◃ α ⋆ ε"
  , "a ⓦ α"
  , "a' ⓕ φ"
  , "φ ⊛ β ⊛ α ◃ γ"
  ]

main :: IO ()
main = do putStrLn "Parsed"
          putStrLn ""
          mapM_ (PP.putStrLn . PP.pPrint) formulas
          putStrLn ""
          putStrLn "Normal"
          putStrLn ""
          mapM_ (PP.putStrLn . C.pp' sig . C.normalize' sig) formulas


-- @ Signature 

module Imogen.Test.Modal
  ( main
  , tests
  , formulasK
  , formulasD
  , formulasT
  )
where

-- @ Imports

import Imogen.Util.Prelude 
import qualified Control.Monad.State as State
import qualified Imogen.Ctx as Ctx
import qualified Imogen.Misc as Misc
import qualified Imogen.Modal as Modal
import Imogen.Modal (Mode(..))
import Imogen.Print (pPrint)
import qualified Imogen.Prover as Prover
import qualified Imogen.Sig as Sig
import Imogen.Sig (Σ)
import qualified Imogen.Util.Print as PP
import qualified Imogen.Util.Parse as P
import qualified Imogen.Util.Test as Test
import Imogen.Util.Test (Test(..), (~:), (@?=))
import Imogen.Util.Three (Three(..))
import qualified System.IO.UTF8 as S

-- @ Sig

sig :: Σ 
sig = P.parse 
  " { p, q, r : U }"

-- @ K

formulasK :: [(String, String, Three)]
formulasK = 
  [ ("k.1",  "⊤", Yes)
  , ("k.2",  "□ p ⊃ □ (p ⊃ q) ⊃ □ q", Yes)
  , ("k.3",  "□ (p ∧ q) ⊃ (□ p) ∧ (□ q)", Yes)
  , ("k.4",  "(□ p) ∧ (□ q) ⊃ □ (p ∧ q)", Yes)
  , ("k.5",  "(□ p) ∨ (□ q) ⊃ □ (p ∨ q)", Yes)
  , ("k.6",  "□ (p ∨ q) ⊃ □ p ∨ □ q", No)
  , ("k.7",  "(□ p) ∨ (□ q) ⊃ □ (p ∧ q)", No)
  , ("k.8",  "◇ (p ∨ q) ⊃ ◇ p ∨ ◇ q", Yes)
  , ("k.9",  "◇ (p ⊃ q) ⊃ (□ p ⊃ ◇ q)", Yes)
  -- Intuitionistically false
  , ("k.10", "◇ (p ⊃ q) ⊂ (□ p ⊃ ◇ q)", No) 
  , ("k.11", "□ (p ∨ q) ⊃ (□ p ∨ ◇ q)", No) 
  , ("k.12", "□ (p ∨ q) ⊂ (□ p ∨ ◇ q)", No) 
  -- T axiom
  , ("k.13", "□ p ⊃ p", No) 
  , ("k.14", "p ⊃ ◇ p", No) 
  ]

-- @ D

formulasD :: [(String, String, Three)]
formulasD = 
  [ ("d.1", "□ p ⊃ ◇ p", Yes) 
  , ("d.2", "◇ (p ⊃ p)", Yes) 
  ]

-- @ T

formulasT :: [(String, String, Three)]
formulasT = 
  [ ("t.1", "□ p ⊃ p", Yes) 
  , ("t.2", "p ⊃ ◇ p", Yes) 
  -- Intuitionistically false
  , ("t.3", "◇ (p ⊃ □ p)", No) 
  -- D axiom
  , ("t.4", "□ p ⊃ ◇ p", Yes) 
  ]

-- @ S4 

formulasS4:: [(String, String, Three)]
formulasS4 = formulasK ++
  [ ("s4.3", "□ p ⊃ □ □ p", Yes)
  , ("s4.4", "□ p ⊃ p", Yes)
  , ("s4.5", "□ P ⊃ P", Yes)
  , ("s4.6", "p ⊃ □ p", Yes)
  , ("s4.7", "□ ⊤ ⇔ ⊤", Yes)
  , ("s4.8", "□ (p ∧ q) ⇔ □ p ∧ □ q", Yes)
  , ("s4.9", "◇ ⊥ ⇔ ⊥", Yes)
  , ("s4.10", "◇ (p ∨ q) ⇔ ◇ p ∨ ◇ q", Yes)
  , ("s4.11", "□ (p ⊃ q) ⊃ (◇ p ⊃ ◇ q)", Yes)
  , ("s4.12", "¬ ◇ ⊥", Yes)
  , ("s4.12", "◇ (p ∨ q) ⊃ ◇ p ∨ ◇ q", Yes)
  , ("s4.13", "(◇ p ⊃ □ q) ⊃ □ (p ⊃ q)", Yes)
  , ("s4.14", "p ⊃ □ ◇ p", Yes)
  , ("s4.15", "◇ p ⊃ □ ◇ p", Yes)
  , ("s4.16", "◇ □ p ⊃ □ ◇ p", Yes)

-- Barcan formula

  , ("barcan", "(∀ X. □ p(X)) ⊃ (□ ∀ X. p(X))", Yes)

-- Distinguished between Pfenning-Davies and Simpson style.
-- (Jason said to tattoo it on my arm...)
-- False in PD, True in Simpson.

  , ("davies-tattoo", "(◇ a ⊃ □ b) ⊃ □ (a ⊃ b)", Yes)

  ]

testsS4 :: Test
testsS4 = "S4" ~: map mkTest formulasS4
  where mkTest (name, f, res) = name ~: do
          res' <- Prover.modalProve S4 (Misc.String f)
          Misc.three res' @?= res

-- @ S5 

formulasS5:: [(String, String, Three)]
formulasS5 = formulasS4 ++
  [ ("s5.1", "p ⊃ □ ◇ p", Yes)
  ]

testsS5 :: Test
testsS5 = "S5" ~: map mkTest formulasS5
  where mkTest (name, f, res) = name ~: do
          res' <- Prover.modalProve S5 (Misc.String f)
          Misc.three res' @?= res

-- @ Top

tests :: Test
tests = "Modal" ~: [ testsS4, testsS5 ]

main :: IO ()
main = do S.putStrLn "Modal"
          θs4 :: [Bool] <- mapM (prv S4) formulasS4
          mapM_ (PP.putStrLn . pPrint sig Ctx.empty) θs4
          θs5 :: [Bool] <- mapM (prv S5) formulasS5
          mapM_ (PP.putStrLn . pPrint sig Ctx.empty) θs5
 where 
  prv :: Mode -> (String, String, Three) -> IO Bool
  prv mode (_, f, b) = Prover.modalProve mode (Misc.String f) >>= return . (== b) . Misc.three


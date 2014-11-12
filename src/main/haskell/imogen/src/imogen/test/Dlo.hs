
-- @ Signature

module Imogen.Test.Dlo
  ( main
  , tests
  , formulas
  )
where

-- @ Imports

import Imogen.Util.Prelude 
import qualified Control.Monad.State as State
import qualified Imogen.Class as Class
import qualified Imogen.Ctx as Ctx
import qualified Imogen.Func as Func
import Imogen.PFormula (Neg)
import qualified Imogen.Prover as Prover
import qualified Imogen.Sig as Sig
import Imogen.Sig (Σ)
import Imogen.Sort (Sort(..), Base(..))
import qualified Imogen.Util.Print as PP
import qualified Imogen.Util.Parse as P
import qualified Imogen.Util.Test as Test
import Imogen.Util.Test (Test(..), (~:), (@?=))
import Imogen.Util.Three (Three(..))
import qualified System.IO.UTF8 as S

-- @ Tests

formulas:: [(String, Neg, Three)]
formulas = 
  let parse (s, a, b) = ( s, P.parse a, b ) in
  map parse 
  [ ("t1", "1 < 2", Yes)
  , ("t2", "∀ (X : Real). ¬ (X < X)", Yes)
  , ("t3", "∀ (X : Real). X < X", No)
  , ("t4", "∀ (X, Y, Z : Real). X < Y ⊃ Y < Z ⊃ X < Z", Yes)
  , ("t5", "(∀ (X, Y : Real). X < Y ⊃ p(X) ⊃ p(Y)) ⊃ (∀ (X, Y, Z : Real). X < Y ⊃ Y < Z ⊃ p(X) ⊃ p(Z))", Yes)
  , ("t6", "(∀ (X, Y : Real). X < Y ⊃ p(X) ⊃ p(Y)) ⊃ (∀ (X, Y, Z, W : Real). X < Y ⊃ Y < Z ⊃ Z < W ⊃ p(X) ⊃ p(W))", Yes)
  , ("t7", "(∀ (X, Y : Real). X < Y ⊃ p(X) ⊃ p(Y)) ⊃ (∀ (X, Y : Real). p(X) ⊃ X < Y ⊃ ∃ (W : Real). X < W ∧ W < Y ∧ p(W))", Yes)
  , ("t8", "(∀ (X, Y : Real). X < Y ⊃ p(X) ⊃ p(Y)) ⊃ p(c) ⊃ ∃ (Y : Real). c < Y ∧ p(Y)", Yes)
  ]

-- @ Top

sig :: Σ 
sig = Sig.fromList [ (Sig.Func $ Func.make "p", Fun [] U)
                   , (Sig.Func $ Func.make "q", Fun [] U)
                   , (Sig.Func $ Func.make "r", Fun [] U)
                   , (Sig.Func $ Func.make "d", Fun [] U)
                   , (Sig.Func $ Func.make "n", Fun [] U)
                   , (Sig.Func $ Func.make "l", Fun [] U)
                   , (Sig.Func $ Func.make "P", Fun [] U)
                   , (Sig.Func $ Func.make "Q", Fun [] U)
                   , (Sig.Func $ Func.make "R", Fun [] U)
                   , (Sig.Func $ Func.make "A", Fun [] U)
                   , (Sig.Func $ Func.make "B", Fun [] U)
                   , (Sig.Func $ Func.make "C", Fun [] U)
                   , (Sig.Func $ Func.make "p1", Fun [U] U)
                   , (Sig.Func $ Func.make "p2", Fun [U, U] U)
                   ] 

tests :: Test
tests = "Dlo" ~: map mkTest formulas
  where mkTest (name, f, res) = name ~: do
          res' <- Prover.dloProve sig f
          res' @?= res

main :: IO ()
main = do S.putStrLn "Dlo"
          θs :: [Bool] <- mapM prv formulas
          mapM_ (PP.putStrLn . Class.pp sig Ctx.empty) θs
 where 
  prv :: (String, Neg, Three) -> IO Bool
  prv (_, f, b) = Prover.dloProve sig f >>= \b' -> return (b == b')



-- @ Signature

module Imogen.Test.Linear
  ( main
  , tests
  , formulas
  )
where

-- @ Imports

import Imogen.Util.Prelude 
import qualified Control.Monad.State as State
import qualified Imogen.Ctx as Ctx
import qualified Imogen.Misc as Misc
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

-- @ Tests

formulas:: [(String, String, Three)]
formulas = 
  [ ("t1", "⊤", Yes)
  , ("t2", "1", Yes)
  , ("t3", "0", No)
  , ("t4", "p ⊸ p", Yes)
  , ("t5", "p ⊸ q ⊸ p", No)
  , ("t6", "p ⊗ q ⊸ q ⊗ p", Yes)
  , ("t7", "p ⊗ q ⊗ p ⊸ q ⊗ p", No)
  , ("t8", "p ⊸ q ⊕ p", Yes)
  , ("t9", "p ⊸ p ⊕ q", Yes)
  , ("t10", "p ⊕ q ⊸ q ⊕ p", Yes)
  , ("t11", "p ⊕ q ⊸ q ⊕ p", Yes)
  , ("t12", "⊤ & ⊤", Yes)
  , ("t13", "1 & 1", Yes)
  , ("t14", "p ⊸ p & ⊤", Yes)
  , ("t15", "P & Q ⊸ P", Yes)
  , ("t16", "P & Q ⊸ Q", Yes)
  , ("t17", "P & Q ⊸ Q & P", Yes)
  , ("t18", "p & q ⊸ p", Yes)
  , ("t19", "p & q ⊸ q", Yes)
  , ("t20", "p & q ⊸ q & p", Yes)
  , ("t21", "p & Q ⊸ Q & p", Yes)
  , ("t22", "p & q ⊸ Q & p", No)
  , ("t23", "p ⊸ 1", No)
  , ("t24", "⊤ ⊸ 1", No)
  , ("t25", "(p ⊸ q ⊸ r) ≗ (p ⊗ q ⊸ r)", Yes)
  , ("t26", "p ⊸ p ⊗ p", No)
  , ("t27", "p ⊃ p ⊗ p", Yes)
    -- FP
  , ("fp1", "A ⊸ A", Yes)
  , ("fp2", "(A ⊸ B) ⊸ (A ⊸ B)", Yes)
  , ("fp3", "A ⊸ B ⊸ A", No)
  , ("fp4", "A ⊸ A ⊸ A", No)
  , ("fp5", "(A ⊸ B ⊸ C) ⊸ (B ⊸ A ⊸ C)", Yes)
  , ("fp6", "A ⊸ B ⊸ A ⊗ B", Yes)
  , ("fp7", "A ⊗ B ⊸ (A ⊸ B ⊸ C) ⊸ C", Yes)
  , ("fp8", "A ⊗ B ⊸ A", No)
  , ("fp9", "A ⊗ B ⊸ B", No)
  , ("fp10", "A ⊗ B ⊸ B ⊗ A", Yes)
  , ("fp11", "A ⊗ A ⊸ A", No)
  , ("fp12", "A ⊸ A ⊗ A", No)
  , ("fp13", "A ⊗ (B ⊗ C) o--o (A ⊗ B) ⊗ C", Yes)
  , ("fp14", "(A ⊗ B ⊸ C) o--o (A ⊸ B ⊸ C)", Yes)
  , ("fp15", "(A ⊸ B ⊗ C) ⊸ ((A ⊸ B) ⊗ (A ⊸ C))", No)
  , ("fp16", "(A ⊸ B ⊗ C) o- ((A ⊸ B) ⊗ (A ⊸ C))", No)
  , ("fp17", "1", Yes)
  , ("fp18", "1 ⊸ C ⊸ C", Yes)
  , ("fp19", "(1 ⊸ A) o--o A", Yes)
  , ("fp20", "(A ⊗ 1) o--o A", Yes)
  , ("fp21", "(1 ⊗ A) o--o A", Yes)
  , ("fp22", "A & B ⊸ A & B", Yes)
  , ("fp23", "A & B ⊸ A", Yes)
  , ("fp24", "A & B ⊸ B", Yes)
  , ("fp24", "A ⊸ B ⊸ A & B", No)
  , ("fp25", "A & B ⊸ B & A", Yes)
  , ("fp25", "A & A o--o A", Yes)
  , ("fp26", "(A & B) & C o--o A & (B & C)", Yes)
  , ("fp27", "A & 1 ⊸ A", Yes)
  , ("fp28", "A ⊸ A & 1", No)
  , ("fp29", "(A ⊸ B & C) o--o ((A ⊸ B) & (A ⊸ C))", Yes)
  , ("fp30", "((A & B) -o C) -o (A -o (B -o C))", No)
  , ("fp30", "((A & B) -o C) o- (A -o (B -o C))", No)
  , ("fp31", "(A ⊗ (B & C)) -o ((A ⊗ B) & (A ⊗ C))", Yes)
  , ("fp32", "(A ⊗ (B & C)) o- ((A ⊗ B) & (A ⊗ C))", No)
  , ("fp33", "⊤ ⊸ ⊤", Yes)
  , ("fp34", "(A ⊸ ⊤) o--o ⊤", Yes)
  , ("fp35", "A ⊸ ⊤ ⊸ A", No)
  , ("fp36", "A & ⊤ o--o A", Yes)
  , ("fp37", "⊤ & A o--o A", Yes)
  , ("fp38", "A ⊗ ⊤ ⊸ A", No)
  , ("fp39", "A ⊸ A ⊗ ⊤", Yes)
  , ("fp40", "A ⊸ A ⊕ B", Yes)
  , ("fp41", "B ⊸ A ⊕ B", Yes)
  , ("fp42", "A ⊕ B ⊸ ((A ⊸ C) & (B ⊸ C)) ⊸ C", Yes)
  , ("fp43", "A ⊕ B ⊸ A", No)
  , ("fp44", "A ⊕ B ⊸ B", No)
  , ("fp45", "A ⊕ B ⊸ B ⊕ A", Yes)
  , ("fp46", "A ⊕ A o--o A", Yes)
  , ("fp47", "(A ⊕ B) ⊕ C o--o A ⊕ (B ⊕ C)", Yes)
  , ("fp48", "(A ⊕ B -o C) o--o ((A -o C) & (B -o C))", Yes)
  , ("fp49", "(A -o (B ⊕ C)) -o ((A -o B) ⊕ (A -o C))", No)
  , ("fp50", "(A -o (B ⊕ C)) o- ((A -o B) ⊕ (A -o C))", Yes)
  , ("fp51", "(A ⊗ (B ⊕ C)) o--o (A ⊗ B ⊕ A ⊗ C)", Yes)
  , ("fp52", "(A & (B ⊕ C)) -o ((A & B) ⊕ (A & C))", No)
  , ("fp53", "(A & (B ⊕ C)) o- ((A & B) ⊕ (A & C))", Yes)
  , ("fp54", "A ⊕ 1 ⊸ 1", No)
  , ("fp55", "1 ⊸ A ⊕ 1", Yes)
  , ("fp56", "A ⊕ ⊤ o--o ⊤", Yes)
  , ("fp57", "0 ⊸ ⊤ ⊸ C", Yes)
  , ("fp58", "(0 ⊸ A) o--o ⊤", Yes)
  , ("fp59", "0 ⊗ A o--o 0", Yes)
  , ("fp60", "0 & A o--o 0", Yes)
  , ("fp61", "(A ⊸ C) ⊸ !A ⊸ C", Yes)
  , ("fp62", "(!A ⊸ (!A ⊸ C)) ⊸ (!A ⊸ C)", Yes)
  , ("fp63", "C ⊸ !A ⊸ C", Yes)
  , ("fp64", "!A ⊸ A", Yes)
  , ("fp65", "! !A o--o !A", Yes)
  , ("fp66", "!(A ⊸ B) ⊸ (!A ⊸ !B)", Yes) 
  , ("fp67", "!(A ⊸ B) o- (!A ⊸ !B)", No) 
  , ("fp68", "!(A ⊗ B) ⊸ (!A ⊗ !B)", No) 
  , ("fp69", "!(A ⊗ B) o- (!A ⊗ !B)", Yes) 
  , ("fp70", "!(A ⊗ B) ⊸ (!A & !B)", No) 
  , ("fp71", "!(A ⊗ B) o- (!A & !B)", No) 
  , ("fp72", "!(A & B) o--o (!A ⊗ !B)", Yes) 
  , ("fp73", "!(A & B) -o (!A & !B)", Yes) 
  , ("fp74", "!(A & B) o- (!A & !B)", No) 
  , ("fp75", "!1 o--o 1", Yes)
  , ("fp76", "! ⊤ -o ⊤", Yes)
  , ("fp77", "⊤ -o !⊤", No)
  , ("fp78", "!0 o--o 0", Yes)
  , ("fp79", "!(A ⊕ B) ⊸ (!A ⊕ !B)", No)
  , ("fp79", "!(A ⊕ B) o- (!A ⊕ !B)", Yes)
  -- First order
  , ("fo1", "∀ X. p1(X) ⊸ p1(X)", Yes)
  , ("fo2", "∀ X Y. p2(X, Y) ⊸ p2(X, Y)", Yes)
  , ("fo3", "∀ X Y. p2(X, Y) ⊸ p2(Y, X)", No)
  , ("fo4", "∀ X. p1(X) ⊸ p1(X) ⊸ p1(X)", No)
  , ("fo5", "∀ X. p1(X) ⊸ p1(X) ⊸ p1(X) ⊗ p1(X)", Yes)
  , ("fo6", "(∃ Y. ∀ X. p2(X, Y)) ⊸ (∀ X. ∃ Y. p2(X, Y))", Yes)
  , ("fo7", "(∀ X. ∃ Y. p2(X, Y)) ⊸ (∃ Y. ∀ X. p2(X, Y))", No)
  -- Change
  , ("c0", "(d ⊸ n ⊗ n) ⊃ (d ⊸ n ⊗ n)", Yes)
  , ("c1", "(l ⊸ q ⊗ q ⊗ q ⊗ q) ⊃ (l ⊸ q ⊗ q ⊗ q ⊗ q)", Yes)
  -- , ("c2", "(n ⊸ p ⊗ p ⊗ p ⊗ p ⊗ p) ⊃ (n ⊸ p ⊗ p ⊗ p ⊗ p ⊗ p)", Yes)
  -- , ("c3", "(d ⊸ n ⊗ n) ⊃ (n ⊸ p ⊗ p ⊗ p ⊗ p ⊗ p) ⊃ (d ⊸ n ⊗ p ⊗ p ⊗ p ⊗ p ⊗ p)", Yes)
  ]

{-  
"(l ⊸ q ⊗ q ⊗ q ⊗ q) ⊃ (q ⊸ d ⊗ d ⊗ n) ⊃ (d ⊸ n ⊗ n) ⊃ (n ⊸ p ⊗ p ⊗ p ⊗ p ⊗ p) ⊃ (l ⊸ q ⊗ q ⊗ d ⊗ d ⊗ d ⊗ n ⊗ n ⊗ n ⊗ p ⊗ p ⊗ p ⊗ p ⊗ p)"
"(n ⊸ p ⊗ p ⊗ p ⊗ p ⊗ p) ⊃ (n ⊸ p ⊗ p ⊗ p ⊗ p ⊗ p)"
-} 

-- @ Top

sig :: Σ 
sig = P.parse 
  "  { p, q, r, d, n, l, P, Q, R, A, B, C : U  \
  \  , p1   : U -> U                           \
  \  , p2   : U -> U -> U                      \
  \  }                                         "

tests :: Test
tests = "Linear" ~: map mkTest formulas
  where mkTest (name, f, res) = name ~: do
          res' <- Prover.linearProve (Misc.String f)
          Misc.three res' @?= res

main :: IO ()
main = do S.putStrLn "Linear"
          θs :: [Bool] <- mapM prv formulas
          mapM_ (PP.putStrLn . pPrint sig Ctx.empty) θs
 where 
  prv :: (String, String, Three) -> IO Bool
  prv (_, f, b) = Prover.linearProve (Misc.String f) >>= return . (== b) . Misc.three



-- @ Signature

module Imogen.Test.Ordered
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
  , ("t4", "p", No)
  , ("t5", "p >-> p", Yes)
  , ("t6", "p ->> p", Yes)
  , ("t7", "p >-> p >-> p", No)
  , ("t8", "p ->> p ->> p", No)
  , ("t9", "(p ->> p) ∧ (q >-> q)", Yes)
  , ("t10", "(p ● q ->> p ● q)", Yes)
  , ("t10'", "(p ● q >-> p ● q)", Yes)
  , ("t11", "(p ● q ->> q ● p)", No)
  , ("t12", "p >-> q >-> q ● p", Yes)
  , ("t14", "p ->> q ->> p ● q", Yes)
  , ("t15", "! ⊤", Yes)
  , ("t16", "! 1", Yes)
  , ("t17", "¡ ⊤", Yes)
  , ("t18", "! p ->> ! q ->> p ∧ q", Yes)
  , ("t19", "a ● (a >-> b) ->> b", Yes)
  , ("t20", "(a ->> b) ● a ->> b", Yes)
  , ("t21", "a >-> (a ->> b) >-> b", Yes)
  , ("t22", "(a >-> b >-> c) >-> (b ● a >-> c)", Yes)
  , ("t23", "(a ->> b ->> c) ->> (a ● b ->> c)", Yes)
  , ("t24", "a >-> b >-> c >-> d >-> d ● c ● b ● a", Yes)
  , ("t25", "a ->> b ->> c ->> d ->> a ● b ● c ● d", Yes)
  , ("t26", "a ∧ b >-> b ∧ a", Yes)
  , ("t27", "(a ->> b ->> c ->> d) ● a ● b ● c ->> d", Yes)
  , ("t28", "b ● (a ->> b >-> c) ● a ->> c", Yes)
  , ("t29", "(N ● (N >-> (N ->> S)) ● N) ->> S", Yes)
  , ("t30", "(a ->> b) ● a ● (b >-> c) ->> c", Yes)
  , ("t31", "(N ● (N ->> N >-> S) ● (N ->> N) ● N) >-> S", Yes)
  , ("t32", "(A ● (A >-> B) ● (B >-> C)) >-> C", Yes)
  , ("t33", "(A ● (A >-> B) ● (B >-> C) ● (C >-> D)) ->> D", Yes)
  , ("t34", "(m ● n ● (n >-> m >-> n ->> s) ● n) ->> s", Yes)
  , ("t35", "(n ● (n >-> n ->> s) ● m ● (m >-> n)) ->> s", Yes)
  , ("t36", "(n ● (n >-> n ->> s) ● (n ->> n) ● n) ->> s", Yes)
    -- First order
  , ("fo1", "(∃ Y. ∀ X. p2(X, Y)) >-> (∀ X. ∃ Y. p2(X, Y))", Yes)
  , ("fo2", "(∀ X. ∃ Y. p2(X, Y)) >-> (∃ Y. ∀ X. p2(X, Y))", No)
  ]

-- Bugs 

-- imogen ordered "(n ● (n ->> n ->> s) ● (n ->> n) ● n) ->> s"

-- @ Top

sig :: Σ 
sig = P.parse 
  "  { p, q, r, d, n, l, P, Q, R, a, b, c, d, N, S, s, A, B, C, D, m : U  \
  \  , p1   : U -> U                                                      \
  \  , p2   : U -> U -> U                                                 \
  \  }                                                                    "

tests :: Test
tests = "Ordered" ~: map mkTest formulas
  where mkTest (name, f, res) = name ~: do
          res' <- Prover.orderedProve (Misc.String f)
          Misc.three res' @?= res

main :: IO ()
main = do S.putStrLn "Ordered"
          θs :: [Bool] <- mapM prv formulas
          mapM_ (PP.putStrLn . pPrint sig Ctx.empty) θs
 where 
  prv :: (String, String, Three) -> IO Bool
  prv (_, f, b) = Prover.orderedProve (Misc.String f) >>= return . (== b) . Misc.three


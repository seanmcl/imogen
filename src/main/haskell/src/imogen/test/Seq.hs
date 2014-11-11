
-- @ Signature

module Imogen.Test.Seq
  ( main
  , tests
  )
where

-- @ Imports

import Imogen.Util.Prelude 
import qualified Control.Monad.State as State
import qualified Imogen.Class as Class
import Imogen.CSubst (Θ)
import qualified Imogen.Ctx as Ctx
import Imogen.Ctx (Γ)
import qualified Imogen.Global as Global
import qualified Imogen.Modal as Modal
import qualified Imogen.Modal.Solver as MSolver
import Imogen.Print (Print, pPrint)
import qualified Imogen.Prover as Prover
import qualified Imogen.Seq as Seq
import Imogen.Seq (Seq)
import qualified Imogen.Sig as Sig
import Imogen.Sig (Σ)
import qualified Imogen.Util.Print as PP
import qualified Imogen.Util.Parse as P
import qualified Imogen.Util.Test as Test
import Imogen.Util.Test (Test(..), (~:), (@?=))
import qualified System.IO.UTF8 as S

-- @ Tests

sig :: Σ 
sig = P.parse 
  "  { p, P, q            : Prop                      \
  \  , r                  : LWorld -> Prop            \
  \  , r2                 : Head -> LWorld -> Prop    \
  \  , p2                 : LFrame -> LFrame -> Prop  \
  \  , ç1, ç2             : Head                      \
  \  , r1                 : U -> U -> Prop            \
  \  , f                  : U -> U -> U               \
  \  , p1, q1, Q          : U -> Prop                 \
  \  , c, a, a'           : U                         \
  \  }                                                "         


ctx :: Γ
ctx = P.parse
  "   { X, X', Y, Y'          : U      \
  \   , α, α1, α2, α3, α4, β : LWorld \
  \   , φ1, φ2               : Head   \
  \   }                                "


seqs1 :: [(String, Seq, Seq, Maybe Θ)]
seqs1 = 
  let parse (s, a, b, θ) = ( s
                           , Class.normalize sig ctx $ P.parse a  
                           , Class.normalize sig ctx $ P.parse b
                           , P.parse θ ) in
  map parse 

  [ ( "t1"
    , "⊤ ⎜ · ⊢ · {}"
    , "⊤ ⎜ · ⊢ · {}"
    , "Just ({}, ⊤)"
    )

  , ( "t2"
    , "⊤ ⎜ · ⊢ p1(X) {X : U}"
    , "⊤ ⎜ · ⊢ p1(Y) {Y : U}"
    , "Just ({X ↦ Y}, ⊤)"
    )

  , ( "t3"
    , "⊤ ⎜ p ⊢ q {}"
    , "⊤ ⎜ · ⊢ q {}"
    , "Nothing"
    )

  , ( "t4"
    , "⊤ ⎜ · ⊢ p2(ç1, ç2) {}"
    , "⊤ ⎜ · ⊢ p2(ç1, ç2) {}"
    , "Just ({}, ⊤)"
    )

  , ( "t5"
    , "⊤ ⎜ · ⊢ p2(ç1, ç2 ⋆ ε) {}"
    , "⊤ ⎜ · ⊢ p2(ç1, ç2 ⋆ ε) {}"
    , "Just ({}, ⊤)"
    )

  , ( "t6"
    , "path(ε) ⎜ · ⊢ · {}"
    , "⊤ ⎜ · ⊢ · {}"
    , "Just ({}, ⊤)"
    )

  ]

subsume :: (Seq, Seq) -> IO (Maybe Θ)
subsume (x, y) = 
  State.evalStateT (Seq.subsume x y) 
    (Prover.startState sig MSolver.solver Global.empty Modal.None)

subtests :: Test
subtests = "Subsumption" ~: map mkTest seqs1
  where mkTest (name, a, b, θ) = name ~: do
          θ' <- subsume (a, b) 
          θ' @?= θ

tests :: Test
tests = "Seq" ~: [ subtests
                 ]

main :: IO ()
main = do S.putStrLn "Subsumption"
          θs :: [(Maybe Θ, Maybe Θ)] <- mapM sub1 seqs1
          mapM_ (PP.putStrLn . pPrint sig ctx) θs
 where 
  sub1 :: (String, Seq, Seq, Maybe Θ) -> IO (Maybe Θ, Maybe Θ)
  sub1 (_, a, b, c) = subsume (a, b) >>= \θ -> return (θ, c)


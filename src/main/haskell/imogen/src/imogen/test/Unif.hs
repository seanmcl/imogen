
-- @ Pragmas

{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}  

-- @ Signature 

module Imogen.Test.Unif
  ( tests )
where

-- @ Imports

import Imogen.Util.Prelude
import Control.Monad.State as State
import Imogen.Atom (Atom)
import qualified Imogen.Class as Class
import Imogen.CSubst (Θ)
import qualified Imogen.Ctx as Ctx
import Imogen.Ctx (Γ)
--import qualified Imogen.Func as Func
import qualified Imogen.Modal as Modal
--import qualified Imogen.Param as Param
import Imogen.Parse ()
--import qualified Imogen.Pred as Pred
import qualified Imogen.Sig as Sig
import Imogen.Sig (Σ)
--import Imogen.Sort (Sort(..), Base(..))
--import qualified Imogen.Util.Print as PP
import qualified Imogen.Util.Parse as P
import qualified Imogen.Util.Test as Test
import Imogen.Util.Test (Test, (~:), (@?=))
import qualified Imogen.Var as Var
import qualified System.IO.UTF8 as S

-- @ Uninterpreted 

sigU :: Σ 
sigU = P.parse 
  "    { p, q, P    : Prop              \
  \    , q1, Q1, r1 : U -> Prop         \
  \    , p2, r2     : U -> U -> Prop    \
  \    , a, a'      : U                 \
  \    , f1         : U -> U            \
  \    , f2         : U -> U -> U       \
  \    , ç, ç'      : U                 \
  \    }                                " 

ctxU :: Γ
ctxU = P.parse
  "   { X, X', Y, Y' : U }              "

termsU :: [(String, String, String, String)]
termsU = 
    [ ("u1", "p", "p", "Just ({}, ⊤)")
    , ("u2", "q1(X)", "q1(Y)", "Just ({X ↦ Y}, ⊤)")
    , ("u3", "r1(X)", "r1(f1(X))", "Nothing")
    , ("u4", "r2(X, Y)", "r2(X', Y')", "Just ({X ↦ X', Y ↦ Y'}, ⊤)")
    ] 

-- @ Linear

sigL :: Σ 
sigL = P.parse 
  "    { p, q, P      : Prop                         \
  \    , q1, Q1, r1   : LWorld -> Prop               \
  \    , p2, r2       : LWorld -> LWorld -> Prop     \
  \    , a, a'        : LWorld                       \
  \    , f1           : LWorld -> LWorld             \
  \    , f2           : LWorld -> LWorld -> LWorld   \
  \    , ç, ç', ĉ, ç2 : LWorld                       \
  \    }                                             " 

ctxL :: Γ
ctxL = P.parse
  "   { X, X', Y, Y', α, α1, α2, α3, α4, α5, α7, α8, β : LWorld  \
  \   , φ1, φ2, φ6 : Head                                        \
  \   }                                                           "

termsL :: [(String, String, String, String)]
termsL = 
  [ ("t1", "p", "p", "Just ({}, ⊤)")
  , ("t2", "q1(X)", "q1(Y)", "Just ({X ↦ Y}, ⊤)")
  , ("t3", "r1(ε)", "r1(ε ⋆ α)", "Just ({α ↦ ε}, ⊤)")
  , ("t4", "r1(α)", "r1(β)", "Just ({α ↦ β}, ⊤)")
  , ("t5", "r1(α1 ⋆ α2)", "r1(α3 ⋆ α2)", "Just ({α1 ↦ α3}, ⊤)")
  , ("t6", "r1(α1 ⋆ α2)", "r1(α3 ⋆ α4)", "Just ({}, α1 ⋆ α2 ≡ α3 ⋆ α4)")
  , ("t7", "φ1 ◃ α1 ⋆ α2", "φ2 ⊛ β ◃ α1", "Just ({φ1 ↦ φ2, α2 ↦ β}, ⊤)")
  , ("t8", "r2(X, Y)", "r2(X', Y')", "Just ({X ↦ X', Y ↦ Y'}, ⊤)")
  , ("t10", "r2(ç, ĉ)", "r2(ç, ĉ)", "Just ({}, ⊤)")
  , ("t11", "ç2 ◃ α8 ⋆ ç3", "φ6 ◃ α5 ⋆ α7", "Just ({φ6 ↦ ç2}, α8 ⋆ ç3 ≡ α5 ⋆ α7)")
  ]

-- @ Test state

type S = (Var.Var, Σ, Modal.Mode)

instance Class.UnifClass S (StateT S IO) where

mkTests :: String -> Σ -> Γ -> [(String, String, String, String)] -> Test
mkTests uname sig ctx terms = uname ~: map mkTest terms
 where 
  mkTest (name, a, b, θ) = 
    let a' :: Atom = Class.normalize sig ctx $ P.parse a  
        b' :: Atom = Class.normalize sig ctx $ P.parse b
        θ' :: Maybe Θ = Class.normalize sig ctx $ P.parse θ 
    in name ~: do
         res <- unif sig ctx (a', b') 
         res @?= θ'

unif :: Σ -> Γ -> (Atom, Atom) -> IO (Maybe Θ)
unif sig ctx (x, y) = 
  State.evalStateT 
   (State.evalStateT (Class.unify1 x y) ctx)
   (Var.start "α", sig, Modal.None)

tests :: Test
tests = "Unif" ~: [ mkTests "Uninterp" sigU ctxU termsU 
                  , mkTests "Linear" sigL ctxL termsL
                  ]


-- @ Top

_main :: IO ()
_main = do 
  -- res <- unif1 bug
  -- pprint res
  S.putStrLn "ε"
  -- where unif1 (n, a, b, c) = 
  --         let a' :: Atom = Class.normalize sigL ctxL $ P.parse a
  --             b' :: Atom = Class.normalize sigL ctxL $ P.parse b
  --             c' :: Maybe Θ = Class.normalize sigL ctxL $ P.parse c
  --         in do 
  --           res <- unif sigL ctxL (a', b')
  --           return $ (n, res, c', a', b')
  --       bug = ("t7", "φ1 ◃ α", "φ2 ⊛ β ◃ ε", "Just ({φ1 ↦ φ2, α2 ↦ β}, ⊤)")
  --       -- bug = ("t7", "φ1 ◃ α1 ⋆ α2", "φ2 ⊛ β ◃ α1", "Just ({φ1 ↦ φ2, α2 ↦ β}, ⊤)")

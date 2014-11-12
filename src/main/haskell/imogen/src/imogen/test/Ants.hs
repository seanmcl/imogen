
-- @ Pragmas

{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances
           , FlexibleInstances #-} 

-- @ Signature

module Imogen.Test.Ants 
  ( main
  , tests
  )
where

-- @ Imports

import Imogen.Util.Prelude 
import qualified Control.Monad.State as State
import qualified Data.Set as Set
import qualified Imogen.Ants as Ants
import Imogen.Ants (Ants)
import qualified Imogen.Class as Class
import Imogen.Class (UnifClass)
import Imogen.CSubst (Θ)
import qualified Imogen.Ctx as Ctx
import Imogen.Ctx (Γ)
import qualified Imogen.Func as Func
import qualified Imogen.Modal as Modal
import Imogen.Modal (Mode)
import qualified Imogen.Param as Param
import qualified Imogen.Pred as Pred
import qualified Imogen.Sig as Sig
import Imogen.Sig (Σ)
import Imogen.Sort (Sort(..), Base(..))
import qualified Imogen.Util.Print as PP
import qualified Imogen.Util.Parse as P
import qualified Imogen.Util.Test as Test
import Imogen.Util.Test (Test(..), (~:), (@?=))
import qualified Imogen.Var as Var
import Imogen.Var (Var)
import qualified System.IO.UTF8 as S

-- @ Tests

-- Test State 

type S = (Var, Σ, Mode)

instance UnifClass S (StateT S IO) where

sig :: Σ 
sig = Sig.fromList [ (Sig.Pred $ Pred.make "p", Rel [])
                   , (Sig.Pred $ Pred.make "p1", Rel [U])
                   , (Sig.Pred $ Pred.make "q", Rel [])
                   , (Sig.Pred $ Pred.make "r", Rel [LWorld])
                   , (Sig.Pred $ Pred.make "r2", Rel [Head, LWorld])
                   , (Sig.Pred $ Pred.make "P", Rel [])
                   , (Sig.Pred $ Pred.make "p2", Rel [LFrame, LFrame])
                   , (Sig.Pred $ Pred.make "q1", Rel [U])
                   , (Sig.Pred $ Pred.make "Q", Rel [U])
                   , (Sig.Func $ Func.make "a", Fun [] U)
                   , (Sig.Func $ Func.make "c", Fun [] U)
                   , (Sig.Param $ Param.make "ç", Fun [] LWorld)
                   , (Sig.Func $ Func.make "p'", Fun [] U)
                   , (Sig.Func $ Func.make "a'", Fun [] U)
                   , (Sig.Func $ Func.make "r1", Rel [U, U])
                   , (Sig.Func $ Func.make "f", Fun [U, U] U)
                   ]

ctx :: Γ
ctx = Ctx.fromList (map (first Var.make) 
                     [ ("X", U), ("X'", U)
                     , ("Y", U), ("Y'", U)
                     , ("α", LWorld), ("α1", LWorld), ("α2", LWorld), ("α3", LWorld), ("α4", LWorld)
                     , ("β", LWorld)
                     , ("φ1", Head), ("φ2", Head)
                     ]) 

-- @ Unification

uants :: [(String, Ants, Ants, [Θ])]
uants = 
  let norm :: Ants -> Ants
      norm = Class.normalize sig ctx
      --norm = id
      parse (s, a, b, θs) = ( s
                            , norm $ P.parse a  
                            , norm $ P.parse b
                            , P.parse θs ) in
  map parse 
  [ ("t1", "·", "·", "[({}, ⊤)]")
  , ("t2", "p(X)", "p(Y)", "[({}, ⊤), ({X ↦ Y}, ⊤)]")
  , ("t3", "p(c)", "p(Y)", "[({}, ⊤), ({Y ↦ c}, ⊤)]")
  , ("t4", "[ p() ]", "[ p() ]", "[({}, ⊤)]")
  , ("t5", "r(α)", "r(ç)", "[({}, ⊤), ({α ↦ ç}, ⊤)]")
  , ("t6", "r(ç)", "r(ç)", "[({}, ⊤)]")
  , ("t7", "r(α ⋆ ç)", "r(ç)", "[({}, ⊤), ({α ↦ ε}, ⊤)]")
  , ("t8", "p' ⓦ α ⋆ ç", "p' ⓦ ç", "[({}, ⊤), ({α ↦ ε}, ⊤)]")
  ]

unif :: (Ants, Ants) -> IO [Θ]
unif (x, y) = do 
  θs <- State.evalStateT (Ants.unify Ctx.empty x y) (Var.start "α", sig, Modal.None)
  return $ map fst (Set.toList θs)

utests :: Test
utests = "Unify" ~: map mkTest uants
  where mkTest :: (String, Ants, Ants, [Θ]) -> Test
        mkTest (name, a, b, θ) = name ~: do 
          θ' <- unif (a, b) 
          θ' @?= θ

-- @ Matching

match :: (Ants, Ants) -> IO [Θ]
match (x, y) = do 
  θs <- State.evalStateT (Ants.match Ctx.empty x y) (Var.start "α", sig, Modal.None)
  return $ map fst θs

mants :: [(String, Ants, Ants, [Θ])]
mants = 
  let parse (s, a, b, θs) = ( s
                           , Class.normalize sig ctx $ P.parse a  
                           , Class.normalize sig ctx $ P.parse b
                           , P.parse θs ) in
  map parse 
  [ ("t1", "·", "·", "[({}, ⊤)]")
  , ("t2", "p(X)", "p(Y)", "[({X ↦ Y}, ⊤)]")
  , ("t3", "p(c)", "p(Y)", "[]")
  ]

mtests :: Test
mtests = "Match" ~: map mkTest mants
  where mkTest (name, a, b, θ) = name ~: do
          θ' <- match (a, b) 
          θ' @?= θ

-- @ Contraction

contr :: (Ants, Ants) -> IO [Θ]
contr (x, glob) = do 
  θs <- State.evalStateT (Ants.contract glob (const True) Ctx.empty x) (Var.start "α", sig, Modal.None)
  return $ map (\(θ, _, _) -> θ) θs

cants :: [(String, Ants, Ants, [Θ])]
cants = 
  let parse (s, a, g, θs) = ( s
                            , Class.normalize sig ctx $ P.parse a  
                            , Class.normalize sig ctx $ P.parse g
                            , P.parse θs ) in
  map parse 
  [ ("t1", "·", "·", "[({}, ⊤)]")
  , ("t2", "·", "p1(c)", "[({}, ⊤)]")
  ]

ctests :: Test
ctests = "Contraction" ~: map mkTest cants
  where mkTest (name, a, b, θ) = name ~: do
          θ' <- contr (a, b)
          θ' @?= θ

-- @ Top

tests :: Test
tests = "Ants" ~: [ utests
                  , mtests 
                  , ctests
                  ]

main :: IO ()
main = do S.putStrLn "Unify"
          θs :: [([Θ], [Θ])] <- mapM unif1 uants
          mapM_ (PP.putStrLn . pPrint) θs
          S.putStrLn "Match"
          θs' <- mapM match1 mants
          mapM_ (PP.putStrLn . pPrint) θs'
          S.putStrLn "Contract"
          θs'' <- mapM contr1 cants
          mapM_ (PP.putStrLn . pPrint) θs''
 where 
  unif1, match1, contr1 :: (String, Ants, Ants, [Θ]) -> IO ([Θ], [Θ])
  unif1 (_, a, b, c) = unif (a, b) >>= \θ -> return (θ, c)
  match1 (_, a, b, c) = match (a, b) >>= \θ -> return (θ, c)
  contr1 (_, a, b, c) = contr (a, b) >>= \θ -> return (θ, c)

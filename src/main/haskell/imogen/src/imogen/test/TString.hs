
-- @ Signature 

module Imogen.Test.TString
  ( main
  , tests
  )
where

-- @ Imports

import Imogen.Util.Prelude 
import qualified Control.Monad.State as State
import qualified Imogen.CSubst as CSubst
import qualified Imogen.Ctx as Ctx
import qualified Imogen.Modal as Modal
import Imogen.Modal (World, Mode(..))
import Imogen.Modal.Solver as Solver
import Imogen.Parse ()
import Imogen.Print (pPrint)
import qualified Imogen.Sig as Sig
import Imogen.Subst (Θ)
import qualified Imogen.Util.Monad as M
import qualified Imogen.Util.Print as PP
import qualified Imogen.Util.Parse as P
import qualified Imogen.Util.Test as Test
import Imogen.Util.Test (Test(..), (~:), (@?=))
import qualified Imogen.Var as Var
import qualified System.IO.UTF8 as S

-- @ Util 

startWorld :: (World, World) -> (World, World, World)
startWorld (w1, w2) = (w1, Modal.ε, w2)

type Eqn = (String, [(World, World)], [Θ])
type Eqns = [Eqn]

type Eqn' = (String, [(World, World)], Maybe CSubst.Θ)
type Eqns' = [Eqn']

mkTests :: String -> Mode -> Eqns -> Test
mkTests tname m eqs = tname ~: map mkTest eqs
 where 
  mkTest :: Eqn -> Test
  mkTest (name, eqns, res) = name ~: 
    (do res' <- State.evalStateT (Modal.unify m $ map startWorld eqns) (Var.start "w")
        res' @?= res)

mkSimpTests :: String -> Mode -> Eqns' -> Test
mkSimpTests tname m eqs = tname ~: map mkTest eqs
 where 
  mkTest :: Eqn' -> Test
  mkTest (name, eqns, res) = name ~: 
    (do res' <- State.evalStateT (Solver.simplify $ map startWorld eqns) (Var.start "w", m)
        res' @?= res)

-- @ K

worldsK:: [(String, [(World, World)], [Θ])]
worldsK = 
  let parse (s, eqs, ts) = ( s, P.parse eqs, P.parse ts ) in
  map parse 
  [ ("k.1", "[(ε, ε)]", "[{}]") 
  , ("k.2", "[(X, X)]", "[{}]") 
  , ("k.3", "[(c, X)]", "[{X ↦ c}]") 
  , ("k.4", "[(c1 · c2, c1 · X)]", "[{X ↦ c2}]") 
  -- , ("k.5", "[(c1 · (c2 · c3), (c1 · c2) · c3)]", "[]") 
  ]

worldsK':: [(String, [(World, World)], Maybe CSubst.Θ)]
worldsK' = 
  let parse (s, eqs, ts) = ( s, P.parse eqs, P.parse ts ) in
  map parse 
  [ ("k.1", "[(ε, ε)]", "Just ({}, ⊤)") 
  , ("k.2", "[(X, X)]", "Just ({}, ⊤)") 
  , ("k.3", "[(c, X)]", "Just ({X ↦ c}, ⊤)") 
  -- , ("k.5", "[(c1 · (c2 · c3), (c1 · c2) · c3)]", "[]") 
  ]

testsK :: Test
testsK = mkTests "K" K worldsK

testsK' :: Test
testsK' = mkSimpTests "K'" K worldsK'

-- @ S4 

worldsS4:: [(String, [(World, World)], [Θ])]
worldsS4 = 
  let parse (s, eqs, ts) = ( s, P.parse eqs, P.parse ts ) in
  map parse 
  [ ("s4.1", "[(ε, ε)]", "[{}]") 
  , ("s4.2", "[(X, X)]", "[{}]") 
  , ("s4.3", "[(c, X)]", "[{X ↦ c}]") 
  , ("s4.4", "[(c1 · c2, c1 · X)]", "[{X ↦ c2}]") 
  , ("s4.5", "[(c1 · (c2 · c3), (c1 · c2) · c3)]", "[{}]") 
  ]

testsS4 :: Test
testsS4 = mkTests "S4" S4 worldsS4

-- @ Top

tests :: Test
tests = "Modal" ~: [ testsK, testsK', testsS4 ]

main :: IO ()
main = do S.putStrLn "Modal"
          θs4 :: [Bool] <- mapM (unif S4) worldsS4
          mapM_ (PP.putStrLn . pPrint Sig.empty Ctx.empty) θs4
 where 
  unif :: Mode -> (String, [(World, World)], [Θ]) -> IO Bool
  unif mode (_, eqs, θs) = do
    θs' <- State.evalStateT (Modal.unify mode $ map startWorld eqs) (Var.start "w")
    return $ θs == θs'


-- @ Signature

module Imogen.Test.World
  ( main
  , tests
  )
where

-- @ Imports

import Imogen.Util.Prelude 
import qualified Control.Monad.State as State
import qualified Imogen.Ctx as Ctx
import qualified Imogen.Linear.World as World
import Imogen.Linear.World (Eqn)
import Imogen.Parse ()
import Imogen.Subst (Θ)
import qualified Imogen.Util.Parse as P
import qualified Imogen.Util.Print as PP
import qualified Imogen.Util.Test as Test
import Imogen.Util.Test (Test(..), (~:), (@?=))
import qualified Imogen.Var as Var
import qualified System.IO.UTF8 as S

-- @ Tests

uworlds :: [(String, [Eqn], Maybe (Θ, [Eqn]))]
uworlds = 
  let parse (s, eqs, θ) = ( s
                          , P.parse eqs
                          , P.parse θ ) in
  map parse 
  [ ("t1", "[(ç, ç)]", "Just ({}, [])")
  , ("t2", "[(α, ç)]", "Just ({α ↦ ç}, [])")
  , ("t3", "[(α ⋆ ε, ç)]", "Just ({α ↦ ç}, [])")
  , ("t4", "[(α ⋆ ç, ç)]", "Just ({α ↦ ε}, [])")
  , ("t5", "[(α ⋆ ç, β ⋆ β')]", "Just ({}, [(α ⋆ ç, β ⋆ β')])")
  , ("t6", "[(α ⋆ ç, _β ⋆ _β')]", "Nothing")
  ]

unif :: [Eqn] -> IO (Maybe (Θ, [Eqn]))
unif eqs = State.evalStateT (State.evalStateT (World.unify eqs) Ctx.empty) (Var.start "α")

utests :: Test
utests = "Unify" ~: map mkTest uworlds
  where mkTest :: (String, [Eqn], Maybe (Θ, [Eqn])) -> Test
        mkTest (name, eqs, θ) = name ~: do 
          θ' <- unif eqs
          θ' @?= θ

tests :: Test
tests = "World" ~: [ utests ]

main :: IO ()
main = do S.putStrLn "Unify"
          θs <- mapM unif1 uworlds
          mapM_ (PP.putStrLn . PP.pPrint) θs
 where 
  unif1 :: (String, [Eqn], Maybe (Θ, [Eqn])) -> IO (Maybe (Θ, [Eqn]), Maybe (Θ, [Eqn]))
  unif1 (_, eqs, c) = unif eqs >>= \θ -> return (θ, c)


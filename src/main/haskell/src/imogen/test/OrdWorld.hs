
-- @ Signature 

module Imogen.Test.OrdWorld
  ( main
  , tests
  )
where

-- @ Imports

import Imogen.Util.Prelude 
import qualified Control.Monad.State as State
import qualified Imogen.Ctx as Ctx
import qualified Imogen.Linear.World ()
import qualified Imogen.Ordered.World as World
import Imogen.Ordered.World (Eqn, WEqn)
import Imogen.Subst (Θ)
import qualified Imogen.Util.Parse as P
import qualified Imogen.Util.Print as PP
import qualified Imogen.Util.Test as Test
import Imogen.Util.Test (Test(..), (~:), (@?=))
import qualified Imogen.Var as Var
import qualified System.IO.UTF8 as S

-- @ Tests

uworlds :: [(String, [Eqn], Maybe (Θ, [Eqn], [WEqn]))]
uworlds = 
  let parse (s, eqs, θ) = ( s
                          , P.parse eqs
                          , P.parse θ ) in
  map parse 
  [ ("t1", "[(ç, ç)]", "Just ({}, [], [])")
  , ("t2", "[(α, ç)]", "Just ({α ↦ ç}, [], [])")
  , ("t3", "[(α · ع, ç)]", "Just ({α ↦ ç}, [], [])")
  , ("t4", "[(α · ç, ç)]", "Just ({α ↦ ع}, [], [])")
  , ("t5", "[(α · ç, β · β')]", "Just ({}, [(α · ç, β · β')], [])")
  , ("t6", "[(α · ç, _β · _β')]", "Nothing")
  -- , ("t7", "[(ç4 · α64 · ç2 · ç5, α45 · ç7 · α46)]", 
  --         "Just ({α2 ↦ α3 · ç2, α45 ↦ ç4 · α1, α46 ↦ (α3 · ç2) · ç5, α64 ↦ α1 · ç7 · α3}, [] , [])")
  , ("t8", "[(α1 · ç · α2, ç)]", "Just ({α1 ↦ ع, α2 ↦ ع}, [], [])")
  ]

{- 
q  : ⊤ | Ⓦ(b, α64) ⊢ ◂(#c3, ·(#c4, α64, #c2, #c5)) : q5

r  : Ⓦ(b, ç7) ⊢ ◂(φ44, ·(α45, ç7, α46))
    ------------------------------------------------ {ç7} : r1 
   ⊤ | Ⓦ(a, α47) ⊢ ◂(φ44, ·(α45, α47, #c1, α46))

res: Left []

 β' ↦ β'' · ç 

 ç4 · α64 · ç2 · ç5 ≡ α45 · ç7 · α46

 α45 ↦ ç4 · α1

 α64 · ç2 · ç5 ≡ α1 · ç7 · α46

 α46 ↦ α2 · ç5

 α64 · ç2 ≡ α1 · ç7 · α2 

 α2 ↦ α3 · ç2

 α64 ≡ α1 · ç7 · α3 

 α64 ↦ α1 · ç7 · α3 
 α45 ↦ ç4 · α1
 α46 ↦ α3 · ç2 · ç5

 ç4 · α1 · ç7 · α3  ç2 · ç5 ≡ ç4 · α1 · ç7 · α3 · ç2 · ç5

ç ≡ α1 · ç · α2 
-} 

unif :: [Eqn] -> IO (Maybe (Θ, [Eqn], [WEqn]))
unif eqs = State.evalStateT (State.evalStateT (World.unify eqs) Ctx.empty) (Var.start "α")

utests :: Test
utests = "Unify" ~: map mkTest uworlds
  where mkTest :: (String, [Eqn], Maybe (Θ, [Eqn], [WEqn])) -> Test
        mkTest (name, eqs, θ) = name ~: do 
          θ' <- unif eqs
          θ' @?= θ

tests :: Test
tests = "OrdWorld" ~: [ utests ]

main :: IO ()
main = do S.putStrLn "Unify"
          θs <- mapM unif1 uworlds
          mapM_ (PP.putStrLn . PP.pPrint) θs
 where 
  unif1 :: (String, [Eqn], Maybe (Θ, [Eqn], [WEqn])) -> IO (Maybe (Θ, [Eqn], [WEqn]), Maybe (Θ, [Eqn], [WEqn]))
  unif1 (_, eqs, c) = unif eqs >>= \θ -> return (θ, c)



-- | Ordered Frames

-- @ Pragmas

{-# LANGUAGE CPP, MultiParamTypeClasses, FlexibleContexts #-} 

-- @ Signature

module Imogen.Ordered.Frame 
  ( -- * Frames
    Frame
  , dest             
  , var              
  , param            
  , make             
    -- * Tag
  , Tag(..)
    -- * Util
  , (ⓛ), (ⓡ), (◃), (ⓦ), (ⓕ)
    -- * Unification
  , unify
  )
where

-- @ Imports

#include "../../undefined.h" 

import Imogen.Util.Prelude hiding (id)
import qualified Control.Monad.State as State
import qualified Data.List as List
import Imogen.Atom (Atom(Rel))
import qualified Imogen.Constr as Constr
import qualified Imogen.CSubst as CSubst
import Imogen.Ctx (Γ)
import qualified Imogen.Func as Func
import qualified Imogen.Linear.Head as Head
import Imogen.Linear.Head (Head)
import qualified Imogen.Linear.World as LWorld
import Imogen.Ordered.OL (PProp, NProp)
import qualified Imogen.Ordered.World as World
import Imogen.Ordered.World(World, ε, (·))
import qualified Imogen.Param as Param
import Imogen.Param(Param, Params, Freeze(..))
import qualified Imogen.Pred as Pred
import Imogen.Rename(Rename(..))
import qualified Imogen.Sort as Sort
import qualified Imogen.Subst as Subst
import Imogen.Subst (Θ, Apply, (✴), (○))
import qualified Imogen.Term as Term
import Imogen.Term(Term(..), Encode(..))
import qualified Imogen.Unif as Unif
import qualified Imogen.Util.Debug as Debug
import Imogen.Util.Fresh (HasFresh)
import qualified Imogen.Util.Log as Log
import Imogen.Util.Log (Log)
import qualified Imogen.Util.Print as PP
import Imogen.Util.Print (Print, pPrint)
import qualified Imogen.Util.Set as Set
import Imogen.Util.Set ((∪))
import qualified Imogen.Var as Var
import Imogen.Var(Var, Vars)

-- @ Frames

{- |
We can always separate a frame into the form f ⊙ h ⊙ g where f and g
are worlds and h is a head.
-} 
data Frame = F World Head World
  deriving (Eq, Ord, Show)

-- | Constructor.
make :: World -> Head -> World -> Frame
make = F

-- | Destructor.
dest :: Frame -> (World, Head, World)
dest (F w1 h w2) = (w1, h, w2)

-- | Make a variable.
var :: Var -> Frame
var φ = F ε (Head.var φ) ε

-- | Make a parameter.
param :: Param -> Frame
param ĉ = F ε (Head.param ĉ) ε

-- @@ Instances

instance Rename Frame where
  rename (F w1 h w2) = do
    h' <- rename h
    w1' <- rename w1
    w2' <- rename w2
    return $ F w1' h' w2'

instance Vars Frame where
  vars (F w1 h w2) = Var.vars w1 ∪ Var.vars h ∪ Var.vars w2
  free = Var.vars

instance Params Frame where
  params (F w1 h w2) = Param.params w1 ∪ Param.params h ∪ Param.params w2

instance Apply Frame where
  F w1 h w2 ✴ θ = F (w1 ✴ θ) (h ✴ θ) (w2 ✴ θ)

instance Print Frame where
  pPrint (F w1 h w2)
    | w1 == ε && w2 == ε = pPrint h
    | w1 == ε = pPrint h <+> PP.text "ⓡ" <+> pPrint w2
    | w2 == ε = pPrint w1 <+> PP.text "ⓛ" <+> pPrint h
    | otherwise = PP.hsep [pPrint w1, PP.text "ⓛ", pPrint h, PP.text "ⓡ", pPrint w2]

-- @ Tag

data Tag = Tag

instance Sort.Tag Tag where

instance Sort.TagOf Tag Frame where

instance Freeze Frame where
  freeze (F h w1 w2) = F (freeze h) (freeze w1) (freeze w2)
  thaw cs (F h w1 w2) = F (thaw cs h) (thaw cs w1) (thaw cs w2)

instance Sort.OK Frame Tag where
  ok _ (F w1 h w2) = Sort.ok World.Tag w1 && Sort.ok Head.Tag h && Sort.ok World.Tag w2

instance Sort.OK Term Tag where
  ok tag t = Sort.ok tag (decode t :: Frame)

-- @ Util

(ⓛ) :: World -> Frame -> Frame
p ⓛ F w1 h w2 = F (p · w1) h w2

(ⓡ) :: Frame -> World -> Frame
F w1 h w2 ⓡ p = F w1 h (w2 · p)

-- @ Unification

type FEqn = (Frame, Frame)
type WEqn = (World, World)
type LEqn = (LWorld.World, LWorld.World)
type HEqn = (Head, Head)

{- 
Frame unification uses the head unification defined above along
with World unification.  
-} 

split1 :: FEqn -> (WEqn, HEqn, WEqn)
split1 (F l1 h1 r1, F l2 h2 r2) = ((l1, l2), (h1, h2), (r1, r2))

split :: [FEqn] -> ([WEqn], [HEqn], [WEqn])
split = unzip3 . map split1 

unify :: (MonadState s m, HasFresh Var s, Log m) => [FEqn] -> StateT Γ m (Maybe (Θ, [WEqn], [LEqn]))
unify eqs = 
  let (ws1, hs, ws2) = split eqs 
        -- first unify the frames
  in do res <- Head.unify hs
        res' <- case res of 
          Nothing -> return Nothing 
          -- if that succeeds, unify the worlds
          Just θ -> do 
            res' <- World.unify (ws1 ++ ws2)
            case res' of
              Nothing -> return Nothing
              Just (θ', ψ, ψ') -> return $ Just (θ ○ θ', ψ, ψ')
        Log.debugM' "Frame.unify" $ PP.vcat [ PP.text "Eqns:" <+> pPrint eqs
                                            , PP.text "Res :" <+> pPrint res' ]
        return res'

instance Unif.Unif Frame Tag where
  unify _ = fmap (fmap f) . unify
    where f (θ, eqs, weqs) = CSubst.make θ (Constr.listAnd $ map mkeq eqs ++ map mkweq weqs)
          mkeq (f1, f2) = Constr.Atom $ Rel (Pred.make "=") [encode f1, encode f2]
          mkweq (f1, f2) = Constr.Atom $ Rel (Pred.make "≡") [encode f1, encode f2]
  match tag eqs = fmap (thaw frozen) $ Unif.unify tag (map (second freeze) eqs)
    where xs = Var.vars eqs
          frozen = Set.map Var.freeze xs

{- 
Unification with frame variables is harder.  I think it's best to
leave them out completely.

φ ~ f ---> {φ ↦ f}

if w ~ ε ---> θ then φ ⊛ w ~ ç ---> θ ○ {φ ↦ ç}
φ1 ⊛ w1 ~ φ2 ⊛ w2 ---> θ ○ {φ ↦ ç}
... 
-} 

-- @ Normalization

-- Handle f ◃ w as a special case of normalization.

(◃) :: Frame -> World -> Atom
f ◃ w = Rel (Pred.make "◂") [encode h, encode $ wl · w · wr]
  where (wl, h, wr) = dest f

(ⓦ) :: PProp -> World -> Atom
a ⓦ w = Rel (Pred.make "Ⓦ") [encode a, encode w]

(ⓕ) :: NProp -> Frame -> Atom
a ⓕ f = Rel (Pred.make "Ⓕ") [encode a, encode f]

instance Encode Frame where
  encode (F w1 h w2) = Fn (Func.make "⊙") [encode w1, encode h, encode w2]
  decode = flatten . fromTerm

instance Sort.Normalize Term Tag where
  normalize _ t = encode (decode t :: Frame)

data FTree = Head Head
           | StarL World FTree
           | StarR FTree World

flatten :: FTree -> Frame
flatten t = case t of 
  Head h -> F ε h ε
  StarL w f -> F (w · w1) h w2
    where F w1 h w2 = flatten f
  StarR f w -> F w1 h (w2 · w)
    where F w1 h w2 = flatten f

fromTerm :: Term -> FTree
fromTerm t = case t of
  Var x -> Head $ Head.var x
  Param c -> Head $ Head.param c
  Fn c [] -> Head $ Head.const c
  Fn s [f, w]
    | Func.name s == "ⓡ" -> StarR (fromTerm f) (decode $ Sort.normalize World.Tag w)
    | Func.name s == "ⓛ" -> StarL (decode $ Sort.normalize World.Tag w) (fromTerm f) 
    | otherwise -> Debug.error $ PP.text "Can't convert to frame: " <+> pPrint t
  Fn s [αl, h, αr]
    | Func.name s == "⊙" -> StarL (decode $ Sort.normalize World.Tag αl) 
                           $ StarR (Head $ decode $ Sort.normalize Head.Tag h)
                           $ (decode $ Sort.normalize World.Tag αr) 
    | otherwise -> Debug.error $ PP.text "Can't convert to frame: " <+> pPrint t
  _ -> Debug.error $ PP.text "Can't convert to frame: " <+> pPrint t

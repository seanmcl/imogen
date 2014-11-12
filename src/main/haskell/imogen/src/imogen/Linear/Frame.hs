
-- | Linear frames

-- @ Pragmas

{-# LANGUAGE CPP, MultiParamTypeClasses, FlexibleContexts #-} 

-- @ Signature

module Imogen.Linear.Frame 
  ( -- * Linear frames
    Frame
  , Tag(..)
    -- * Operations
  , make 
  , dest             
  , var              
  , param            
  , (⊛), (◃), (ⓦ), (ⓕ)
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
import Imogen.Linear.LL (PProp, NProp)
import qualified Imogen.Linear.World as World
import Imogen.Linear.World(World, ε, (⋆))
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

{- 
In light of the equivalence 

 (f ⊛ p) ◃ q ≡ f ◃ (q ⋆ p)

we can represent frames uniformly as a variable or constant
applied to a world.  Thus, the representation of a frame is
one of

 φ ⊛ p, ĉ ⊛ p

Heads  h ::= φ | ĉ
Frames f ::= h | f ⊛ p

The term normal form for frames is

Fn "⊛" [h, w] 

where h is a head (var or param) and w is the normal form of the world
component.  
-} 

data Frame = F Head World
  deriving (Eq, Ord, Show)

-- @@ Instances

instance Freeze Frame where
  freeze (F h w) = F (freeze h) (freeze w)
  thaw cs (F h w) = F (thaw cs h) (thaw cs w)

instance Rename Frame where
  rename (F h w) = do h' <- rename h
                      w' <- rename w
                      return $ F h' w'

instance Vars Frame where
  vars (F h p) = Var.vars h ∪ Var.vars p
  free = Var.vars

instance Params Frame where
  params (F h p) = Param.params h ∪ Param.params p

instance Apply Frame where
  F h p ✴ θ = F (h ✴ θ) (p ✴ θ)

instance Print Frame where
  pPrint (F h p) | p == ε = pPrint h
                 | otherwise = PP.hsep [pPrint h, PP.text "⊛", pPrint p]

-- @ Sort tag

-- | A sort tag.
data Tag = Tag

instance Sort.Tag Tag where

instance Sort.TagOf Tag Frame where

instance Sort.OK Frame Tag where
  ok _ (F h w) = Sort.ok Head.Tag h && Sort.ok World.Tag w

instance Sort.OK Term Tag where
  ok tag t = Sort.ok tag (decode t :: Frame)

-- @ Operations

-- | Make a variable.
var :: Var -> Frame
var φ = F (Head.var φ) ε

-- | Make a parameter.
param :: Param -> Frame
param ĉ = F (Head.param ĉ) ε

-- | Constructor.
make :: Head -> World -> Frame
make = F

-- | Destructor.
dest :: Frame -> (Head, World)
dest (F h p) = (h, p)

-- | Extend a frame by a world.
(⊛) :: Frame -> World -> Frame
F h p ⊛ q = F h (p ⋆ q)

-- @ Unification

{- 
Frame unification uses the head unification defined above along
with World unification.  
-} 

type HEqn = (Head, Head)
type WEqn = (World, World)
type FEqn = (Frame, Frame)

split1 :: FEqn -> (HEqn, WEqn)
split1 (F h p, F h' p') = ((h, h'), (p, p'))

split :: [FEqn] -> ([HEqn], [WEqn])
split = unzip . map split1 

unify :: (MonadState s m, HasFresh Var s, Log m) => [FEqn] -> StateT Γ m (Maybe (Θ, [WEqn]))
unify eqs = 
  let (heads, worlds) = split eqs 
        -- first unify the frames
  in do res <- Head.unify heads
        res' <- case res of 
          Nothing -> return Nothing 
          -- if that succeeds, unify the worlds
          Just θ -> do 
            res' <- World.unify worlds
            case res' of
              Nothing -> return Nothing
              Just (θ', ψ) -> return $ Just (θ ○ θ', ψ)
        Log.debugM' "Frame.unify" $ PP.vcat [ PP.text "Eqns:" <+> pPrint eqs
                                            , PP.text "Res :" <+> pPrint res' ]
        return res'

instance Unif.Unif Frame Tag where
  unify _ = fmap (fmap f) . unify
    where f (θ, eqs) = CSubst.make θ (Constr.listAnd $ map mkeq eqs)
          mkeq (f1, f2) = Constr.Atom $ Rel (Pred.make "=") [encode f1, encode f2]
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
f ◃ w = Rel (Pred.make "◃") [encode h, encode $ v ⋆ w] 
  where (h, v) = dest f

(ⓦ) :: PProp -> World -> Atom
a ⓦ w = Rel (Pred.make "ⓦ") [encode a, encode w]

(ⓕ) :: NProp -> Frame -> Atom
a ⓕ f = Rel (Pred.make "ⓕ") [encode a, encode f]

instance Encode Frame where
  encode (F h w) = Fn (Func.make "⊛") [encode h, encode w]
  decode = flatten . fromTerm

-- Recall the normal form is Fn "⊛" [h, w] 

instance Sort.Normalize Term Tag where
  normalize _ t = encode (decode t :: Frame)

data FTree = Head Head
           | Star FTree World

flatten :: FTree -> Frame
flatten t = case t of 
  Head h -> F h ε 
  Star f w -> F h (w' ⋆ w)
    where F h w' = flatten f

fromTerm :: Term -> FTree
fromTerm t = case t of
  Var x -> Head $ Head.var x
  Param c -> Head $ Head.param c
  Fn c [] -> Head $ Head.const c
  Fn s [f, w]
    | Func.name s == "⊛" -> Star (fromTerm f) (decode $ Sort.normalize World.Tag w)
    | otherwise -> Debug.error $ PP.text "Can't convert to frame: " <+> pPrint t
  _ -> Debug.error $ PP.text "Can't convert to frame: " <+> pPrint t


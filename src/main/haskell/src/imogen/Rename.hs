
{- | 
/Renaming contexts/.

Renaming is a crucial operation in the inverse method.  Each sequent and inference
rule has free variables that are implicitly universally quantified on the outside
of the rule\/sequent.  Unification can go horribly awry if a rule and a sequent
share variable names.  This module defines a class for the renaming of objects like sequents
and inference rules.
-} 

-- @ Pragmas

{-# LANGUAGE FlexibleContexts #-} 

-- @ Signature

module Imogen.Rename 
  ( -- * Renaming type class
    Rename(..)
    -- * Renaming context operations
  , empty
  , lookup
  , insert
  , delete
  ) 
where

-- @ Imports

import Imogen.Util.Prelude hiding (lookup) 
import qualified Control.Monad.State as State
import Control.Monad.State (StateT)
import qualified Control.Monad.Trans as Trans
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import qualified Data.Set as Set
import qualified Data.Traversable as Trav
import Imogen.Param (Param)
import Imogen.Pred (Pred)
import Imogen.Sort (Sort, Base)
import qualified Imogen.Util.Fresh as Fresh
import Imogen.Util.Fresh (HasFresh)
import qualified Imogen.Var as Var
import Imogen.Var (Var)

-- @ Context

-- | The renaming context is essentially a map from variables to variables.
newtype RCtx = Γ (Map Var Var) 

-- | The empty context.
empty :: RCtx
empty = Γ Map.empty

-- | Lookup a variable in the context.
lookup :: Var -> RCtx -> Maybe Var
lookup x (Γ m) = Map.lookup x m

-- | Add a new variable to the context.  If the variable is already present it is an error.
insert :: Var -> Var -> RCtx -> RCtx
insert x σ (Γ vm) = Γ $ Map.insertWith (error "insert: already present") x σ vm

-- | We need to be able to delete variables from the context when renaming 
--   formulas with quantifiers.
delete :: Var -> RCtx -> RCtx
delete x (Γ vm) = if Map.member x vm then Γ $ Map.delete x vm
                  else error $ "No mapping for " ++ show x ++ " in renaming context."

-- @ Class
        
class Rename a where
  -- | Rename an object in the current state extended with a renaming context.
  rename :: (MonadState s m, HasFresh Var s) => 
    a -> StateT RCtx m a
  -- | Run the renaming operation to completion.
  doit :: (MonadState s m, HasFresh Var s) => a -> m a
  doit x = State.evalStateT (rename x) empty

-- @@ Instances

instance Rename Var where
  rename x = do 
    ctx <- State.get
    case lookup x ctx of 
      Just x' -> return x'
      Nothing -> do 
        x' <- Trans.lift $ Fresh.fresh $ Var.name x
        State.put (insert x x' ctx)
        return x'

instance Rename Param where
  rename = return

instance Rename a => Rename [a] where
  rename = mapM rename

instance Rename a => Rename (Seq a) where
  rename = Trav.mapM rename

instance Rename Pred where
  rename = return

instance Rename Sort where
  rename = return

instance Rename Base where
  rename = return

instance (Rename a, Rename b) => Rename (a, b) where
  rename (x, y) = do
    x' <- rename x
    y' <- rename y
    return $ (x', y')

instance (Rename a, Rename b, Rename c) => Rename (a, b, c) where
  rename (x, y, z) = do
    x' <- rename x
    y' <- rename y
    z' <- rename z
    return $ (x', y', z')

instance (Rename a, Rename b, Rename c, Rename d) => Rename (a, b, c, d) where
  rename (x, y, z, w) = do
    x' <- rename x
    y' <- rename y
    z' <- rename z
    w' <- rename w
    return $ (x', y', z', w')

instance (Rename a, Rename b, Rename c, Rename d, Rename e) => Rename (a, b, c, d, e) where
  rename (x, y, z, w, v) = do
    x' <- rename x
    y' <- rename y
    z' <- rename z
    w' <- rename w
    v' <- rename v
    return $ (x', y', z', w', v')

instance (Rename a, Rename b, Rename c, Rename d, Rename e, Rename f) => Rename (a, b, c, d, e, f) where
  rename (x, y, z, w, v, u) = do
    (x', y', z', w', v') <- rename (x, y, z, w, v)
    u' <- rename u
    return $ (x', y', z', w', v', u')

{- 
FIXME: This should be done with Data.Traversable.mapM, but Data.Set
isn't an instance.  I should make it one at some point, but it will
take some work.
-} 

instance (Ord a, Rename a) => Rename (Set a) where
  rename s = mapM rename (Set.toList s) >>= return . Set.fromList

instance (Ord a, Rename a, Rename b) => Rename (Map a b) where
  rename m = mapM rename (Map.toList m) >>= return . Map.fromList

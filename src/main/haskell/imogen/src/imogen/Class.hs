
{- |
Type classes for dynamic dispatch.

This module collects the functions that dynamically dispatch on the
sort of the argument.  The classes here are all generalizations of 
already existing classes.  

  * 'Normalize' (generalizing 'Sort.Normalize') is for converting terms to a normal form.

  * 'OK' (generalizing 'Sort.OK') is for semantic consistency checking.

  * 'Unif' (generalizing 'Unif.Unif') is for unification.

The classes all work in roughly the same way.  In addition to the terms, the
functions accept a signature Σ giving sorts to the constants of the input 
and a context Γ giving sorts to the variables.  Which versions of the 
algorithms to use is then determined by looking up the sort, and calling
the appropriate function.  

For example, if @f@ has sort @Head -> LWorld -> U -> U@ then 
f(t1, t2, t3) ≗ f(s1, s2, s3) will be determined by unifying 
t1 with s1 at sort Head, t2 with s2 at sort LWorld, and t3 with s3 at
sort U.  

For numerous reasons, the functions are not all defined in one big pattern match, but are
rather spread out through the code.  (E.g. some of the types are abstract, and the
functions like unification must be defined in the module where the type is defined.)
To handle this we used type classes that take a tag argument, where there is a 
1-1 correspondence between tags and sorts.  Dynamic dispatch is achieved 
by calling the base class function with the tag corresponding to the sort of the
arguments.

Note that there is nothing complicated or deep in the use of tags for
manual dynamic dispatch.  The idea is simply that by doing things with
a type class and a tag, I avoid two things:

1) Having to include a normalize (unify, etc) function in the signature of every
interesting sort.  
2) Making the exported function have the right type.

These are both handled with no effort by using a type class.

-} 

-- @ Pragmas 

{-# LANGUAGE CPP, Rank2Types, MultiParamTypeClasses, FunctionalDependencies
           , FlexibleContexts, OverlappingInstances #-} 

-- @ Signature

module Imogen.Class
  ( -- * Normal forms
    Normalize(..)
    -- * Unification
  , Unif(..)
  , UnifClass
    -- * Semantic consistency
  , OK(..)
  )
where

-- @ Imports

#include "../undefined.h" 

import Imogen.Util.Prelude hiding (pred)
import qualified Control.Monad.State as State
import qualified Data.Foldable as Fold
import qualified Data.List as List
import qualified Imogen.Atom as Atom
import Imogen.Atom (Atom(Rel))
import qualified Imogen.Constr.Dlo as Dlo
import qualified Imogen.CSubst as CSubst
import Imogen.CSubst (Apply, Θ, ι, (○), (✴), (↦))
import qualified Imogen.Ctx as Ctx
import Imogen.Ctx (Γ)
import qualified Imogen.Func as Func
import qualified Imogen.Linear.Head as Head
import qualified Imogen.Linear.Frame as LFrame
import qualified Imogen.Linear.World as LWorld
import qualified Imogen.Modal as Modal
import Imogen.Modal (HasMode)
import qualified Imogen.Modal.Solver as SolverM
import qualified Imogen.Ordered.Frame as OFrame
import qualified Imogen.Ordered.World as OWorld
import qualified Imogen.Pred as Pred
import qualified Imogen.Sig as Sig
import Imogen.Sig (Σ)
import qualified Imogen.Sort as Sort
import Imogen.Sort (Sort, Base)
import qualified Imogen.Subst as Subst
import qualified Imogen.Term as Term
import Imogen.Term (Term(..))
import qualified Imogen.Unif as Unif
import Imogen.Util.Fresh (HasFresh)
import qualified Imogen.Util.Log as Log
import Imogen.Util.Log (Log)
import qualified Imogen.Util.Map as Map
import qualified Imogen.Util.Monad as M
import qualified Imogen.Util.Print as PP
import qualified Imogen.Util.Set as Set
import Imogen.Util.Set ((∉))
import qualified Imogen.Var as Var
import Imogen.Var (Var, vars)

-- @ Normalize

-- | Convert a term to normal form by sort.
class Normalize a where
  normalize :: Σ -> Γ -> a -> a

-- @@ Instances 

instance Normalize a => Normalize (Maybe a) where
  normalize sig ctx x = x >>= return . normalize sig ctx 

instance Normalize a => Normalize [a] where
  normalize sig ctx l = map (normalize sig ctx) l

instance (Ord a, Normalize a) => Normalize (Set a) where
  normalize sig ctx = Set.map (normalize sig ctx)

instance (Ord a, Normalize b) => Normalize (Map a b) where
  normalize sig ctx = Map.map (normalize sig ctx) 

instance Normalize Term where
  normalize sig ctx t = do 
    case t of 
      Var x -> case Ctx.lookup x ctx of
        Nothing -> error $ "No sort for var: " ++ show x
        Just σ -> mkf σ t
      Param c -> case Sig.lookup c sig of
        Nothing -> error $ "No sort for param: " ++ show c
        Just (Sort.Fun [] σ) -> mkf σ t
        Just _ -> __IMPOSSIBLE__ 
      Fn f [] | Func.isIntegerLiteral f -> t
      Fn f _ -> case Sig.lookup f sig of
          Nothing -> error' $ PP.text "No sort for function: " $$ PP.pPrint t $$ PP.pPrint sig
          Just (Sort.Fun _ σ') -> mkf σ' t
          _ -> error $ "normalize: predicate sort in function position" 
    where mkf σ = case σ of 
            Sort.LWorld -> Sort.normalize LWorld.Tag
            Sort.Head -> Sort.normalize Head.Tag
            Sort.LFrame -> Sort.normalize LFrame.Tag
            Sort.Real -> Sort.normalize Dlo.Tag
            Sort.OWorld -> Sort.normalize OWorld.Tag
            Sort.OFrame -> Sort.normalize OFrame.Tag
            Sort.Principal -> id
            Sort.Int -> id
            Sort.U -> id
            Sort.MWorld -> __IMPOSSIBLE__ 

instance Normalize Atom where
  normalize sig ctx (Rel p ts) = Rel p (normalize sig ctx ts)

instance Normalize Modal.World where
  normalize _ _ x = x

instance Normalize Modal.Atom where
  normalize sig ctx (Modal.Rel p ts w) = Modal.Rel p (normalize sig ctx ts) (normalize sig ctx w)

instance Normalize Subst.Θ where
  normalize sig ctx θ = Subst.map (normalize sig ctx) θ 

instance Normalize Θ where
  normalize sig ctx θ = CSubst.make (normalize sig ctx θ') ψ'
    where (θ', ψ') = CSubst.dest θ 

-- @ OK

{- |
In some instances, terms can not make semantic sense, and we
can then throw out a sequent with the nonsense terms.  For instance,
in linear logic, the sequent Γ ⊢ h ◃ p ⋆ p can not come up in any
legal derivation, and we can then throw out the sequent.  OK.ok checks
whether this is the case for a given tag.
-} 
class OK a where
  ok :: Σ -> Γ -> a -> Bool

-- @@ Instances

instance OK a => OK (Maybe a) where
  ok sig ctx (Just a) = ok sig ctx a
  ok _ _ Nothing = True

instance OK a => OK [a] where
  ok sig ctx = all (ok sig ctx)

instance OK b => OK (Map a b) where
  ok sig ctx m = Fold.all (ok sig ctx) m

instance (Ord a, OK a) => OK (Set a) where
  ok sig ctx = Fold.all (ok sig ctx)

instance OK Term where
  ok sig ctx t = case t of
    Var x -> case Ctx.lookup x ctx of
      Nothing -> __IMPOSSIBLE__ 
      Just σ -> fn σ t
    Param c -> case Sig.lookup c sig of
      Nothing -> __IMPOSSIBLE__ 
      Just (Sort.Fun [] σ) -> fn σ t
      Just _ -> __IMPOSSIBLE__ 
    Fn f _ -> case Sig.lookup f sig of
      Nothing -> __IMPOSSIBLE__ 
      Just (Sort.Fun _ σ) -> fn σ t
      Just _ -> __IMPOSSIBLE__ 
   where 
    fn σ = case σ of 
      Sort.LWorld -> Sort.ok LWorld.Tag 
      Sort.Head -> Sort.ok Head.Tag 
      Sort.LFrame -> Sort.ok LFrame.Tag
      Sort.Real -> Sort.ok Dlo.Tag 
      Sort.OWorld -> Sort.ok OWorld.Tag 
      Sort.OFrame -> Sort.ok OFrame.Tag 
      Sort.Principal -> Sort.ok Unif.U
      Sort.Int -> Sort.ok Unif.U
      Sort.U -> Sort.ok Unif.U
      Sort.MWorld -> __IMPOSSIBLE__ 

instance OK Atom where
  ok sig _ (Rel p ts) = case Sig.lookup p sig of
    Nothing -> True
    Just (Sort.Rel σs) -> List.all fn (zip σs ts)
    Just _ -> __IMPOSSIBLE__ 
   where 
    fn (σ, t) = case σ of 
      Sort.LWorld -> Sort.ok LWorld.Tag t
      Sort.Head -> Sort.ok Head.Tag t
      Sort.LFrame -> Sort.ok LFrame.Tag t
      Sort.Real -> Sort.ok Dlo.Tag t
      Sort.OWorld -> Sort.ok OWorld.Tag t
      Sort.OFrame -> Sort.ok OFrame.Tag t
      Sort.Principal -> Sort.ok Unif.U t
      Sort.Int -> Sort.ok Unif.U t
      Sort.U -> Sort.ok Unif.U t
      Sort.MWorld -> __IMPOSSIBLE__ 

instance OK Modal.Atom where
  ok sig ctx (Modal.Rel p ts _) = ok sig ctx (Rel p ts)

-- @ Unif

-- | A utility class for packaging a long context.
class (MonadState s m, HasFresh Var s, Sig.Has s, Functor m, Log m, HasMode s) => UnifClass s m | m -> s where 

-- | Unification is defined independently at each different sort. 
class Apply a => Unif a where

  -- | Unify a list of equations.  Note that some unification algorithms invent
  --   new variables, which is the reason for the variable context Γ tacked on
  --   to the front of the state.
  unify :: UnifClass s m => [(a, a)] -> StateT Γ m (Maybe Θ)
  unify [] = return $ Just ι
  unify ((a, b) : rest) = do
    r1 <- unify1 a b
    case r1 of
      Nothing -> return Nothing
      Just θ -> do
        r2 <- unify (rest ✴ θ)
        case r2 of 
          Nothing -> return Nothing
          Just θ' -> return $ Just $ θ ○ θ'

  -- | Match a list of equations
  match :: UnifClass s m => [(a, a)] -> StateT Γ m (Maybe Θ)
  match [] = return $ Just ι
  match ((a, b) : rest) = do
    r1 <- match1 a b
    case r1 of
      Nothing -> return Nothing
      Just θ -> do
        r2 <- match (rest ✴ θ)
        case r2 of 
          Nothing -> return Nothing
          Just θ' -> return $ Just $ θ ○ θ'

  -- | Unify all elements of a list together
  unifyList :: UnifClass s m => [a] -> StateT Γ m (Maybe Θ)
  unifyList ts = unify $ zip ts (take n (tail $ concat $ repeat ts))
    where n = length ts

  -- | Unify one pair of elements
  unify1 :: UnifClass s m => a -> a -> StateT Γ m (Maybe Θ)
  unify1 a b = unify [(a, b)]

  -- | Unify one pair of elements
  match1 :: UnifClass s m => a -> a -> StateT Γ m (Maybe Θ)
  match1 a b = match [(a, b)]

instance Unif a => Unif [a] where
  unify ps = if all (\(l1, l2) -> length l1 == length l2) ps
              then unify $ concatMap (uncurry zip) ps
              else return Nothing

  match ps = if all (\(l1, l2) -> length l1 == length l2) ps
              then match $ concatMap (uncurry zip) ps
              else return Nothing

{- 
This utility function is where we branch on the sort of the term.  We do
this indirectly via the sort tags.
-} 

comb :: UnifClass s m => 
       (forall σ. Unif.Unif Term σ => σ -> Term -> Term -> StateT Γ m (Maybe Θ)) 
       -> Maybe Θ -> (Base, Term, Term) -> StateT Γ m (Maybe Θ)
comb f acc (σ, t1, t2) = do 
  res <- case acc of 
    Nothing -> return Nothing
    Just θ -> do
      let t1' = t1 ✴ θ 
          t2' = t2 ✴ θ 
      res <- f' t1' t2'
      case res of 
        Nothing -> return Nothing
        Just θ' -> return $ Just $ θ ○ θ'
  return res
 where 
   f' = case σ of 
     Sort.LWorld -> f LWorld.Tag 
     Sort.Head   -> f Head.Tag 
     Sort.LFrame -> f LFrame.Tag
     Sort.Real   -> f Dlo.Tag 
     Sort.OWorld -> f OWorld.Tag
     Sort.OFrame -> f OFrame.Tag
     Sort.Principal -> f Unif.U
     Sort.Int -> f Unif.U
     Sort.U -> f Unif.U
     Sort.MWorld -> __IMPOSSIBLE__ 

combineUnif :: UnifClass s m => Maybe Θ -> (Base, Term, Term) -> StateT Γ m (Maybe Θ)
combineUnif = comb Unif.unify1

combineMatch :: UnifClass s m => Maybe Θ -> (Base, Term, Term) -> StateT Γ m (Maybe Θ)
combineMatch = comb Unif.match1

instance Unif Term where
  unify1 t1 t2 = do
    res <- case (t1, t2) of 
      (Var x, Var x') | x == x' -> return $ Just $ ι
      (Var x, _) | x ∉ vars t2 -> return $ Just $ x ↦ t2
                 | otherwise -> return Nothing
      (_, Var x) | x ∉ vars t1 -> return $ Just $ x ↦ t1
                 | otherwise -> return Nothing
      (Param c1, Param c2) 
        | c1 == c2 -> return $ Just ι
        | otherwise -> return Nothing
      (Fn f _, _) -> handleFn f
      (_, Fn f _) -> handleFn f 
    return res
   where 
     handleFn f = do
      sig <- M.lift Sig.get
      case Sig.lookup f sig of
        Nothing -> do 
          Log.emergencyM "Class.unify1" $ "No sort for function: " ++ Func.name f
          __IMPOSSIBLE__ 
        Just (Sort.Fun _ σ) -> combineUnif (Just ι) (σ, t1, t2)
        Just σ -> do 
          Log.emergencyM "Class.unify1" $ "Weird sort for function: " ++ Func.name f ++ " : " ++ show σ 
          __IMPOSSIBLE__ 
  match1 t1 t2 = do 
    res <- case (t1, t2) of 
      (Var x, Var x') | x == x' -> return $ Just $ ι
      (Var x, _) -> return $ Just $ x ↦ t2
      (_, Var _) -> return Nothing
      (Param c1, Param c2) 
        | c1 == c2 -> return $ Just ι
        | otherwise -> return Nothing
      (Fn f _, _) -> handleFn f 
      (_, Fn f _) -> handleFn f 
    return res
   where
     handleFn f = do
      sig <- M.lift Sig.get
      case Sig.lookup f sig of
        Nothing -> do 
          Log.emergencyM "Class.unify1" $ "No sort for function: " ++ Func.name f
          __IMPOSSIBLE__
        Just (Sort.Fun _ σ) -> combineMatch (Just ι) (σ, t1, t2)
        _ -> __IMPOSSIBLE__

-- To unify two atoms, we unify the lists of arguments.

instance Unif Atom where
  unify1 (Rel p1 ts1) (Rel p2 ts2) 
    | p1 /= p2 = return Nothing
    | length ts1 /= length ts2 = do 
        Log.emergencyM "Class.unify1" $ "arity problem: " ++ Pred.name p1
        __IMPOSSIBLE__
    | otherwise = unify (zip ts1 ts2)
  match1 (Rel p1 ts1) (Rel p2 ts2) 
    | p1 /= p2 = return Nothing
    | length ts1 /= length ts2 = do 
        Log.emergencyM "Class.unify1" $ "arity problem: " ++ Pred.name p1
        __IMPOSSIBLE__
    | otherwise = match (zip ts1 ts2)

{- 
We need to unify worlds as well.
-} 

instance Unif Modal.Atom where
  unify1 (Modal.Rel p1 ts1 w1) (Modal.Rel p2 ts2 w2) 
    | p1 /= p2 = return Nothing
    | length ts1 /= length ts2 = do 
        Log.emergencyM "Class.unify1" $ "arity problem: " ++ Pred.name p1
        __IMPOSSIBLE__
    | otherwise = do
      res1 <- unify (zip ts1 ts2)
      case res1 of 
        Nothing -> return Nothing
        Just θ -> do
         res2 <- M.lift $ SolverM.simplify ([(w1, Modal.ε, w2)] ✴ θ)
         case res2 of 
           Nothing -> return Nothing
           Just θ' -> return $ Just $ θ ○ θ'
  match1 (Modal.Rel p1 ts1 w1) (Modal.Rel p2 ts2 w2) 
    | p1 /= p2 = return Nothing
    | length ts1 /= length ts2 = do 
        Log.emergencyM "Class.match1" $ "arity problem: " ++ Pred.name p1
        __IMPOSSIBLE__
    | otherwise = do
      res1 <- match (zip ts1 ts2)
      case res1 of 
        Nothing -> return Nothing
        Just θ -> do
         res2 <- M.lift $ SolverM.simplify ([(w1, Modal.ε, w2)] ✴ θ)
         case res2 of 
           Nothing -> return Nothing
           Just θ' -> return $ Just $ θ ○ θ'


{- | 
/Multi-sorted Unification/

Unification is one of the most complex operations in the theorem prover.
The reason it's more complex than usual is that we use different algorithms
depending on the sort of the elements being unified.  So for instance, 
unifying a ⋆ b ≗ b ⋆ a at type LWorld (a linear logic world) results in
the identitu substitution, while unification of the same at the uninterpreted
function sort results in failure.  There are at least 7 different unification
algorithms being used.  

The idea is as follows.  Terms are represented uniformly in the usual @Term@ tree structure.
However, other types where unification occurs may be abstract.  Thus, there
is the @Encode@ type class that allows you to @encode@ a type into a @Term@ and
@decode@ it to a different type.  

To run a particular algorithm, you @decode@ the term at a given sort and run
the local unification algorithm.  Then you @encode@ the result.  Since we
want to have a uniform interface to the unification algorithms, the signature
is prescribed by the @Unif@ type class.  An instance of the class 

In this module we also implement unification for uninterpreted
functions.  The algorithm has been adapted (i.e. stolen) from

  * Nipkow and Baader,  \"Term Rewriting and All That\", p. 79-81. 
    <http://www4.informatik.tu-muenchen.de/~nipkow/TRaAT/>

We needed to extend the definitions there with unification of
parameters, but the algorithm is essentially the same.

Note that the algorithm is blatantly exponential.  Unifying mutable
term graphs is a possibility, but would be a significant pain to
implement.  (Not just in Haskell, though the difficulties here are
more significant than in ML.)
-} 

-- @ Pragmas

{-# LANGUAGE ScopedTypeVariables, MultiParamTypeClasses, 
             FlexibleContexts, FlexibleInstances, UndecidableInstances,
             OverlappingInstances #-} 

-- @ Signature 

module Imogen.Unif
  ( -- * Generic Unification 
    Unif(..) 
    -- * Uninterpreted function instance
  , U(..)
  )
where 

-- @ Imports

import Imogen.Util.Prelude 
import qualified Data.Maybe as Maybe
import qualified Imogen.Ctx as Ctx
import Imogen.Ctx (Γ)
import qualified Imogen.CSubst as CSubst
import Imogen.CSubst (Θ, ι, Apply(..), (○))
import qualified Imogen.Sort as Sort
import Imogen.Sort (Tag, TagOf)
import qualified Imogen.Subst as Subst
import Imogen.Subst ((↦), (⟼))
import Imogen.Term (Term(..), Encode, decode)
import qualified Imogen.Util.Fresh as Fresh
import Imogen.Util.Fresh (HasFresh)
import qualified Imogen.Util.Log as Log
import Imogen.Util.Log (Log)
import qualified Imogen.Util.Pair as Pair
import Imogen.Var (Var)

-- @ Unification

{- |
Unification is parameterized by the type being unified (to values of which you 
must be able to apply a substitution), and a @Tag@ σ.  There is one tag type
for each unification variant.  (This is enforced by functional dependencies
in "Imogen.Sort".)  The tag is what allows the dynamic dispatch
at a given sort.

Unification can invent new variables, and thus we need to return a context
for the invented variables.  Matching can't invent new variables, but we
sometimes implement matching via unification composed with freezing, so
we keep the same signature.

The instance can either implement 'unify' for a list of equations, or
'unify1' that takes a single equation.
-} 

class (Apply a, Tag σ) => Unif a σ where
  unify :: (MonadState s m, HasFresh Var s, Log m) => σ -> [(a, a)] -> StateT Γ m (Maybe Θ)
  unify _ [] = return $ Just ι
  unify tag ((a, b) : rest) = do
    r1 <- unify1 tag a b
    case r1 of
      Nothing -> return Nothing
      Just θ -> do
        r2 <- unify tag (rest CSubst.✴ θ)
        return $ fmap (θ ○) r2
  match :: (MonadState s m, HasFresh Var s, Log m) => σ -> [(a, a)] -> StateT Γ m (Maybe Θ)
  match _ [] = return $ Just ι
  match tag ((a, b) : rest) = do
    r1 <- match1 tag a b
    case r1 of
      Nothing -> return Nothing
      Just θ -> do
        r2 <- match tag (rest CSubst.✴ θ)
        case r2 of 
          Nothing -> return Nothing
          Just θ' -> return $ Just $ θ ○ θ'
  unifyList :: (MonadState s m, HasFresh Var s, Log m) => σ -> [a] -> StateT Γ m (Maybe Θ)
  unifyList tag ts = unify tag $ zip ts (take n (tail $ concat $ repeat ts))
    where n = length ts
  unify1 :: (MonadState s m, HasFresh Var s, Log m) => σ -> a -> a -> StateT Γ m (Maybe Θ)
  unify1 tag a b = unify tag [(a, b)]
  match1 :: (MonadState s m, HasFresh Var s, Log m) => σ -> a -> a -> StateT Γ m (Maybe Θ)
  match1 tag a b = match tag [(a, b)]

-- | To unify a list, unify the elements componentwise.

instance Unif a σ => Unif [a] σ where
  unify tag = unify tag . concatMap (uncurry zip)
  match tag = match tag . concatMap (uncurry zip)

-- | To unify a term at a given sort tag, decode the term at the sort and
-- unify at the specialized type.

instance (Encode a, TagOf tag a, Unif a tag) => Unif Term tag where
  unify tag ts = unify tag ws
    where ws :: [(a, a)]
          ws = map (Pair.map decode) ts
  match tag ts = match tag ws
    where ws :: [(a, a)]
          ws = map (Pair.map decode) ts

-- @ Implementation for uninterpreted functions and predicates

type Θ' = Subst.Θ

data U = U

instance Sort.Tag U where

instance Sort.OK Term U where
  ok _ _ = True

instance Unif Term U where
  unify _ = return . fmap CSubst.fromSubst . solve Subst.ι 
  match _ = return . fmap CSubst.fromSubst . matchs Subst.ι 

-- | The occurs check.

occurs :: Var -> Term -> Bool
occurs x (Var y) = x == y
occurs x (Fn _ ts) = any (occurs x) ts
occurs _ _ = False

solve :: Θ' -> [(Term, Term)] -> Maybe Θ'
solve θ [] = Just θ
solve θ ((t1, t2) : eqs) = 
  case (t1, t2) of 
    (Var x, _) -> if t1 == t2 then solve θ eqs else elim θ x t2 eqs 
    (_, Var x) -> elim θ x t1 eqs
    (Fn f fs, Fn g gs) 
      | f == g && length fs == length gs -> solve θ (zip fs gs ++ eqs)
      | otherwise -> Nothing
    _ -> if t1 == t2 then solve θ eqs else Nothing

{- |
elim x t eqs s 

Replace x with t in eqs and s, effectively eliminating the variable x from the problem
when x ∉ vars t
-} 

elim :: Θ' -> Var -> Term -> [(Term, Term)] -> Maybe Θ'
elim θ x t eqs = 
  if occurs x t then Nothing else 
  let θ' :: Θ' = x ↦ t  in
  solve (θ Subst.○ θ') (eqs Subst.✴ θ')

-- | Matching

matchs :: Θ' -> [(Term, Term)] -> Maybe Θ'
matchs θ [] = Just θ
matchs θ ((t1,t2) : eqs) = 
  case (t1, t2) of 
    (Var x, _) -> case Subst.lookup x θ of
      Nothing -> matchs ((x ⟼ t2) θ) eqs
      Just t | t == t2 -> matchs θ eqs
             | otherwise -> Nothing
    (Fn f fs, Fn g gs)
      | f == g && length fs == length gs -> matchs θ (zip fs gs ++ eqs)
      | otherwise -> Nothing
    _ -> if t1 == t2 then matchs θ eqs else Nothing



{- | Variable contexts Γ. 

A context is simply a mapping of variables to sorts.
-} 

-- @ Signature

module Imogen.Ctx 
  ( -- * Contexts
    Γ
    -- * Operations
  , empty
  , singleton
  , insert
  , insertMany
  , join
  , joins
  , restrict
  , lookup
  , (!)
  , member
    -- * Conversions
  , fromList
  , toList
    -- * Legal
  , legal
    -- * State
  , Has
  , get
  )
where

-- @ Imports

import Imogen.Util.Prelude hiding (lookup) 
import qualified Control.Monad.State as State
import qualified Data.Foldable as Fold
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Set ((\\))
import Imogen.Rename (Rename(rename))
import qualified Imogen.Sort as Sort
import Imogen.Sort (Base)
import Imogen.Term ()
import qualified Imogen.Util.Debug as Debug
import qualified Imogen.Util.Print as PP
import Imogen.Util.Print (Print, pPrint)
import qualified Imogen.Var as Var
import Imogen.Var (Var, Vars)

-- @ Contexts

-- | Variable Contexts.
data Γ = Γ (Map Var Base)
  deriving (Eq, Ord, Show)

-- | The empty context.
empty :: Γ 
empty = Γ Map.empty

{- | 
Insert a sorted variable into a context.  If the variable is already
present with a different sort it is an error.  

As a sanity check we don't allow linear or ordered frame variables
since that would indicate a bug in the respective unification
algorithms.
 -} 
insert :: Var -> Base -> Γ -> Γ
insert v σ ctx@(Γ m)
    | σ /= Sort.LFrame = Γ $ Map.insertWith comb v σ m
    | otherwise = error "Frame variables are not allowed!"
  where comb x y = if x == y then x else Debug.error $ PP.text "Ctx.insert: unequal" <+> pPrint (v, σ, ctx)

insertMany :: [Var] -> Base -> Γ -> Γ
insertMany xs σ ctx = foldr (\x -> insert x σ) ctx xs

-- | Create a singleton context.
singleton :: Var -> Base -> Γ
singleton x σ = Γ $ Map.singleton x σ

{- 
Restrict the context to only account for elements of the given sets.
A context should account for all the variables in a given sequent
or rule.  
-} 
restrict :: Γ -> Set Var -> Γ
restrict ctx@(Γ m) xs = 
  let xs' = Map.keysSet m 
      diff = xs \\ xs'
      ys = xs' \\ xs 
  in if Set.null diff 
     then Γ $ Set.fold Map.delete m ys
     else Debug.error $ PP.text "Ctx.restrict" <+> pPrint (ctx, xs)
      -- Γ $ Set.fold (\x -> Map.insert x Sort.LWorld) (Set.fold Map.delete m ys) diff

-- | Create a context from a list.
fromList :: [(Var, Base)] -> Γ 
fromList = foldr (uncurry insert) empty

-- | Listify.
toList :: Γ -> [(Var, Base)]
toList (Γ m) = Map.toList m

-- | Lookup a variable in the context.
lookup :: Var -> Γ -> Maybe Base
lookup x (Γ m) = Map.lookup x m

-- | Nonfailing lookup.
(!) :: Γ -> Var -> Base
(!) (Γ m) x = case Map.lookup x m of
  Nothing -> error "Ctx.!"
  Just σ -> σ 

-- | Does a variable have a sort in the context?
member :: Var -> Γ -> Bool
member x (Γ m) = Map.member x m

-- | Combine two contexts.  If they disagree on the sort of a variable it is an error.
join :: Γ -> Γ -> Γ
join (Γ m1) (Γ m2) = Γ $ Map.unionWith u m1 m2
  where u σ1 σ2 | σ1 == σ2 = σ1
                | otherwise = error "join: sorts not equal"
                  
-- | List version of join.
joins :: [Γ] -> Γ
joins = foldr join empty

-- @@ Instances

instance Rename Γ where
  rename (Γ m) = Γ <$> rename m

instance Print Γ where
  pPrint (Γ m) = PP.setHoriz . binds $ Map.toList m
    where binds l = 
            let l1 = List.sortBy (\x y -> compare (snd x) (snd y)) l
                l2 = List.groupBy (\x y -> snd x == snd y) l1
                l3 =  map (\x -> (map fst x, snd (head x))) l2
            in map bind l3
          bind (xs, σ) = 
            PP.hcat $ pPrint (head xs) :
                      (map (\x -> PP.text ", " <> pPrint x) (tail xs)) 
                      ++ [PP.text " : ", pPrint σ]

-- @ State

-- | Pass a context around in a state monad.
class Has s where
  state :: s -> Γ

instance Has Γ where
  state s = s

-- | Grab the context from the state.
get :: (MonadState s m, Has s) => m Γ
get = State.gets state

-- @ Legal

{- |
A context Γ is /legal/ for an object if it gives sorts to all the (free) variables
of the object
-} 
legal :: Vars a => Γ -> a -> Bool
legal ctx = Fold.all (flip member ctx) . Var.free

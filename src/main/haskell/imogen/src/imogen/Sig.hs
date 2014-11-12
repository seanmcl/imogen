
{- | /Signatures/.

Signatures serve two purposes.  Principally, they are mappings of \"constants\" to 'Sort's.  Constants in this context
means one of three types:

 * Parameters

 * Function symbols

 * Predicate symbols

Their secondary purpose is to assign /polarities/ to (atomic) predicate symbols.  For 
example, input formulas parse p(X) as a positive atomic proposition and P(X) as
negative.  This information is recorded in the signature.
-} 

-- @ Signature 

module Imogen.Sig 
  ( -- * Signature entries 
    Entry(..)
    -- * Signatures
  , Σ 
  , empty
    -- * Operations
  , insert
  , member
  , lookup
  , join
  , joins
  , fromList
  , toList
    -- * Legal
  , legal
    -- * MonadState class 
  , Has(..)
  , get
  , put
  , modify
    -- * Polarities
  , Polarity(..)
  , opp
  , polarity
  , setPolarity
  )
where

-- @ Imports

import Imogen.Util.Prelude hiding (lookup)
import qualified Control.Monad.State as State
import qualified Data.Foldable as Fold
import qualified Data.Map as Map
import qualified Imogen.Func as Func
import Imogen.Func (Func, Funcs)
import qualified Imogen.Param as Param
import Imogen.Param (Param, Params)
import qualified Imogen.Pred as Pred
import Imogen.Pred (Pred, Preds)
import Imogen.Sort (Base(..), Sort(..))
import qualified Imogen.Term as Term
import qualified Imogen.Util.Debug as Debug
import qualified Imogen.Util.Print as PP
import Imogen.Util.Print (Print, pPrint)

-- @ Signatures

data Entry = Param Param
           | Func Func
           | Pred Pred
  deriving (Eq, Ord, Show)

data Polarity = Pos | Neg
  deriving (Eq, Ord, Show)

-- | The signature type.
data Σ = Σ { toEntryMap :: Map Entry Sort 
           , toPolarityMap :: Map Pred Polarity 
           }
  deriving (Eq, Ord, Show)

-- * Operations

class Sig a where
  -- | Insert an object with its sort.  
  --   Note that the elements of the hidden class Sig are 'Param', 'Func', and 'Pred'.
  insert :: a -> Sort -> Σ -> Σ
  -- | See if an object is present.
  --   Note that the elements of the hidden class Sig are 'Param', 'Func', and 'Pred'.
  member :: a -> Σ -> Bool
  -- | Lookup the sort of an object.
  --   Note that the elements of the hidden class Sig are 'Param', 'Func', and 'Pred'.
  lookup :: a -> Σ -> Maybe Sort

instance Sig Param where
  insert = insert' . Param
  member = member' . Param
  lookup = lookup' . Param

instance Sig Func where
  insert = insert' . Func
  member = member' . Func
  lookup = lookup' . Func

instance Sig Pred where
  insert = insert' . Pred
  member = member' . Pred
  lookup = lookup' . Pred

insert' :: Entry -> Sort -> Σ -> Σ
insert' x σ sig@(Σ m m') = case Map.lookup x m of
  Nothing -> Σ (Map.insert x σ m) m'
  Just σ' | σ == σ' -> sig
          | otherwise -> Debug.error $ 
             PP.vcat [ PP.text "Conflicting mappings for" <+> pPrint x
                     , pPrint σ <+> PP.text "/=" <+> pPrint σ'
                     ]

member' :: Entry -> Σ -> Bool
member' x = Map.member x . toEntryMap

lookup' :: Entry -> Σ -> Maybe Sort
lookup' x = Map.lookup x . toEntryMap

{- |
The \"empty\" signature contains the sorts of all the built-in function
and predicate symbols.
-} 
empty :: Σ
empty = Σ (Map.fromList
  [  -- Arithmetic
    (Func $ Func.make "+", Fun [Int, Int] Int)
  , (Pred $ Pred.make "=", Rel [Int, Int])
  , (Pred $ Pred.make "<", Rel [Int, Int])
    -- Linear logic
  , (Pred $ Pred.make "ⓦ", Rel [U, LWorld])
  , (Pred $ Pred.make "ⓕ", Rel [U, LFrame])
  , (Pred $ Pred.make "◃", Rel [Head, LWorld])
  , (Func $ Func.make "⊛", Fun [Head, LWorld] LFrame)
  , (Func $ Func.make "⋆", Fun [LWorld, LWorld] LWorld)
  , (Func $ Func.make "ε", Fun [] LWorld)
    -- Ordered logic
  , (Pred $ Pred.make "Ⓦ", Rel [U, OWorld])
  , (Pred $ Pred.make "Ⓕ", Rel [U, OFrame])
  , (Pred $ Pred.make "◂", Rel [Head, OWorld])
  , (Func $ Func.make "⊙", Fun [OWorld, Head, OWorld] OFrame)
  , (Func $ Func.make "·", Fun [OWorld, OWorld] OWorld)
  , (Func $ Func.make "ع", Fun [] OWorld)
  , (Func $ Term.func Term.defaultPrincipal, Fun [] Principal)
     -- Modal logic
  , (Pred $ Pred.make "≗", Rel [MWorld, MWorld, MWorld])
  , (Pred $ Pred.make "path", Rel [MWorld])
  , (Pred $ Pred.make "edge", Rel [MWorld])
  ]) Map.empty

-- | Create a signature from a list.
fromList :: [(Entry, Sort)] -> Σ 
fromList = foldr (\(x, σ) ctx -> insert' x σ ctx) empty

-- | Grab the 'Sort' component of the signature.
toList :: Σ -> [(Entry, Sort)] 
toList = Map.toList . toEntryMap

{- | 
Take the union of two signatures.  The two signatures may both contain
the same symbol, but they must agree about the sort of the symbol.  If
not it is a fatal error.
-} 
join :: Σ -> Σ -> Σ
join (Σ v1 p1) (Σ v2 p2) = Σ (Map.unionWith u v1 v2) (Map.unionWith u p1 p2)
  where u σ1 σ2 | σ1 == σ2 = σ1
                | otherwise = error "join: sorts not equal"
                  
-- | The 'join' of a list.
joins :: [Σ] -> Σ
joins = foldr join empty

-- @ Class

-- | An easy way to thread a signature around is in the MonadState class.
class Has s where
  fromState :: s -> Σ
  toState :: Σ -> s -> s

-- | Grab the signature from a state
get :: (MonadState s m, Has s) => m Σ
get = State.gets fromState

-- | Put the signature back into a state.
put :: (MonadState s m, Has s) => Σ -> m ()
put = State.modify . toState 

-- | Modify the signature of a state in place.
modify :: (MonadState s m, Has s) => (Σ -> Σ) -> m ()
modify f = State.modify (\s -> toState (f $ fromState s) s)

-- Some instances for testing.

instance Has Σ where
  fromState s = s
  toState sig _ = sig

instance Has b => Has (a, b) where
  fromState (_, s) = fromState s
  toState sig (a, b) = (a, toState sig b)

instance Has b => Has (a, b, c) where
  fromState (_, s, _) = fromState s
  toState sig (a, b, c) = (a, toState sig b, c)

-- @ Polarity

-- | > opp Pos == Neg && opp Neg == Pos
opp :: Polarity -> Polarity
opp Pos = Neg
opp Neg = Pos

-- | Lookup the polarity of a predicate symbol.
polarity :: Pred -> Σ -> Maybe Polarity
polarity x = Map.lookup x . toPolarityMap

{- | 
Set the polarity of a predicate symbol.  If the symbol already has
a polarity it is an error.
-} 
setPolarity :: Pred -> Polarity -> Σ -> Σ
setPolarity x p (Σ sm pm) = Σ sm $ Map.insertWith (error "Polarity already set") x p pm

-- @ Legal

{- |
A signature Σ is legal for an object if all the parameters, function and predicate 
symbols are assigned a sort by Σ
-} 
legal :: (Funcs a, Preds a, Params a) => Σ -> a -> Bool
legal sig a = Fold.all (flip member sig) (Pred.preds a) &&
              Fold.all (flip member sig) (Func.funcs a) &&
              Fold.all (flip member sig) (Param.params a)

-- @ Printing

instance Print Entry where
  pPrint e = case e of 
    Param p -> pPrint p
    Func p -> pPrint p
    Pred p -> pPrint p

instance Print Σ where
  pPrint (Σ sm pm) = PP.setVert $ map ppBind binds
    where binds = Map.toList sm
          ppBind (Pred s, σ) = case Map.lookup s pm of 
            Nothing -> pPrint s <+> PP.text ":" <+> pPrint σ
            Just pol -> pPrint s <> ppPol pol <+> PP.text ":" <+> pPrint σ
          ppBind (Func f, σ) = pPrint f <+> PP.text ":" <+> pPrint σ
          ppBind (Param f, σ) = pPrint f <+> PP.text ":" <+> pPrint σ
          ppPol Pos = PP.text "+"
          ppPol Neg = PP.text "-"

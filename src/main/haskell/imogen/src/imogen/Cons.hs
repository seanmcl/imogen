
-- | Consequents of a sequent.

-- @ Signature

module Imogen.Cons 
  ( Cons(..) )
where

-- @ Imports 

import Imogen.Util.Prelude 
import qualified Data.Maybe as Maybe
import qualified Imogen.Class as Class
import qualified Imogen.Modal as Modal
import Imogen.Modal (Atom, Worlds(worlds))
import Imogen.Param (Params(..), Petrify(..))
import Imogen.Rename (Rename(rename))
import qualified Imogen.Subst as Subst
import Imogen.Subst(Apply(..))
import qualified Imogen.Util.Print as PP
import Imogen.Util.Print (Print, pPrint)
import qualified Imogen.Util.Set as Set
import Imogen.Util.Set ((∅))
import Imogen.Var (Vars(..))

-- @ Consequents 

{- | 
The consequent of a sequent is either empty (for a left rule in the
presence of negation) or a path atom.
-} 
data Cons = Rel Atom
          | X
  deriving (Eq, Ord, Show)

-- @@ Instances

instance Apply Cons where
  X ✴ _ = X
  Rel a ✴ θ = Rel $ a ✴ θ

instance Vars Cons where
  vars (Rel a) = vars a
  vars X = (∅)
  free = vars

instance Params Cons where
  params (Rel a) = params a
  params X = (∅)

instance Rename Cons where
  rename X = return X
  rename (Rel a) = rename a >>= return . Rel

instance Petrify Cons where
  petrify _ X = X
  petrify ρ (Rel a) = Rel $ petrify ρ a

instance Class.Normalize Cons where
  normalize sig ctx (Rel (Modal.Rel p ts w)) = 
    Rel $ Modal.Rel p (Class.normalize sig ctx ts) (Class.normalize sig ctx w)
  normalize _ _ X = X

instance Class.OK Cons where
  ok sig ctx (Rel a) = Class.ok sig ctx a
  ok _ _ X = True

instance Class.Unif Cons where
  unify = Class.unify . Maybe.mapMaybe filt
    where filt eq = case eq of 
            (X, _) -> Nothing
            (_, X) -> Nothing
            (Rel p1, Rel p2) -> Just (p1, p2)
  match eqs = case filt eqs of 
    Nothing -> return Nothing
    Just eqs' -> Class.match eqs'
    where filt :: [(Cons, Cons)] -> Maybe [(Atom, Atom)]
          filt = foldr foldfn (Just [])
          foldfn _  Nothing = Nothing
          foldfn eq (Just acc) = case eq of
            (X, _) -> Just acc
            (_, X) -> Nothing
            (Rel p1, Rel p2) -> Just $ (p1, p2) : acc

instance Worlds Cons where
  worlds c = case c of
    X -> (∅)
    Rel a -> worlds a

instance Print Cons where
  pPrint X = PP.text "·"
  pPrint (Rel p) = pPrint p


{- | 
Global elements during proof search.  

Globalization is the observation that certain elements of a proof tree, such
as antecedents generated during the initial active phase, are present in every
sequent of a backward proof.  Thus, we needn't store these elements in every
search sequent.
-} 

-- @ Signature

module Imogen.Global 
  ( -- * Global elements
    Global
  , empty
  , make
  , dest
  , ants
  , constrs
    -- * For a state monad
  , Has(..)
  , get
  )
where

-- @ Imports

import Imogen.Util.Prelude 
import qualified Control.Monad.State as State
import qualified Imogen.Ants as Ants
import Imogen.Ants (Ants)
import qualified Imogen.Constr as Constr
import Imogen.Constr (Ψ, (⊤))
import Imogen.Param (Petrify(..))

-- @ Globals

{- | 
The globals in Imgogen consist of a set of antecedents, a set of constraints, 
and a context mapping parameters in the above to their sorts.  (There are never
any variables in the globals.
-} 
data Global = G { ants :: Ants         -- ^ Antecedents
                , constrs :: Ψ         -- ^ Constraints
                }
  deriving Show

-- | The empty globals.
empty :: Global
empty = G Ants.empty (⊤)

-- | Constructor.
make :: Ants -> Ψ -> Global
make = G

-- | Destructor.
dest :: Global -> (Ants, Ψ)
dest (G a b) = (a, b)

instance Petrify Global where
  petrify ρ (G ד ψ) = G (petrify ρ ד) (petrify ρ ψ)

-- instance Print Global where
--   pPrint (G ד ψ) = PP.pPrint (ד, ψ)

-- @ Class

-- | For easy passing in a state monad.
class Has s where
  state :: s -> Global

instance Has Global where
  state s = s

instance Has b => Has (a, b) where
  state (_, s) = state s

-- | Grab the globals from the current state.
get :: (MonadState s m, Has s) => m Global
get = State.gets state

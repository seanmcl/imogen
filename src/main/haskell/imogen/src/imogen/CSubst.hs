
{- |
Constrained substitutions

Recall from the documentation for "Imogen.Constr" that a constrainted substitution is
a subsitution (Θ) together with a constraint (Ψ).  
-} 

-- @ Pragmas

{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-} 

-- @ Signature

module Imogen.CSubst 
  ( -- * Constrainted substitutions
    Θ 
    -- * Application
  , Apply(..)
  , GenApply(..)
    -- * Constructors
  , make
  , (○)
  , ι
  , (↦)
  , (⟼)
  , fromSubst
  , fromConstr
    -- * Destructors
  , subst
  , constrs
  , dest
  ) 
where

-- @ Imports

import Imogen.Util.Prelude 
import qualified Imogen.Subst as Subst
import qualified Imogen.Constr as Constr
import Imogen.Constr (Ψ, (∧), (⊤))
import Imogen.Var (Var)
import Imogen.Param (Freeze(..))
import Imogen.Term (Term)

-- @ Constrainted substitutions

data Θ = Θ 
  { subst :: Subst.Θ   -- ^ Grab the substitution.
  , constrs :: Ψ       -- ^ Grab the constraints.
  }
  deriving (Eq, Ord, Show)

{- 
Composition of CSubsts is asymmetric.  We assume that ψ2 ✴ θ1 == ψ2.
This is because the use of composition arises from unifying a sequence

 (t1, t2) : eqs

where t1 ≗ t2 --> Just (θ1, ψ1) and then we call unify (eqs ✴ θ1).
-} 
(○) :: Θ -> Θ -> Θ
Θ θ1 ψ1 ○ Θ θ2 ψ2 = Θ (θ1 Subst.○ θ2) (Constr.simp $ (ψ1 Subst.✴ θ2) ∧ ψ2)

-- | The identity substitution.
ι :: Θ 
ι = Θ Subst.ι (⊤)

-- | Singleton.
(↦) :: Var -> Term -> Θ
x ↦ t = Θ (x Subst.↦ t) (⊤)

-- | Extension.
(⟼) :: Var -> Term -> Θ -> Θ
(x ⟼ t) (Θ θ ψ) = Θ ((x Subst.⟼ t) θ) ψ

-- | Primary constructor.
make :: Subst.Θ -> Ψ -> Θ 
make = Θ 

-- | Create a constraint substitution with trivial (⊤) constraints.
fromSubst :: Subst.Θ -> Θ 
fromSubst θ = Θ θ (⊤)

-- | Create a constraint substitution with an identity substitution.
fromConstr :: Ψ -> Θ 
fromConstr = Θ Subst.ι

-- | Get the parts.
dest :: Θ -> (Subst.Θ, Ψ)
dest (Θ θ ψ) = (θ, ψ)

-- @@ Instances

instance Freeze Θ where
  freeze (Θ θ ψ) = Θ (freeze θ) (freeze ψ)
  thaw cs (Θ θ ψ) = Θ (thaw cs θ) (thaw cs ψ)

-- @@ Apply

-- | Application.
class Subst.Apply a => Apply a where
  (✴) :: a -> Θ -> a

instance Subst.Apply a => Apply a where
  x ✴ Θ θ _ = x Subst.✴ θ

-- | Generic application.
class Subst.GenApply a b => GenApply a b where
  (✶) :: a -> Θ -> b

instance Subst.GenApply a b => GenApply a b where
  x ✶ Θ θ _ = x Subst.✶ θ


-- | Sequents Γ ⊢ π 

-- @ Pragmas

-- The orphan for Class abbreviations is OK.

{-# LANGUAGE CPP, Rank2Types, MultiParamTypeClasses, FunctionalDependencies
           , FlexibleContexts, FlexibleInstances #-} 

-- @ Signature

module Imogen.Seq
  ( -- * Identifiers
    Id
    -- * Sequents
  , Seq
  , Class
    -- * Components
  , constrs, ants, cons, uid, context
  , dest
  , contradictory
    -- * Construction
  , new
--  , initial
    -- * Operations
  , subsume
  , subsumes
  , subsumed
  , contract
    -- * Unsafe
  , unsafeMake
  )
where

-- @ Imports
 
#include "../undefined.h" 

import Imogen.Util.Prelude hiding (elem, log)
-- import Control.Monad.Reader (ReaderT)
import qualified Control.Monad.State as State
import qualified Data.Maybe as Maybe
import qualified Imogen.Ants as Ants
import Imogen.Ants(Ants)
import qualified Imogen.Class as Class
import Imogen.Class (UnifClass)
import Imogen.Cons(Cons(..))
import qualified Imogen.Constr as C
import Imogen.Constr(Ψ, HasSolver, (⊃), (∧), (⊥))
import qualified Imogen.CSubst as CSubst
import Imogen.CSubst(Θ, (○), (✴))
import qualified Imogen.Ctx as Ctx
import Imogen.Ctx (Γ)
import qualified Imogen.Global as Global
import Imogen.Global (Global)
import qualified Imogen.Modal as Modal
import Imogen.Modal (HasMode, Worlds)
import qualified Imogen.Param as Param
import Imogen.Param (Param, Petrify(..))
import qualified Imogen.Path as Path
import qualified Imogen.Print as Print
import Imogen.Print (Print, pPrint)
import qualified Imogen.Rename as Rename
import Imogen.Rename (Rename(rename))
import qualified Imogen.Sig as Sig
import qualified Imogen.Subst as Subst
import Imogen.Subst (Apply, ι)
import qualified Imogen.Util.Counter as Counter
import Imogen.Util.Counter (Counter, HasCounter)
import qualified Imogen.Util.Fresh as Fresh
import Imogen.Util.Fresh (HasFresh)
import qualified Imogen.Util.Log as Log
import Imogen.Util.Log (Log)
import qualified Imogen.Util.Monad as M
import qualified Imogen.Util.Print as PP
import qualified Imogen.Util.Set as Set
import Imogen.Util.Set ((∪))
import Imogen.Util.Three (Three(..))
import qualified Imogen.Var as Var
import Imogen.Var (Var)

-- @ Identifiers

-- | Identifiers.
type Id = Counter Seq

-- newtype Id = Id (Counter Seq)
--   deriving (Eq, Ord, Show)

-- instance PP.Print Id where
--   pPrint (Id c) = PP.pPrint c

-- @ Sequents

{- 
| Sequents have the form Γ ⊢ π(t) where π(t) is either a path atom or a
variable (the consequent) and Γ is a mapping of paths to sets of argument 
lists.
-} 

-- | Sequents.
data Seq = Seq { uid     :: Counter Seq  -- ^ Unique identifier.
               , constrs :: Ψ            -- ^ Constraints.
               , ants    :: Ants         -- ^ Antecedents.
               , cons    :: Cons         -- ^ Consequent.
               , context :: Γ            -- ^ Context.
               }
  deriving (Eq, Ord, Show)

instance Class.OK Seq where
  ok sig ctx (Seq _ _ ד γ _) = Class.ok sig ctx ד && Class.ok sig ctx γ

instance Print Seq where
  pPrint sig _ q = 
    let ctx = context q in
    PP.fsep [ PP.text "q" <> PP.pPrint (uid q) <+> PP.text ":"
            , pPrint sig ctx (constrs q), PP.text "|"
            , pPrint sig ctx (ants q), PP.text "⊢"
            , PP.hsep [ pPrint sig ctx (cons q)
                      , PP.pPrint (context q) 
                      ]
            ]

instance Print [Seq] where
  pPrint sig ctx = PP.vcat . map (pPrint sig ctx)

instance Worlds Seq where
  worlds q = Modal.worlds (cons q) ∪ Modal.worlds (ants q)

-- | A sequent with contradictory constraints.
contradictory :: Seq
contradictory = Seq Counter.dummyCtr (⊥) Ants.empty X Ctx.empty

{- | 
This should be used for testing only.  It exists only so we can parse
a Seq from a String.
-} 
unsafeMake :: Ψ -> Ants -> Cons -> Γ -> Seq
unsafeMake = Seq Counter.dummyCtr

-- @ Class

class ( MonadState s m, HasFresh Var s, Sig.Has s, Log m
      , UnifClass s m, Functor m,  HasFresh Param s, Global.Has s, HasMode s
      , Path.HasLabels s, HasSolver s, HasCounter Seq s ) => Class s m | m -> s where

-- instance Class s m => Class s (ReaderT r m) where
-- instance Class s m => UnifClass s m where

instance Apply Seq where
  Seq qid ψ ד γ ctx ✴ θ = 
    Seq qid (ψ Subst.✴ θ) (ד Subst.✴ θ) (γ Subst.✴ θ) ctx

instance Class.Normalize Seq where
  normalize sig _ctx (Seq qid ψ ד γ ctx) = 
    Seq qid ψ 
            (Class.normalize sig ctx ד) 
            (Class.normalize sig ctx γ) ctx

instance Petrify Seq where
  petrify ρ (Seq qid ψ ד γ ctx) = 
    Seq qid (petrify ρ ψ) (petrify ρ ד) (petrify ρ γ) ctx

-- Make sure the given context gives sorts to all variables and parameters.

legal :: Γ -> Ψ -> Ants -> Cons -> Bool
legal ctx ψ ד γ = Set.all (flip Ctx.member ctx) (Var.free (ψ, ד, γ))

{- | Create a new sequent.

Postconditions:
 -} 
new :: Class s m => Γ -> Ψ -> Ants -> Cons -> m Seq
new ctx ψ ד γ = do
  sig <- Sig.get
  solver <- State.gets C.getSolver
  Log.debugM' "Seq.new" $ PP.text "Input:" <+> pPrint sig ctx (ctx, ψ, ד, γ, solver)
  -- Make sure the context is legal
  if not $ legal ctx ψ ד γ
    then do Log.emergencyM' "Seq.new" $ PP.text "Bad initial context:" <+> 
               pPrint sig ctx (ctx, ψ, ד, γ)
            __IMPOSSIBLE__ 
    else return ()
  -- Close the constraints (existentially quantify vars not occuring in the body)
  let ψ1 = C.closeEx ctx (Var.free (ד, γ)) ψ
  -- Simplify the constraints, generating new constraints, a substitution and a new context
  (simp, ctx1) <- State.runStateT (C.simplify sig ψ1) ctx
  -- let (θ, ψ2) = (ι, ψ1)
  let (θ, ψ2) = case simp of 
        Nothing -> (ι, ψ1)
        Just r -> r
      (ד1, γ1) = (ד, γ) Subst.✴ θ
      -- ψ3 = C.liftW (seqWorlds ∩) ψ2
  glob <- Global.get
  -- Remove global antecedents
  let ד2 = Ants.difference ד1 (Global.ants glob)
  -- Rename variables
  (ψ4, ד3, γ2, ctx2) <- State.evalStateT (rename (ψ2, ד2, γ1, ctx1)) Rename.empty
  -- New counter
  uid1 <- Counter.tick
  -- Restrict the context to the remainging vars and params
  let ctx3 = Ctx.restrict ctx2 $ Var.free (ψ4, ד3, γ2)
      res = Class.normalize sig ctx3 $ (Seq uid1 ψ4 ד3 γ2 ctx3) 
  Log.debugM' "Seq.new" $ PP.vcat [ PP.text "Input:" <+> pPrint sig ctx (ctx, ψ, ד, γ)
                                  , PP.text "Output:" <+> pPrint sig ctx3 res ]
  return res

-- | Destructor.
dest :: Seq -> (Ψ, Ants, Cons)
dest (Seq _ ψ ד γ _) = (ψ, ד, γ)

-- -- | Initial sequents have the form ⊤ | π ⊢ π
-- initial :: Class s m => Γ -> Atom -> Atom -> m Seq
-- initial ctx π1 π2 = new ctx C.init (Ants.singleton π1) (Rel π2) 

-- @ Subsumption

{- 
| Subsumption is a critical relation in all forward theorem provers.  
The intuition is that some facts contain strictly more information than
others, and we can discard those with less information.  

A sequent 

ψ1 | Γ1 ⊢ p1 

subsumes 

ψ2 | Γ2 ⊢ p2

if we can find a substitution θ such that

  1) p1 θ = p2
  2) Γ1 θ ⊆ Γ2
  3) ψ2 ⊧ ψ1 θ
-} 

subsume :: Class s m => Seq -> Seq -> m (Maybe Θ)
subsume q1@(Seq _ ψ1 ד1 γ1 ctx1) q2@(Seq _ ψ2 ד2 γ2 ctx2) = do
  -- Some preliminary work:
  -- * Grab the global signature
  sig <- Sig.get
  -- * Combine the contexts of the two sequents.  Any variable in
  --   the resulting substitution will have a sort in this context.
  let ctx = Ctx.join ctx1 ctx2
  -- * Grab the global constraints
  ψg <- Global.constrs <$> Global.get
  -- * A good substitution is one that satisfies ψg ∧ ψ2 ⊃ ψ1 θ
  let goodSubst (θ, ctx') = State.evalStateT (C.valid sig ((ψg ∧ ψ2) ⊃ (ψ1 ✴ θ))) ctx'
  -- Now we begin the subsumption tests
  -- * Match the consequent of q1 to the consequent of q2
  res <- State.evalStateT (Class.match1 γ1 γ2) ctx
  res' :: Maybe Θ <- case res of 
    -- * If the consequents don't match, fail
    Nothing -> return Nothing
    -- * Otherwise apply the matching substitution to the antecedents of q1
    Just θ1 -> do
      θs <- Ants.match ctx (ד1 ✴ θ1) ד2
      -- And drop the context
      fmap (fmap fst) (M.findM goodSubst (map (first (θ1 ○)) θs))
  Log.debugM' "Seq.subsume" $ 
     PP.vcat [ PP.text "q1  :" <+> pPrint sig ctx q1
             , PP.text "q2  :" <+> pPrint sig ctx q2 
             , PP.text "res :" <+> pPrint sig ctx res
             , PP.text "res':" <+> pPrint sig ctx res' 
             ]
  return res'

-- | Check subsumption without returning the substitution.
subsumes :: Class s m => Seq -> Seq -> m Three
subsumes x y = 
  let ctx = Ctx.join (context x) (context y) in do
  sig <- Sig.get
  s <- subsume x y 
  case s of 
    Nothing -> return No
    Just θ -> do
      M.ifM (State.evalStateT (C.valid sig (CSubst.constrs θ)) ctx)
        (return Yes) (return Unsure)

-- | flip subsumes
subsumed :: Class s m => Seq -> Seq -> m Three
subsumed = flip subsumes 

-- @ Contraction

{- 
Contraction for sequents is a simple matter of contracting the antecedents
and for each contract, applying the substitution to the consequent.
-} 

contract :: forall s m. Class s m => Seq -> m [(Seq, Θ)] 
contract q@(Seq _ ψ ד γ ctx) = do  
  sig <- Sig.get
  -- Log.debugM' "Seq.contract" $ pPrint sig ctx q
  glob <- Global.get
  let ok θ = Class.ok sig ctx (γ ✴ θ)
  cs <- Ants.contract (Global.ants glob) ok ctx ד 
  res <- mapM f cs
  Log.debugM' "Seq.contract" $ 
     PP.vcat [ PP.text "q   :" <+> pPrint sig ctx q
             , PP.text "res':" <+> pPrint sig ctx res
             ]
  -- Log.debugM "Seq.contract" $ "Done..."
  return res
 where -- f :: (Θ, Γ, Ants) -> m (Seq, Θ)
       f (θ, ctx', ד1) = do
         q' <- new (Ctx.join ctx ctx') (ψ ✴ θ) ד1 (γ ✴ θ) 
         return $ (q', θ)

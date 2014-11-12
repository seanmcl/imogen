
-- | Inference rules

-- @ Pragmas

{-# LANGUAGE CPP, MultiParamTypeClasses, FunctionalDependencies, 
             FlexibleContexts, FlexibleInstances #-} 

-- @ Signature

module Imogen.Rule
  ( -- * Inference rules
    Rule
  , RSeq(..)
  , Class
    -- * Constructor
  , new
    -- * Destructors
  , uid
  , context
  , hyps
  , concl
  , fixedParams
  , constrs
    -- * Rule application
  , apply
    -- * Unsafe
  , unsafeMake
  )
where

-- @ Imports

#include "../undefined.h" 

import Imogen.Util.Prelude hiding (log)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Control.Monad.State as State
import qualified Imogen.Ants as Ants
import Imogen.Ants (Ants)
import qualified Imogen.Class as Class
import Imogen.Cons (Cons(..))
import qualified Imogen.Constr as C
import Imogen.Constr (Ψ, (∧))
import qualified Imogen.CSubst as CSubst
import Imogen.CSubst (Apply, Θ, ι, (○), (✴), (✶))
import qualified Imogen.Ctx as Ctx
import Imogen.Ctx (Γ)
import qualified Imogen.Modal as Modal
import Imogen.Modal (Atom, Mode)
import qualified Imogen.Param as Param
import Imogen.Param(Param, Params, Petrify(..))
import qualified Imogen.Print as Print
import Imogen.Print (Print, pPrint)
import qualified Imogen.Rename as Rename
import Imogen.Rename (Rename, rename)
import qualified Imogen.Sig as Sig
import qualified Imogen.Seq as Seq
import Imogen.Seq (Seq)
import qualified Imogen.Subst as Subst
import Imogen.Term (Term)
import qualified Imogen.Util.Counter as Counter
import Imogen.Util.Counter (HasCounter, Counter)
import qualified Imogen.Util.Log as Log
import qualified Imogen.Util.Monad as M
import qualified Imogen.Util.Print as PP
import qualified Imogen.Util.Set as Set
import Imogen.Util.Set ((∉), (∩))
import qualified Imogen.Var as Var
import Imogen.Var (Var, Vars)

-- @ Sequents

{- 
An inference rule is a list of hypothesis sequents and a conclusion
sequent, along with a set of the generataed parameters.  Note that
sequents occurring as hypotheses are different than database sequents,
since they don't mention constraints.  The conjunction of the
constraints are collected in the conclusion.

      Γ, A ⊢ · 
--------------------{}
 ψ | Γ, A ∧ B ⊢ · 

   Γ ⊢ A(ç) 
---------------{ç}
 ψ | Γ ⊢ ∀ x. A(x)

-} 

data RSeq = RSeq Ants Cons
  deriving (Eq, Ord, Show)

instance Vars RSeq where
  vars (RSeq ד γ) = Var.vars (ד, γ)
  free (RSeq ד γ) = Var.free (ד, γ)

instance Params RSeq where
  params (RSeq ד γ) = Param.params (ד, γ)

instance Subst.Apply RSeq where
  RSeq ד γ ✴ θ = RSeq (ד Subst.✴ θ) (γ Subst.✴ θ)

instance Rename RSeq where
  rename (RSeq ד γ) = rename (ד, γ) >>= return . uncurry RSeq 

instance Petrify RSeq where
  petrify ρ (RSeq ד γ) = RSeq (petrify ρ ד) (petrify ρ γ)

instance Class.Normalize RSeq where
  normalize sig ctx (RSeq ד γ) = RSeq (Class.normalize sig ctx ד) (Class.normalize sig ctx γ)

instance Print RSeq where
  pPrint sig ctx (RSeq ד γ) = PP.sep [pPrint sig ctx ד, PP.text "⊢" <+> pPrint sig ctx γ]

addAnts :: Ants -> RSeq -> RSeq
addAnts ד' (RSeq ד γ) = RSeq (Ants.union ד ד') γ

-- @ Constraints

{-
Rule application is significantly complicated by constraints.  This
has primarily to do with the nesting of quantifiers.  For instance,
consider the following deduction

           Ψ | Γ ---> A @ π 
---------------------------------------
 ∀ e. π ≡ π' · e ∧ Ψ | Γ ---> A @ π'

This rule is rather useless since there is no possible
π that satisfies π ≡ π' · e for different edges e.  The
problem is that if π doesn't occur in Γ, the constraint should be

           Ψ | Γ ---> A @ π 
-----------------------------------------------
 ∀ e. ∃ π. π ≡ π' · e ∧ Ψ | Γ ---> A @ π'

Of course, this later rule can't be correct if π occurs in Γ, since
the scoping would be wrong.  Thus, determining the shape of the constraint 
of a rule application requires knowledge of the entire sequent, not just
the constraint part of the input.  

We implement this dependency by making the constraint part of a 
rule a function from sequents to constraints.  Given an input
sequent, the function should figure out what the constraint is
given the existential elements. 

Note further that when you apply the rule, you must first
nondeterministically choose the subset of the antecedents and
the mapping of the subset to the rule antecedents.  This 
generates a slew of unification equations.  Since those 
need to get put in the correct location also, the 
function that creates the constratint must take
those equations as an argument also.

Problem:  Initial stabilization phase may not be separable because
of the constraint entailment that must hold at the end.  Investigate
this.

-} 

data RConstr = Leaf Ψ 
             | Node (Seq -> [Eqn] -> RConstr)

applyRConstr :: RConstr -> Seq -> RConstr
applyRConstr (Leaf _) _ = error "Not a node" 
applyRConstr (Node f) x = f x

-- @ Rules

data Rule = Rule { uid :: Counter Rule
                 , hyps :: [RSeq]
                 , concl :: RSeq
                 , fixedParams :: Set Param
                 , rconstr :: RConstr
                 , context :: Γ 
                 }
  deriving (Eq, Ord, Show)

instance Class.Normalize Rule where
  normalize sig _ctx (Rule uid' ζ δ ρ ψ ctx) = 
    Rule uid' (Class.normalize sig ctx ζ) (Class.normalize sig ctx δ) ρ ψ ctx

-- Note: Stable parameters should not show up in any inference rule
-- generated parameter list.

instance Petrify Rule where
  petrify ρ (Rule uid' ζ δ ρ' ψ ctx) 
   | (Map.keysSet ρ ∩ ρ') /= Set.empty = 
     __IMPOSSIBLE__
   | otherwise = Rule uid' (petrify ρ ζ) (petrify ρ δ) ρ' (petrify ρ ψ) ctx

instance Print Rule where
  pPrint sig _ r = 
    let ctx = context r
        hypos = PP.vcat $ PP.commas $ map (pPrint sig ctx) (hyps r)
        conc = pPrint sig ctx (constrs r) <+> PP.text "|" <+> pPrint sig ctx (concl r)
        n = min 60 (max (length $ PP.render hypos) (length $ PP.render conc))
    in PP.vcat [ hypos
               , PP.hsep [ PP.text (replicate (n+3) '-'), PP.pPrint (fixedParams r), PP.text ": r" <> PP.pPrint (uid r), PP.pPrint (context r)]
               , conc
               ]

instance Print [Rule] where
  pPrint sig ctx rules = 
    PP.vcat $ map (\r -> pPrint sig ctx r $$ PP.space) rules

-- RuleClass has an extra counter for rule ids.

class (Seq.Class s m, HasCounter Rule s) => Class s m | m -> s where

new :: Class s m => [RSeq] -> RSeq -> Set Param -> Ψ -> Γ -> m Rule
new ד γ ρ ψ ctx = 
  let ctx' = Ctx.restrict ctx (Var.vars (ψ, ד, γ)) in
  do sig <- Sig.get
     (ד', γ', ρ', ψ', ctx'') <- State.evalStateT (rename (ד, γ, ρ, ψ, ctx')) Rename.empty
     uid' <- Counter.tick
     return $ Class.normalize sig ctx'' $ Rule uid' ד' γ' ρ' (C.simp ψ') ctx''

-- | This should be used for testing only.  It exists only so we can parse a Rule from
-- | a String.
unsafeMake :: [RSeq] -> RSeq -> Set Param -> Ψ -> Γ -> Rule
unsafeMake = Rule Counter.dummyCtr

-- @ Application

{- 
Rule application is the process of generating new sequents and inference
rules from existing rules and sequents.  Matching a multipremise sequent
to a sequent will generate a new rules.  Matching a single premise
rule to a sequent will generate a new sequent.

Return Nothing if the consequents don't match.
Return Just (Nothing, θ) if the consequents match, and the rule consequent is not instantiated.
Return Just (Some π, θ) if the consequents match, and the rule consequent is instantiated.
-} 

unifyCons :: Class s m => Cons -> Cons -> StateT Γ m (Maybe (Maybe Atom, Θ))
unifyCons γq γr = 
  case (γq, γr) of
    (Rel πq, X) -> return $ Just (Just πq, ι)
    (X, _) -> return $ Just (Nothing, ι)
    (Rel πq, Rel πr) -> do 
      u <- Class.unify1 πq πr
      case u of 
         Nothing -> return Nothing
         Just θ -> return $ Just (Nothing, θ)

{- 
| unify1 ρ q rq unifies q and rq with all possible antecedent
mappings.  It returns the unused antecedents, the consequent PathAtom
if it was instantiated (i.e., rseq = ד ⊢ ·) and the unifying substitution.
-} 

unify1 :: Class s m => Γ -> Seq -> RSeq -> m [(Ψ, Ants, Maybe Atom, Θ, Γ)]
unify1 ctx q _r@(RSeq דr γr) = 
  let (ψ, דq, γq) = Seq.dest q 
  in do 
    (c, ctx') <- State.runStateT (unifyCons γq γr) ctx
    sig <- Sig.get
    (_θs, res) <- case c of 
      Nothing -> return ([], [])
      Just (π, θ1) -> do 
        θs <- Ants.unify ctx' (דq ✴ θ1) (דr ✴ θ1) 
        -- Remove the matched antecedents
        let f :: (Θ, Γ, Ants) -> (Ψ, Ants, Maybe Atom, Θ, Γ)
            f (θ2, rctx, unusedAnts) =
                ( ψ ✴ θ3
                , Ants.difference (Class.normalize sig ctx (unusedAnts ✴ θ3))
                                    (Class.normalize sig ctx (דr ✴ θ3))
                , π ✴ θ3
                , θ3
                , rctx
                )
              where θ3 = θ1 ○ θ2
        return $ (θs, map f θs)
    -- Log.debugM' "Rule.unify1" $
    --   PP.vcat [ PP.text "q  :" <+> pPrint sig ctx' q
    --           , PP.text "r  :" <+> pPrint sig ctx' r
    --           , PP.text "c  :" <+> pPrint sig ctx' c
    --           , PP.text "θs :" <+> pPrint sig ctx' θs
    --           , PP.text "res:" <+> pPrint sig ctx' res
    --           ]
    return res

{- 
Apply the sequent

 Γq ⊢ γq

to the rule

 Γr ⊢ γr  ...
-------------- ρ
    Γ ⊢ γ

For the application to be legal, 
  for each parameter p ∈ ρ, 
  and each substitution θ
  and each set of unused antecedents ד 
we check the following:

  1) For each variable x ∈ (Γr, γr), p ∉ x ✴ θ
  2) ρ ∉ ד
  3) If γr = Ξ and γq ≠ Ξ then p ∉ γq ✴ θ

-} 

legal :: Mode -> Γ -> Ants -> Maybe Atom -> Θ -> Set Var -> Set Param -> Bool
legal mode ctx ד π θ xs ρ = Modal.legalSubst mode ctx (CSubst.subst θ)
                         && Set.all legal1 ρ
 where 
  legal1 ç = Set.all (\x -> ç ∉ Param.params (x ✶ θ :: Term)) xs 
          && ç ∉ Param.params ד
          && ç ∉ Param.params (π ✴ θ)

apply :: Class s m => Seq -> Rule -> m (Either [Seq] [Rule])
apply q r@(Rule _ (h:hs) conc ρ ψ ctx) = 
  let -- Join the contexts
      rqctx = Ctx.join (Seq.context q) ctx
      mapFn :: Class s m => (Ψ, Ants, Maybe Atom, Θ, Γ) -> m (Maybe Rule)
      mapFn (ψq, ד, π, θ, rctx) = do
        -- sig <- Sig.get
        -- Log.debugM' "Rule.apply" $ 
        --   PP.vcat [ PP.text "ψq :" <+> pPrint sig rqctx ψq
        --           , PP.text "ד  :" <+> pPrint sig rqctx ד
        --           , PP.text "π  :" <+> pPrint sig rqctx π
        --           , PP.text "θ  :" <+> pPrint sig rqctx θ
        --           , PP.text "ωq  :" <+> pPrint sig rqctx ωq ]
        -- If the substitution is not legal, return
        mode <- Modal.get
        if not $ legal mode rctx ד π θ (Var.vars h) ρ then return Nothing else 
         Just <$> new hs' conc' ρ ψ' rctx
           where hs' = map instantiateHyp (hs ✴ θ)
                 conc' = addAnts ד $ instantiateConc (conc ✴ θ)
                 -- The number of the hole is the number of remaining
                 -- hypotheses in the rule
                 f1 = ψ
                 f2 = ψq
                 -- ψ' = C.combine (CSubst.constrs θ) (C.make (C.fill (length hs, f2) f1 ✴ θ) (ω1' ∪ ω2'))
                 ψ' = (CSubst.constrs θ) ∧ ((f1 ∧ f2) ✴ θ)
                 instantiateHyp (RSeq ד1 γ1) = RSeq ד1 γ
                   where γ = case γ1 of 
                               X -> maybeToAtom π 
                               _ -> γ1
                 instantiateConc (RSeq ד1 γ1) = RSeq ד2 γ2
                   where ד2 = Ants.union ד ד1
                         γ2 = case γ1 of 
                                X -> maybeToAtom π
                                _ -> γ1 
                 maybeToAtom Nothing = X
                 maybeToAtom (Just p) = Rel p
      toSeq :: Class s m => Rule -> m Seq 
      toSeq (Rule _ [] (RSeq ד γ) _ ψ' ctx') = Seq.new ctx' ψ' ד γ
      toSeq _ = error "apply: Impossible" 
  -- now determine if we generate new sequents or new rules
  in do newRules <- unify1 rqctx q h
        rules <- M.mapMaybeM mapFn newRules
        res <- case hs of 
          [] -> mapM toSeq rules >>= return . Left
          _  -> return $ Right rules
        sig <- Sig.get
        Log.debugM' "Rule.apply" $ 
          PP.vcat [ PP.text "q        :" <+> pPrint sig Ctx.empty q
                  , PP.text "r        :" <+> pPrint sig Ctx.empty r
                  --, PP.text "newRules :" <+> pPrint sig Ctx.empty newRules
                  , PP.text "res      :" <+> pPrint sig Ctx.empty res 
                  ]
        return res
apply _ (Rule _ [] _ _ _ _) = error "apply: Impossible 1" 

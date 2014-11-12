
-- | Linear Worlds.

-- @ Pragmas

{-# LANGUAGE CPP, Rank2Types, FlexibleContexts, MultiParamTypeClasses, FlexibleInstances #-} 

-- @ Signature 

module Imogen.Linear.World 
  ( -- * Worlds
    World
  , Tag(..)
    -- * Operations
  , ε
  , (⋆)
  , null
  , var
  , param
  , concat
    -- * Unification
  , Eqn
  , unify
  , solver
  )
where

-- @ Imports

#include "../../undefined.h" 

import Imogen.Util.Prelude hiding (concat, const, id, null)
import qualified Control.Monad.State as State
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Imogen.Atom(Atom(Rel))
import qualified Imogen.Constr as Constr
import Imogen.Constr (Formula, Solver(..), (⊤))
import qualified Imogen.CSubst as CSubst
import qualified Imogen.Ctx as Ctx
import Imogen.Ctx (Γ)
import qualified Imogen.Func as Func
import Imogen.Func (Func)
import qualified Imogen.Param as Param
import Imogen.Param (Param, Params, Freeze(..))
import qualified Imogen.Pred as Pred
import qualified Imogen.Rename as Rename
import Imogen.Rename (Rename(..))
import Imogen.Sig (Σ)
import qualified Imogen.Sort as Sort
import qualified Imogen.Subst as Subst
import Imogen.Subst (Θ, Apply, GenApply, (↦), (✶), (✴), (○), ι)
import qualified Imogen.Term as Term
import Imogen.Term (Term(..), Encode(..))
import qualified Imogen.Unif as Unif
import qualified Imogen.Util.Fresh as Fresh
import Imogen.Util.Fresh (HasFresh)
import qualified Imogen.Util.List as List
import qualified Imogen.Util.Log as Log
import Imogen.Util.Log (Log)
import qualified Imogen.Util.Monad as M
import qualified Imogen.Util.Pair as Pair
import qualified Imogen.Util.Print as PP
import Imogen.Util.Print (Print, pPrint)
import qualified Imogen.Var as Var
import Imogen.Var (Var, Vars)

-- @ Constants

{- | 
A constant is a parameter or a function symbol.  There are no
functions with arguments in linear worlds
-} 
data Const = P Param
           | C Func
  deriving (Ord, Eq, Show)

instance Print Const where
  pPrint (C f) = pPrint f
  pPrint (P c) = pPrint c

-- @ Worlds

-- | Linear worlds.

{- 
Since (⋆) is AC1, we can store a world as a sorted list of variables
and constants.  The empty list will stand for ε.  A representation
invariant is that the list is sorted wrt the Ord class.  When
combining worlds with (⋆), we need to resort the list.
-} 
data World = W [Var] [Const]
  deriving (Ord, Eq, Show)

-- @@ Instances

instance Rename World where
  rename (W xs as) = rename xs >>= return . flip W as

instance Vars World where
  vars (W xs _) = Set.fromList xs
  free = Var.vars

instance Params World where
  params (W _ xs) = Set.fromList $ Maybe.mapMaybe getParam xs
   where getParam (P c) = Just c
         getParam _ = Nothing

instance Print World where
  pPrint (W [] []) = PP.text "ε"
  pPrint (W αs çs) = 
    PP.fsep $ List.intersperse (PP.text "⋆") (map pPrint αs ++ map pPrint çs)

-- @ Sort tag

-- | A sort tag.
data Tag = Tag

instance Sort.Tag Tag where

instance Sort.TagOf Tag World where

instance Sort.OK World Tag where
  ok _ (W xs cs) = List.nub xs == xs && List.nub cs == cs

instance Sort.OK Term Tag where
  ok tag t = Sort.ok tag (decode t :: World)

-- @ Operations

-- | The initial world.
ε :: World
ε = W [] []

-- | w == ε ?
null :: World -> Bool
null = (==) ε

-- | Combination of worlds.  This could be more efficient...
(⋆) :: World -> World -> World
(W α1 ç1) ⋆ (W α2 ç2) = W (List.sort $ α1 ++ α2) (List.sort $ ç1 ++ ç2)


-- | Concatenate worlds.  Note that we don't have to sort at every
--   step.  Just combine them and sort at the end.
concat :: [World] -> World
concat ps = W (comb αs) (comb çs)
  where (αs, çs) = unzip $ map dest ps
        comb :: Ord a => [[a]] -> [a]
        comb = List.sort . List.concat
        dest (W αs' çs') = (αs', çs')

-- | Make a variable.
var :: Var -> World
var α = W [α] []

-- | Make a parameter.
param :: Param -> World
param ç = W [] [P ç]

-- @ Encoding

{- 
Encode and decode worlds as terms.  The term normal form for worlds is

> Fn "⋆" [Var "α1", ..., Var "αn", Param "ç1", ..., Param "çn"]

ε is represented by 

> Fn "⋆" [] 
-} 
instance Encode World where
  encode (W [] []) = Fn (Func.make "ε") []
  encode (W [α] []) = Var α 
  encode (W [] [c]) = constTerm c
  encode (W αs çs) = Fn (Func.make "⋆") $ map Var αs ++ map constTerm çs
  decode = flatten . fromTerm

constTerm :: Const -> Term
constTerm (P ç) = Param ç
constTerm (C f) = Fn f []

instance Sort.Normalize Term Tag where
  normalize _ t = encode (decode t :: World)

data WTree = Epsilon
           | Star WTree WTree
           | WVar Var
           | WConst Const

flatten :: WTree -> World
flatten t = case t of 
  Epsilon -> W [] []
  WVar x -> W [x] []
  WConst x -> W [] [x]
  Star t1 t2 -> 
    let W xs1 cs1 = flatten t1
        W xs2 cs2 = flatten t2
    in W (List.sort $ xs1 ++ xs2) (List.sort $ cs1 ++ cs2)

fromTerm :: Term -> WTree
fromTerm t = case t of
  Var x -> WVar x
  Param c -> WConst (P c)
  Fn f [] 
    | Func.name f == "ε" -> Epsilon
    | otherwise -> WConst (C f)
  Fn f args -> case (Func.name f, args) of 
    ("⋆", t':ts) -> foldr Star (fromTerm t') (map fromTerm ts)
    ("⋆", []) -> Epsilon
    _ -> error $ "Can't convert to world: " ++ show t

instance GenApply Var World where
  α ✶ θ = case Subst.lookup α θ of
    Nothing -> W [α] []
    Just p -> decode p

instance GenApply [Var] World where
  αs ✶ θ = concat $ map (✶ θ) αs

instance Apply World where
  (W αs çs) ✴ θ = W αs' (çs' ++ çs) 
    where W αs' çs' = αs ✶ θ

-- @ Unification 

{- 

Equational theory

 ε ⋆ p ≡ p
 p ⋆ q ≡ q ⋆ p
 p ⋆ (q ⋆ r) ≡ (p ⋆ q) ⋆ r
 (f ⊛ p) ◃ q ≡ f ◃ (q ⋆ p)

Note that the first 3 equations are equality on worlds, while
the last is equality on negative atoms.  Note that given the
last equality, any ground negative atom f ◃ p can be written uniquely
as 

φ ◃ p ⋆ q 

where φ is a frame variable or constant (a Head) and f = φ ⊛ q.  
This simplifies the unification problem
considerably.  A set of equations 

{ f1 ◃ p1 ≡ f1' ◃ p1', ⋯, fn ◃ pn ≡ fn' ◃ pn' }

is transformed to 

{ φ1 ◃ p1 ⋆ q1 ≡ φ1' ◃ p1' ⋆ q1', ⋯, φn ◃ pn ⋆ qn ≡ φn' ◃ pn' ⋆ qn'}

where we can essentially solve two distinct unification
problems, the first of the φi that involves only constant and
variable matching.  This unification problem is unitary.  
Then we solve the AC1 world unification problem of the pi, qi.
Since pi,qi can have constants (the parameters are effectively
constants in this setting), we no longer have unique mgus.  For
example, ç ≡ α1 ⋆ α2 has 2 solutions, { α1 ↦ ç, α2 ↦ ε }
and { α1 ↦ ε, α2 ↦ ç }.  A full solution involves translating
the problem into nonhomogenous diophantine equations and solving
those.  The problem is NP-complete, and a bit of a mess.  

Instead, we will try to find an i such that the problem
pi ⋆ qi ≡ pi' ⋆ qi' is unitary.  Then we can apply the
substitution to the rest of the equations and hope that
the substitution generates more unitary equations.  If
not, we will maintain the non-unitary equations as constraints.

● Frame variable/parameter unification

Goal: to solve a set of equations where each side is
either a frame variable or a frame parameter.

Case: If one of the sides is a variable, return the substitution.

 φ ≡ f ↪ { φ ↦ f }

Case: If both sides are parameters, return the parameter substitution.

 φ* ≡ φ'* ↪ { φ* ↦ φ'* }

It is a simple problem.  Every problem has a most general solution,
computable in linear time.  This will be dealt with in the Frame
module.

● World unification

Goal: to solve a set of equations Δ where each side is an 
arbitrary world expression.

 0) remove all ε from Δ 
    p ⋆ ε ↦ p

 1) if α ≡ p ∈ Δ or p ≡ α ∈ Δ 
    Δ ↦ {α ↦ p} (Δ \ {α ≡ p})
    Add {α ↦ p} to θ 

 2) If p ≡ ε ∈ Δ or ε ≡ p ∈ Δ and ç ∈ p, fail.

 3) If p ≡ ε ∈ Δ or ε ≡ p ∈ Δ and p = α1 ⋆ ⋯ ⋆ αn
    Δ ↦ {α1 ↦ ε, …, αn ↦ ε} (Δ \ {p ≡ ε})
    Add {α ↦ ε, …, αn ↦ ε} to θ 

 4) If an equation ρ has the form ç ⋆ p1 ≡ ç ⋆ p2, or 
    α ⋆ p1 ≡ α ⋆ p2 then replace ρ with p1 ≡ p2

 5) If an equation ρ has the form ç1 ⋆ ⋯ ⋆ çn ≡ ç ⋆ p, (and by rule 5 none
    of the çi appear in p) then fail.

 6) If an equation ρ has the form α ⋆ çs ≡ αs ⋆ çs', where there are no
    variables in çs and |çs| > 0, then {α ↦ α' ⋆ çs'} where α' is a new variable.
    (Recall that by now çs ∩ çs' = ∅)

 7) Otherwise, either ρ has the form
    ç1 ⋆ ⋯ ⋆ çn ≡ αs
    where αs consists of at least two variables
    or both sides of ρ has one of the following forms
    α1 ⋆ α2 ⋆ w, α ⋆ ç ⋆ w
    In this case, we return ρ as a constraint.

Since we maintain a sorted list form, no ε appears, so step 0 is not
necessary.  For the rest of the steps, we maintain a substitution 
in the State monad that will be returned at the end of the unification.
We need fresh world variable generaton too, so we use a state 
transformer.
-} 

-- | A convenient abbreviation for a world equation.
type Eqn = (World, World)

data Res = Changed (Θ, [Eqn])
         | Unchanged
         | Fail

type Phase = (MonadState s m, HasFresh Var s) => [Eqn] -> StateT Γ m Res

{- 
 1) if α ≡ p ∈ Δ or p ≡ α ∈ Δ 
    Δ ↦ (Δ \ {α ≡ p}) {α ↦ p} 
    θ ↦ θ ○ {α ↦ p} 
-} 

phase1 :: Phase
phase1 eqs = 
  case List.findRemFirst uni eqs of 
    Nothing -> return Unchanged
    Just ((α, p), eqs') -> 
      let θ :: Θ 
          θ = α ↦ encode p in 
      return $ Changed $ (θ, eqs' ✴ θ)
  where uni :: Eqn -> Maybe (Var, World)
        uni (W [α] [], p) = Just (α, p)
        uni (p, (W [α] [])) = Just (α, p)
        uni _ = Nothing

{- 
 2) If p ≡ ε ∈ Δ or ε ≡ p ∈ Δ and ç ∈ p, fail.
-} 

phase2 :: Phase
phase2 eqs = return $ if any uni eqs then Fail else Unchanged
 where uni :: Eqn -> Bool
       uni (W [] [], W _ çs) | length çs > 0 = True
       uni (W _ çs, W [] []) | length çs > 0 = True
       uni _ = False

{- 
 3) If p ≡ ε ∈ Δ or ε ≡ p ∈ Δ and p = α1 ⋆ ⋯ ⋆ αn
    Δ ↦ {α1 ↦ ε, …, αn ↦ ε} (Δ \ {p ≡ ε})
    Add {α ↦ ε, …, αn ↦ ε} to θ 
-} 

phase3 :: Phase
phase3 eqs = 
  case List.findRemFirst uni eqs of
    Nothing -> return Unchanged
    Just (αs, eqs') ->
      let θ = Subst.fromList $ map (\x -> (x, encode ε)) αs in
      do return $ Changed $ (θ, eqs' ✴ θ)
  where uni :: Eqn -> Maybe [Var]
        uni (W [] [], W αs []) | length αs > 0 = Just αs
        uni (W αs [], W [] []) | length αs > 0 = Just αs
        uni _ = Nothing

{- 
 4) If an equation ρ has the form ç ⋆ p1 ≡ ç ⋆ p2 or α ⋆ p1 ≡ α ⋆ p2, 
    then replace ρ with p1 ≡ p2.  If this reduces the formula to ε ≡ ε, 
    delete it.
-} 

phase4 :: Phase
phase4 eqs = if eqs == eqs' then return Unchanged 
                 else return $ Changed $ (ι, eqs')
 where eqs' = filter (not . trivial) $ map simpl eqs
       simpl :: Eqn -> Eqn
       simpl (W α1s ç1s, W α2s ç2s) = (W α1s' ç1s', W α2s' ç2s')
         where (ç1s', ç2s') = diff ç1s ç2s
               (α1s', α2s') = diff α1s α2s
               diff [] ys = ([], ys)
               diff xs [] = (xs, [])
               diff (x:xs) (y:ys) 
                 | x == y = diff xs ys
                 | otherwise = if x < y then first (x:) $ diff xs (y:ys)
                               else second (y:) $ diff (x:xs) ys
       trivial (W [] [], W [] []) = True
       trivial _ = False

{- 
 5) If an equation ρ has the form ç1 ⋆ ⋯ ⋆ çn ≡ ç ⋆ p, (and by rule 5 none
    of the çi appear in p) then fail.
-} 

phase5 :: Phase
phase5 eqs = return $ if any uni eqs then Fail else Unchanged
 where uni (W [] _çs1, W _ çs2) | length çs2 > 0 = True
       -- in the above case we know that çs1 /= ε, which would be remove 
       -- in phase 2 or 3
       uni (W _ çs1, W [] _çs2) | length çs1 > 0 = True
       uni _ = False

{- 
 6) If an equation ρ has the form α ⋆ çs ≡ αs ⋆ çs', where there are no
    variables in çs, then {α ↦ α' ⋆ çs'} where α' is a new variable, and
    replace ρ by α' * çs ≡ αs.  
-} 

phase6 :: Phase
phase6 eqs = 
  case List.findRemFirst uni eqs of
    Nothing -> return Unchanged
    Just ((W [α] çs, W αs çs'), eqs') ->
      do α' <- M.lift $ Fresh.fresh "α"
         State.modify (Ctx.insert α' Sort.LWorld)
         let θ = α ↦ encode (W [α'] çs')
             eqn = (W [α'] çs, W αs [])
         return $ Changed $ (θ, eqn : eqs' ✴ θ)
    _ -> __IMPOSSIBLE__
  where uni :: Eqn -> Maybe Eqn
        uni eq@(W [_α] _, W _ (_ : _)) = Just eq
        uni eq@(W _ (_ : _), W [_α] _) = Just $ Pair.flip eq
        uni _ = Nothing

{- |
We export 'unify' so it can be reused by "Imogen.Linear.Frame" without the need
to encode and decode the constraint equations.
-} 

-- Cycle through the phases in order until the equations reach a fixed point.
unify :: forall s m. (MonadState s m, HasFresh Var s, Log m) => [Eqn] -> StateT Γ m (Maybe (Θ, [Eqn]))
unify eqns = do 
  res <- run phases (ι, eqns)
  Log.debugM' "World.unify" $ PP.vcat [ PP.text "Eqns:" <+> pPrint eqns
                                      , PP.text "Res :" <+> pPrint res ]
  return res
  where phases :: [([Eqn] -> StateT Γ m Res)] -- MR
        phases = [ phase1, phase2, phase3, phase4, phase5, phase6 ]
        run [] res = return $ Just res
        run (p:ps) (θ, eqs) = do 
          res <- p eqs
          case res of 
            Fail -> return Nothing
            Unchanged -> run ps (θ, eqs)
            Changed (θ', eqs') -> run phases (θ ○ θ', eqs')

instance Freeze World where
  freeze (W αs çs) = W [] (map (P . Var.freeze) αs ++ çs)
  thaw xs (W [] çs) = W αs' çs'
    where (αs, çs') = List.partition part çs
          part (P ç) = Set.member ç xs
          part _ = False
          αs' = map thawf αs
          thawf (P ç) = Var.thaw ç
          thawf _ = __IMPOSSIBLE__ 
  thaw _ _ = __IMPOSSIBLE__ 

instance Unif.Unif World Tag where
  unify _ = fmap (fmap f) . unify
    where f (θ, eqs) = CSubst.make θ (Constr.listAnd $ map mkeq eqs)
          mkeq (w1, w2) = Constr.Atom $ Rel (Pred.make "=") [encode w1, encode w2]
  match tag eqs = fmap (thaw frozen) $ Unif.unify tag (map (second freeze) eqs)
    where frozen = Set.map Var.freeze (Var.vars eqs)

-- @ Solver

-- | The linear world constraint solver.
solver :: Solver
solver = Solver "Linear" simplify valid 

valid :: (MonadState s m, HasFresh Var s, Log m) => Σ -> Formula -> StateT Γ m Bool
valid _ ψ = return $ Constr.simp ψ == (⊤)

simplify :: (MonadState s m, HasFresh Var s, Log m) => Σ -> Formula -> StateT Γ m (Maybe (Θ, Formula))
simplify _ ψ = do 
  --Log.debugM' "World.simplify" $ pPrint ψ
  res <- unify $ fromConstr ψ
  case res of 
    Nothing -> return Nothing
    Just (θ, eqs) -> return $ Just (θ, toConstr eqs)

toConstr :: [Eqn] -> Formula
toConstr = Constr.listAnd . map mkEq
  where mkEq (s, t) = Constr.Atom $ Rel (Pred.make "≡") [encode s, encode t]

fromConstr :: Formula -> [Eqn]
fromConstr f = Maybe.mapMaybe getEq (Constr.destAnd f)
 where 
  getEq (Constr.Atom (Rel p [s, t])) 
   | Pred.name p == "=" = Just (decode s, decode t)
   | otherwise = error $ "bad pred: " ++ show p
  getEq Constr.Top = Nothing
  getEq t = error $ "fromConstr: " ++ show t

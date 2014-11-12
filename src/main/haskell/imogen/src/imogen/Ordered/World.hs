
-- | Ordered Worlds 

-- @ Pragmas

{-# LANGUAGE CPP, Rank2Types, FlexibleContexts, MultiParamTypeClasses #-} 

-- @ Signature 

module Imogen.Ordered.World
  ( -- * Worlds
    World
    -- * Tag
  , Tag(..)
    -- * Operators
  , ε
  , (·)
  , ι
  , null
  , var 
  , param
  , const
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
import qualified Data.Foldable as Fold
import qualified Data.Maybe as Maybe
import Imogen.Atom (Atom(Rel))
import qualified Imogen.Constr as Constr
import Imogen.Constr (Formula, Solver(..), (⊤))
import qualified Imogen.CSubst as CSubst
import qualified Imogen.Ctx as Ctx
import Imogen.Ctx (Γ)
import qualified Imogen.Func as Func
import Imogen.Func (Func)
import qualified Imogen.Linear.World as W
import Imogen.Linear.World ((⋆))
import qualified Imogen.Param as Param
import Imogen.Param (Param, Params, Freeze(..))
import qualified Imogen.Pred as Pred
import qualified Imogen.Rename as Rename
import Imogen.Rename (Rename(..))
import Imogen.Sig (Σ)
import qualified Imogen.Sort as Sort
import qualified Imogen.Subst as Subst
import Imogen.Subst (Θ, Apply, GenApply, (↦), (✶), (✴), (○))
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
import qualified Imogen.Util.Seq as Seq
import Imogen.Util.Seq (Seq, (><), ViewL(..), ViewR(..), ViewL2(..), ViewR2(..), (<|), (|>))
import qualified Imogen.Util.Set as Set
import Imogen.Util.Set ((∅), (∈))
import qualified Imogen.Var as Var
import Imogen.Var (Var, Vars)


-- @ Constants

data Const = P Param
           | F Func
  deriving (Ord, Eq, Show)

-- @@ Instances 

instance Encode Const where
  encode (P x) = Param x
  encode (F f) = Fn f []
  decode (Param c) = P c
  decode (Fn f []) = F f
  decode _ = error "Can't decode"

instance Print Const where
  pPrint (P x) = pPrint x
  pPrint (F x) = pPrint x

-- @ Entries

data Entry = V Var
           | C Const
           | I W.World
  deriving (Ord, Eq, Show)

-- @@ Instances

instance Rename Entry where
  rename e = case e of
    V x -> rename x >>= return . V
    I x -> rename x >>= return . I
    _ -> return e

instance Vars Entry where
  vars e = case e of
    V x -> Set.singleton x
    I x -> Var.vars x
    _ -> (∅)
  free = Var.vars

instance Params Entry where
  params e = case e of
    C (P c) -> Set.singleton c
    I x -> Param.params x
    _ -> (∅)

instance Freeze Entry where
  freeze e = case e of
    V x -> C $ P $ Var.freeze x
    _ -> e
  thaw ρ e = case e of
    C (P c) -> if c ∈ ρ then V $ Var.thaw c else e
    _ -> e

instance GenApply Var Entry where
  α ✶ θ = case Subst.lookup α θ of
    Nothing -> V α
    Just p -> decode p

instance Apply Entry where
  V x ✴ θ = x ✶ θ 
  I w ✴ θ = I $ w ✴ θ 
  c@(C _) ✴ _ = c

instance Print Entry where
  pPrint e = case e of 
    V x -> pPrint x
    C x -> pPrint x
    I x -> pPrint x

-- @ Worlds

data World = W { entries :: Seq Entry }
  deriving (Ord, Eq, Show)

-- @@ Instances

instance Rename World where
  rename (W ws) = rename ws >>= return . W 

instance Vars World where
  vars (W xs) = Var.vars xs
  free = Var.vars

instance Params World where
  params (W xs) = Param.params xs

instance Print World where
  pPrint (W e) = case Fold.toList e of
    [] -> PP.text "ع"
    xs -> PP.fsep $ List.intersperse (PP.text "·") (map pPrint xs)

-- @ Operations

ε :: World
ε = W Seq.empty

(·) :: World -> World -> World
W w1 · W w2 = W $ w1 >< w2

ι :: W.World -> World
ι = W . Seq.singleton . I

null :: World -> Bool
null = (==) ε

sing :: Entry -> World
sing = W . Seq.singleton

var :: Var -> World
var = sing . V

param :: Param -> World
param = sing . C . P

const :: Func -> World
const = sing . C . F

fromList :: [Entry] -> World
fromList = W . Seq.fromList

constants :: World -> Set Const
constants (W es) = consts es
  where consts e = case Seq.viewl e of 
          EmptyL -> Set.empty
          C c :< es' -> Set.insert c (consts es')
          _ :< es' -> consts es'

linearWorlds :: World -> Set W.World
linearWorlds (W es) = ws es
  where ws e = case Seq.viewl e of
          EmptyL -> Set.empty
          I w :< es' -> Set.insert w (ws es')
          _ :< es' -> ws es'

-- @ World Tag

data Tag = Tag

-- @@ Instances

instance Sort.Tag Tag where

instance Sort.TagOf Tag World where

instance Sort.OK World Tag where
  ok _ (W es) = List.nub es' == es'
    where es' = Fold.toList es

instance Sort.OK Term Tag where
  ok tag t = Sort.ok tag (decode t :: World)

-- @ Normalization

-- Remove adjacent ι terms.

normalize :: World -> World
normalize = W . norm . entries
 where 
  norm es = case Seq.viewl2 es of
    Seq.EmptyL2 -> es
    Seq.SingL2 _ -> es
    (I w1, I w2) ::< w -> norm $ I (w1 ⋆ w2) <| w
    (e1, e2) ::< es' -> e1 <| (norm $ e2 <| es')

-- Encode and decode worlds as terms.

instance Encode Entry where
  encode (V x) = Var x
  encode (C x) = encode x
  encode (I x) = Fn (Func.make "ι") [encode x]
  decode (Var x) = V x
  decode (Param x) = C (P x)
  decode (Fn f []) = C (F f)
  decode (Fn f [x]) 
   | f == Func.make "ι" = I $ decode x
   | otherwise = error "Can't decode"
  decode _ = error "Can't decode"

instance Encode World where
  encode (W es) = case Fold.toList es of
    [] -> Fn (Func.make "ع") []
    [e] -> encode e
    es' -> Fn (Func.make "·") (map encode es')
  decode = flatten . fromTerm

instance Sort.Normalize Term Tag where
  normalize _ t = encode (decode t :: World)

data WTree = Epsilon
           | Dot WTree WTree
           | WVar Var
           | WConst Const
           | WWorld W.World

flatten :: WTree -> World
flatten t = case t of 
  Epsilon -> W Seq.empty
  WVar x -> var x
  WConst x -> sing $ C x
  WWorld w -> sing $ I w
  Dot t1 t2 -> 
    let W es1 = flatten t1
        W es2 = flatten t2
    in W $ es1 >< es2

fromTerm :: Term -> WTree
fromTerm t = case t of
  Var x -> WVar x
  Param c -> WConst (P c)
  Fn f [] 
    | Func.name f == "ع" -> Epsilon
    | otherwise -> WConst $ F f
  Fn f args -> case (Func.name f, args) of 
    ("·", t':ts) -> List.foldl' Dot (fromTerm t') (map fromTerm ts)
    ("·", []) -> Epsilon
    ("ι", [w]) -> WWorld $ decode w
    _ -> error $ "Can't convert to ordered world: " ++ show t

-- @ Substitutions

instance Apply World where
  W es ✴ θ = normalize $ W $ es ✴ θ

-- @ Unification 

{- 

Equational theory

 ε · p ≡ p
 p · (q · r) ≡ (p · q) · r
 f ⓡ p ◃ q ≡ f ◃ q · p
 p ⓛ f ◃ q ≡ f ◃ p · q
 ι W.ε ≡ ε 
 ι (p ⋆ q) ≡ ι p · ι q

Note that in the last rule p and q ∈ W.World.  Similar to linear
logic, we can write any given frame as w1 ⓛ f ⓡ w2 where f is a head.
Then we need only solve the world unification problem.  We take a
solver for W.Worlds as given, though of course this procedure also
has unsolvable parts which get added to the constraints.

legend:  α : variable
         c : constant (or parameter) (not an ι)
         ĉ : ι-constant (constant or ι)
         w : W.World

All rules are written for the left side.  They work equally
well on the right side, with the order of arguments reversed.

** Combine ι-s

w1 · ι p · ι q · w2 ---> w1 · ι (p ⋆ q) · w2 

** Epsilon

ε ≡ ε ---> ⊤
ε ≡ α · p ---> α ↦ ε, ε ≡ p 
ε ≡ c · p ---> fail
ε ≡ ι w · p ---> w ≡ ε, ε ≡ p

** Singleton 

α ≡ w ---> α ↦ w
c ≡ α1 ⋯ c ⋯ αn ---> α1 ⋯ αn ≡ ε
c ≡ α1 ⋯ c' ⋯ αn ---> fail
c ≡ α1 ⋯ ι α ⋯ αn ---> fail
c ≡ α1 ⋯ c1 ⋯ c2 ⋯ αn ---> fail
c ≡ c · p ---> p ≡ ε
c ≡ c' · p ---> fail
c ≡ ι w · p ---> fail
ι w ≡ p1 · c · p2 ---> fail
ι w ≡ α · ι w' · p ---> α ↦ ι α', ι w ≡ ι (α' ⋆ w') · p
ι w ≡ α1 · ι w1 · α2 · ι w2 · ⋯ · αN ---> αi ↦ ι αi', w ≡ α1' ⋆ ⋯ ⋆ αN' ⋆ w1 ⋆ ⋯ ⋆ wN

** General

At this point we have at least 2 elements on each side of the
equations.  We consider the first symbols of the equality.

Constant-Constant

c · p ≡ c · q ---> p ≡ q
c · p ≡ c' · q ---> fail

Constant-W.World

c · p ≡ ι p · q ---> fail 

Constant-Variable

c · p ≡ α · ĉ · q ---> α ↦ c · α', p ≡ α' · q
c · p ≡ α · α' · q (Not unitary)

Now the first symbol of either side of the equation is a variable or W.World
The possible forms of one side are

ι w · α · p ≡ 
ι w · c · p ≡ 
α · ι w · c · p ≡ 
α1 · ι w · α2 · p ≡ 
α1 · α2 · p ≡ 

Thus there are 15 possibilities (up to symmetry), only one of which is
unitarily reducible.

 1) ι w1 · α1 · p ≡ ι w2 · α2 · q  (No solution)

 2) ι w1 · α · p ≡ ι w2 · c · q (Not unitary)
    In this case α ≡ ε or α ≡ c · α' or α ≡ ι α3 · α'    

 3-5) ι w1 · α1 · p ≡ α2 · ι w2 · c · q (Hopeless)

 6) ι w1 · c1 · p ≡ ι w2 · c2 · q (Reducible!)
    ι w1 · c1 · p ≡ ι w2 · c2 · q ---> w1 ≡ w2, c1 · p ≡ c2 · q

 7-9) ι w1 · c1 · p ≡ ι w2 · c2 · q (Hopeless)

 10-15) (Hopeless)
-} 

type Eqn = (World, World)
type WEqn = (W.World, W.World)

data Res = Changed (Θ, [Eqn], [WEqn])
         | Unchanged
         | Fail

type Phase = (MonadState s m, HasFresh Var s) => [Eqn] -> StateT Γ m Res

{- 
0) w1 · ι p · ι q · w2 ---> w1 · ι (p ⋆ q) · w2 
-} 

normEqns :: [Eqn] -> [Eqn]
normEqns = map (both normalize)

{- 
1) 

ε ≡ ε ---> ⊤
ε ≡ α · p ---> α ↦ ε, ε ≡ p 
ε ≡ c · p ---> fail
ε ≡ ι w · p ---> w ≡ ε, ε ≡ p
-} 

phase1 :: Phase
phase1 eqs = case List.findRemFirst epsfn eqs of
  Nothing -> return Unchanged
  Just ((_, r), eqs') -> return $ case Seq.viewl $ entries r of 
    EmptyL -> Changed (Subst.ι, eqs', [])
    V α :< p -> Changed (α ↦ encode ε, (ε, W p) : eqs', [])
    C _c :< _p -> Fail
    I w :< p -> Changed (Subst.ι, (ε, W p) : eqs', [(w, W.ε)])
 where
  epsfn (l, r) = if l == ε then Just (l, r)
                 else if r == ε then Just (r, l)
                 else Nothing

{- 
2)

α ≡ w ---> α ↦ w
c ≡ α1 ⋯ c ⋯ αn ---> α1 ⋯ αn ≡ ε
c ≡ α1 ⋯ c' ⋯ αn ---> fail
c ≡ α1 ⋯ ι _ ⋯ αn ---> fail
ι w ≡ p1 · c · p2 ---> fail
ι w ≡ α · ι w' · p ---> α ↦ ι α', ι w ≡ ι (α' ⋆ w') · p
ι w ≡ α1 · ι w1 · α2 · ι w2 · ⋯ · αN ---> αi ↦ ι αi', w ≡ α1' ⋆ ⋯ ⋆ αN' ⋆ w1 ⋆ ⋯ ⋆ wN

-} 

phase2 :: Phase
phase2 eqs = case List.findRemFirst singfn eqs of
  Nothing -> return Unchanged
  Just ((e, r), eqs') -> case e of 
    V α -> return $ Changed (α ↦ encode (W r), eqs', [])
    C _ -> case List.findRemFirst (\e' -> if e == e' then Just e' else Nothing) (Fold.toList r) of
      Just (_, r') -> return $ Changed $ (Subst.ι, (W $ Seq.fromList r', ε) : eqs', [])
      Nothing -> return $ 
        if Set.null (constants (W r)) && Set.null (linearWorlds (W r))
        then Unchanged 
        else Fail
    I w 
     | hasConst r -> return Fail
     | otherwise -> 
      let (αs, ιs) = split r in do
      αs' <- M.lift $ mapM Fresh.fresh (replicate (length αs) "α")
      State.modify (\ctx -> foldr (flip Ctx.insert Sort.OWorld) ctx αs')
      return $ Changed (Subst.fromList $ zip αs' (map Var αs), eqs', [(w, W.concat $ map W.var αs' ++ ιs)])
 where 
  singfn (W l, W r) = case (Seq.viewl2 l, Seq.viewl2 r) of 
    (SingL2 l', _) -> Just $ (l', r)
    (_, SingL2 r') -> Just $ (r', l)
    _ -> Nothing
  hasConst :: Seq Entry -> Bool
  hasConst = Fold.any isConst
  isConst (C _) = True
  isConst _ = False
  split es = case Seq.viewl es of 
    EmptyL -> ([], [])
    V x :< rest -> (x : xs, ιs)
      where (xs, ιs) = split rest
    I i :< rest -> (xs, i : ιs)
      where (xs, ιs) = split rest
    _ -> __IMPOSSIBLE__ 

{- 
3) 

c · p ≡ c · q ---> p ≡ q
c · p ≡ c' · q ---> fail
c · p ≡ ι w · q ---> fail 
c · p ≡ α · ĉ · q ---> α ↦ c · α', p ≡ α' · q
c · p ≡ α · α' · q (Not unitary)

Remember to reduce on both sides
-} 

phase3a :: Phase
phase3a eqs = case List.findRemFirst cfn eqs of
  Nothing -> return Unchanged
  Just ((W l, W r), eqs') -> case (Seq.viewl l, Seq.viewl r) of
    (C c :< p, C c' :< q) 
     | c == c' -> return $ Changed (Subst.ι, (W p, W q) : eqs', [])
     | otherwise -> return Fail
    (C c :< p, V α :< q) -> case Seq.viewl q of
      EmptyL -> __IMPOSSIBLE__ 
      V _ :< _ -> return Unchanged
      _ -> do
        α' <- M.lift $ Fresh.fresh "α"
        State.modify (Ctx.insert α' Sort.OWorld)
        return $ Changed (α ↦ (encode $ fromList [C c, V α']), (W p, var α' · W q) : eqs', [])
    (C _ :< _, I _ :< _) -> return Fail
    _ -> __IMPOSSIBLE__ 
 where
  cfn eq@(W l, W r) = case (Seq.viewl l, Seq.viewl r) of
    (C _ :< _, _) -> Just eq
    (_, C _ :< _) -> Just $ Pair.flip eq
    _ -> Nothing

phase3b :: Phase
phase3b eqs = case List.findRemFirst cfn eqs of
  Nothing -> return Unchanged
  Just ((W l, W r), eqs') -> case (Seq.viewr l, Seq.viewr r) of
    (p :> C c, q :> C c') 
     | c == c' -> return $ Changed (Subst.ι, (W p, W q) : eqs', [])
     | otherwise -> return Fail
    (p :> C c, q :> V α) -> case Seq.viewr q of
      EmptyR -> __IMPOSSIBLE__ 
      _ :> V _ -> return Unchanged
      _ -> do 
        α' <- M.lift $ Fresh.fresh "α"
        State.modify (Ctx.insert α' Sort.OWorld)
        return $ Changed (α ↦ (encode $ fromList [V α', C c]), (W p, W q · var α') : eqs', [])
    (_ :> C _, _ :> I _) -> return Fail
    _ -> __IMPOSSIBLE__ 
 where
  cfn eq@(W l, W r) = case (Seq.viewr l, Seq.viewr r) of
    (_ :> C _, _) -> Just eq
    (_, _ :> C _) -> Just $ Pair.flip eq
    _ -> Nothing

{- 
4) 

ι w1 · c1 · p ≡ ι w2 · c2 · q ---> w1 ≡ w2, c1 · p ≡ c2 · q
-} 

phase4a :: Phase
phase4a eqs = case List.findRemFirst cfn eqs of
  Nothing -> return Unchanged
  Just (((w1, c1, p), (w2, c2, q)), eqs') -> 
    return $ Changed (Subst.ι, (W $ C c1 <| p, W $ C c2 <| q) : eqs', [(w1, w2)])
 where
  cfn (W l, W r) = case (Seq.viewl2 l, Seq.viewl2 r) of
    ((I w1, C c1) ::< p, (I w2, C c2) ::< q) -> Just ((w1, c1, p), (w2, c2, q))
    _ -> Nothing

phase4b :: Phase
phase4b eqs = case List.findRemFirst cfn eqs of
  Nothing -> return Unchanged
  Just (((p, c1, w1), (q, c2, w2)), eqs') ->
    return $ Changed (Subst.ι, (W $ p |> C c1, W $ q |> C c2) : eqs', [(w1, w2)])
 where
  cfn (W l, W r) = case (Seq.viewr2 l, Seq.viewr2 r) of
    (p ::> (C c1, I w1), q ::> (C c2, I w2)) -> Just ((p, c1, w1), (q, c2, w2))
    _ -> Nothing

{- 
Cycle through the phases in order until the equations reach a fixed point.
-} 

unify :: forall s m. (MonadState s m, HasFresh Var s, Log m) => [Eqn] -> StateT Γ m (Maybe (Θ, [Eqn], [WEqn]))
unify eqns = do 
  res <- run phases (Subst.ι, normEqns eqns, [])
  Log.debugM' "OWorld.unify" $ PP.vcat [ PP.text "Eqns:" <+> pPrint eqns
                                       , PP.text "Res :" <+> pPrint res ]
  return res
  where phases :: [([Eqn] -> StateT Γ m Res)] -- MR
        phases = [ phase1
                 , phase2
                 , phase3a
                 , phase3b
                 , phase4a
                 , phase4b
                 ]
        run [] res = return $ Just res
        run (p:ps) (θ, eqs, weqs) = do 
          res <- p eqs
          case res of 
            Fail -> return Nothing
            Unchanged -> run ps (θ, eqs, weqs)
            Changed (θ', eqs', []) -> run phases (θ ○ θ', eqs', weqs)
            Changed (θ', eqs', weqs') -> do
              res' <- W.unify $ weqs ++ weqs'
              case res' of
                Nothing -> return Nothing
                Just (θ'', weqs'') -> run phases (θ ○ θ' ○ θ'', eqs' ✴ θ'', weqs'')

{- 
 run phases (θ ○ θ', eqs', weqs ++ weqs')
-} 

instance Freeze World where
  freeze (W es) = W (freeze es)
  thaw ρ (W es) = W $ thaw ρ es

instance Unif.Unif World Tag where
  unify _ = fmap (fmap f) . unify
    where f (θ, eqs, weqs) = CSubst.make θ (Constr.listAnd $ map mkeq eqs ++ map mkweq weqs)
          mkeq (w1, w2) = Constr.Atom $ Rel (Pred.make "=") [encode w1, encode w2]
          mkweq (w1, w2) = Constr.Atom $ Rel (Pred.make "≡") [encode w1, encode w2]
  match tag eqs = fmap (thaw frozen) $ Unif.unify tag (map (second freeze) eqs)
    where frozen = Set.map Var.freeze (Var.vars eqs)

-- @ Solver

solver :: Solver
solver = Solver "Ordered" simplify valid 

valid :: (MonadState s m, HasFresh Var s, Log m) => Σ -> Formula -> StateT Γ m Bool
valid _ ψ = return $ Constr.simp ψ == (⊤)

simplify :: (MonadState s m, HasFresh Var s, Log m) => Σ -> Formula -> StateT Γ m (Maybe (Θ, Formula))
simplify _ ψ = do 
  -- Log.debugM' "World.simplify" $ pPrint ψ
  res <- unify $ fromConstr ψ
  case res of 
    Nothing -> return Nothing
    Just (θ, eqs, weqs) -> return $ Just (θ, toConstr eqs weqs)

toConstr :: [Eqn] -> [WEqn] -> Formula
toConstr eqs weqs = Constr.listAnd $ map mkeq eqs ++ map mkweq weqs
 where 
  mkeq (s, t) = Constr.Atom $ Rel (Pred.make "=") [encode s, encode t]
  mkweq (s, t) = Constr.Atom $ Rel (Pred.make "≡") [encode s, encode t]

fromConstr :: Formula -> [Eqn]
fromConstr f = Maybe.mapMaybe getEq (Constr.destAnd f)
 where
  getEq (Constr.Atom (Rel p [s, t])) 
   | Pred.name p == "=" = Just (decode s, decode t)
   | otherwise = error $ "bad pred: " ++ show p
  getEq Constr.Top = Nothing
  getEq t = error $ "fromConstr: " ++ show t

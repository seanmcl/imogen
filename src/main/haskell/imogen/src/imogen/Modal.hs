
-- | Modal logic.

-- @ Pragmas

{-# LANGUAGE CPP, Rank2Types, FlexibleContexts, MultiParamTypeClasses #-} 

-- @ Signature

module Imogen.Modal
  ( -- * World elements
    Elem
    -- * Worlds
  , World
  , Worlds(..)
  , ε 
  , var, param
  , null
  , extend
  , fromList
    -- * Predicates
  , allVars
  , containsConst
    -- * Views
  , ViewL(..)
  , viewl
  , ViewR(..)
  , viewr
  , ViewL2(..)
  , viewl2
  , (|>)
    -- * World atoms
  , Atom(Rel)
    -- * Modality
  , Mode(..)
  , legalSubst
    -- * State monad
  , HasMode(..)
  , get
    -- * Paths
  , mkPath
  , isPath
  , destPath
  , mkEdge
  , isEdge
  , destEdge
    -- * Unification
  , Eqn
  , mkEqn
  , isEqn
  , destEqn
  , Eqn3
  , eqn3ToEqn
  , eqnToEqn3
  , unify
  , simplify
  )
where

-- @ Imports

#include "../undefined.h" 

import Imogen.Util.Prelude hiding (null, seq, concat, length)
import qualified Control.Monad.State as State
import qualified Data.Foldable as Fold
import qualified Data.Maybe as Maybe
import qualified Imogen.Atom as Atom
import qualified Imogen.Ctx as Ctx
import Imogen.Ctx (Γ)
import qualified Imogen.Func as Func
import Imogen.Func (Func, Funcs(..))
import qualified Imogen.Param as Param
import Imogen.Param (Param, Params(..), Freeze(..), Petrify(..))
import qualified Imogen.Pred as Pred
import Imogen.Pred (Pred, Preds(..))
import Imogen.Rename (Rename(rename))
import qualified Imogen.Sort as Sort
import qualified Imogen.Subst as Subst
import Imogen.Subst (Θ, ι, (↦), (⟼), (○), (✴), (✶))
import qualified Imogen.Term as Term
import Imogen.Term (Term(..), Encode(..))
import qualified Imogen.Util.Fresh as Fresh
import Imogen.Util.Fresh (HasFresh)
import qualified Imogen.Util.List as List
import qualified Imogen.Util.Lex as Lex
import qualified Imogen.Util.Log as Log
import Imogen.Util.Log (Log)
import qualified Imogen.Util.Map as Map
import qualified Imogen.Util.Monad as M
import qualified Imogen.Util.Name as Name
import Imogen.Util.Name (HasName(..))
import qualified Imogen.Util.Parse as P
import Imogen.Util.Parse (Parse)
import qualified Imogen.Util.Print as PP
import Imogen.Util.Print (Print, pPrint, (<+>))
import qualified Imogen.Util.Seq as Seq
import Imogen.Util.Seq (Seq)
import qualified Imogen.Util.Set as Set
import Imogen.Util.Set ((∪), (∅))
import qualified Imogen.Var as Var
import Imogen.Var (Var, Vars(..))

-- @ World elements

-- | Elements of a world path.
data Elem = V Var | P Param | F Func
  deriving (Eq, Ord, Show)

isConst :: Elem -> Bool
isConst (V _) = False
isConst _ = True

isVar :: Elem -> Bool
isVar = not . isConst

-- @@ Instances

instance Vars Elem where
  vars (V x) = Set.singleton x
  vars _ = Set.empty
  free = vars

instance Params Elem where
  params (P x) = Set.singleton x
  params _ = Set.empty

instance Funcs Elem where
  arityFuncs (F x) = Set.singleton (x, 0)
  arityFuncs _ = Set.empty

instance Rename Elem where
  rename (V x) = V <$> rename x
  rename e = return e

instance Freeze Elem where
  freeze e = case e of 
    V x -> P $ Var.freeze x
    _ -> e
  thaw ρ e = case e of 
    P x -> if Set.member x ρ then V $ Var.thaw x else e
    _ -> e

instance Petrify Elem where
  petrify m e = case e of 
    P x -> maybe e F (Map.lookup x m)
    _ -> e

instance Encode Elem where
  encode e = case e of 
    V x -> Var x
    P x -> Param x
    F x -> Fn x []
  decode t = case t of 
    Var x -> V x
    Param x -> P x
    Fn x [] -> F x
    _ -> error' $ PP.text "can't decode:" <+> pPrint t

instance Print Elem where
  pPrint e = case e of 
   V x -> pPrint x
   P x -> pPrint x
   F x -> pPrint x

-- @ Worlds

newtype World = W { seq :: Seq Elem }
  deriving (Eq, Ord, Show)

-- | The initial world.
ε :: World
ε = W Seq.empty

null :: World -> Bool
null = Seq.null . seq

length :: World -> Int
length = Seq.length . seq

-- | Construct a world from a list of elements.
fromList :: [Elem] -> World
fromList = W . Seq.fromList

toList :: World -> [Elem]
toList = Seq.toList . seq

(><) :: World -> World -> World
(><) (W w1) (W w2) = W $ (Seq.><) w1 w2

concat :: [World] -> World
concat = foldr (><) ε 

singleton :: Elem -> World
singleton = W . Seq.singleton

var :: Var -> World
var = singleton . V

param :: Param -> World
param = singleton . P

allVars :: World -> Bool
allVars (W s) = Fold.all isVar s

containsConst :: World -> Bool
containsConst (W s) = Fold.any isConst s

-- @ Views

data ViewL = EmptyL | Elem :< World
data ViewR = EmptyR | World :> Elem
data ViewL2 = EmptyL2 | SingL2 Elem | (Elem, Elem) ::< World

viewl :: World -> ViewL
viewl (W s) = case Seq.viewl s of 
  Seq.EmptyL -> EmptyL
  (Seq.:<) e w -> e :< W w

viewr :: World -> ViewR
viewr (W s) = case Seq.viewr s of 
  Seq.EmptyR -> EmptyR
  (Seq.:>) w e -> W w :> e

viewl2 :: World -> ViewL2
viewl2 (W s) = case Seq.viewl2 s of 
  Seq.EmptyL2 -> EmptyL2
  Seq.SingL2 e -> SingL2 e
  (Seq.::<) (e1, e2) w -> (e1, e2) ::< W w

(|>) :: World -> Elem -> World
W w |> e = W $ (Seq.|>) w e

(<|) :: Elem -> World -> World
e <| W w = W $ (Seq.<|) e w

class Extend a where
  extend :: World -> a -> World

instance Extend Var where
  extend w x = w |> V x

instance Extend Param where
  extend w x = w |> P x

instance Extend Func where
  extend w x = w |> F x

-- @* Instances

instance Encode World where
  encode w = case viewl w of
    EmptyL -> Fn (Func.make "ε") [] 
    x :< s' | null s' -> encode x
            | otherwise -> Fn (Func.make "·") $ map encode (toList w)
  decode t = case t of 
    Var x -> singleton $ V x
    Param x -> singleton $ P x
    Fn f [] | Func.name f == "ε" -> ε 
            | otherwise -> singleton $ F f
    Fn f xs | Func.name f == "·" -> W $ Seq.fromList $ decodeList xs
            | otherwise -> __IMPOSSIBLE__ 

decodeList :: [Term] -> [Elem]
decodeList [] = []
decodeList (t:ts) = 
  let es = decodeList ts in
  case t of 
    Var x -> V x : es
    Param x -> P x : es
    Fn f [] -> F f : es
    Fn f xs | Func.name f == "·" -> decodeList xs ++ es
            | otherwise -> __IMPOSSIBLE__ 

instance Subst.GenApply Elem World where
  e ✶ θ = case e of 
    V x -> decode (x ✶ θ)
    _ -> singleton e

instance Subst.Apply World where
  (W s) ✴ θ = concat $ Seq.toList s ✶ θ

-- instance CSubst.Apply World where
--   w ✴ θ = w ✴ subst θ 

instance Vars World where
  vars (W s) = Fold.foldr (\e ρ -> vars e ∪ ρ) Set.empty s
  free = vars

instance Params World where
  params (W s) = Fold.foldr (\e ρ -> params e ∪ ρ) Set.empty s

instance Funcs World where
  arityFuncs (W s) = Fold.foldr (\e ρ -> arityFuncs e ∪ ρ) Set.empty s

instance Rename World where
  rename (W s) = W <$> rename s

instance Freeze World where
  freeze (W s) = W $ fmap freeze s
  thaw ps (W s) = W $ fmap (thaw ps) s

instance Petrify World where
  petrify m (W s) = W $ fmap (petrify m) s

instance Print World where
  pPrint (W s) 
   | Seq.null s = PP.text "ε"
   | otherwise = PP.hsep $ List.intersperse (PP.text "·") (map pPrint $ Seq.toList s)

-- @ Atoms

data Atom = Rel { wpred :: Pred
                , _wargs :: [Term]
                , _world :: World
                } 
  deriving (Eq, Ord, Show)

-- @@ Instances

instance HasName Atom where
  name = Name.name . wpred

instance Vars Atom where
  vars (Rel _ args w) = Var.vars args ∪ Var.vars w
  free = Var.vars

instance Params Atom where
  params (Rel _ args w) = Param.params args ∪ Param.params w

instance Funcs Atom where
  arityFuncs (Rel _ args w) = Func.arityFuncs args ∪ Func.arityFuncs w

instance Preds Atom where
  arityPreds (Rel p τs _) = Set.singleton (p, List.length τs)

instance Rename Atom where
  rename (Rel p τs w) = do
    τs' <- rename τs
    w' <- rename w
    return $ Rel p τs' w'

instance Subst.Apply Atom where
  (Rel p τs w) ✴ θ = Rel p (τs ✴ θ) (w ✴ θ)

instance Freeze Atom where
  freeze (Rel p ts w) = Rel p (freeze ts) (freeze w)
  thaw cs (Rel p ts w) = Rel p (thaw cs ts) (thaw cs w)

instance Petrify Atom where
  petrify ρ (Rel p ts w) = Rel p (petrify ρ ts) (petrify ρ w)

instance Print Atom where
  pPrint (Rel p ts w) = ppAtom p ts <+> ppWorld w

ppAtom :: Pred -> [Term] -> PP.Doc
ppAtom p [] = pPrint p
ppAtom p τs = pPrint p <> PP.parens (PP.fcat $ List.intersperse (PP.text ", ") (map pPrint τs))

ppWorld :: World -> PP.Doc
ppWorld w = PP.text "@" <+> pPrint w

-- @ Modalities

data Mode = None | K | D | K4 | D4 | T | S4 | S5
  deriving (Eq, Ord, Show)

instance Print Mode where
  pPrint = PP.text . show

instance Parse Mode where
  parser = P.choice $ map (\(s, c) -> Lex.reservedOp s >> return c) cons
   where cons = [ ("K", K)
                , ("D", D)
                , ("K4", K4)
                , ("D4", D4)
                , ("T", T)
                , ("S4", S4)
                , ("S5", S5) ]

class HasMode s where
  state :: s -> Mode

instance HasMode Mode where
  state = id

instance HasMode b => HasMode (a, b) where
  state (_, k) = state k

instance HasMode c => HasMode (a, b, c) where
  state (_, _, k) = state k

get :: (MonadState s m, HasMode s) => m Mode
get = State.gets state

-- | Is a substitution legal at a given modality.
legalSubst :: Mode -> Γ -> Θ -> Bool
legalSubst m ctx θ = 
  let -- Grab just the modal mappings
      getMode (x, t) = case Ctx.lookup x ctx of
        Nothing -> __IMPOSSIBLE__
        Just Sort.MWorld -> Just $ decode t
        _ -> Nothing
      l :: [World] = Maybe.mapMaybe getMode (Subst.toList θ)
  in case m of 
       K -> all ((== 1) . length) l
       D -> all ((== 1) . length) l
       T -> all ((<= 1) . length) l
       K4 -> all ((>= 1) . length) l
       D4 -> all ((>= 1) . length) l
       S4 -> True
       S5 -> all ((== 1) . length) l
       None -> True

-- @ Unification

{- | 
   Equations betwen worlds π1 ≡ π2.  This is the form the
   equality constraints will be stored.  The unification procedure
   uses a three-part representation of equations, the Eqn3 type.
-} 
type Eqn = (World, World)

-- | The three-part equations used by the Otten-Kreitz unification procedure.
type Eqn3 = (World, World, World)

-- | The return type of unification simplification. 
data Res = Changed (Θ, [Eqn3])
         | Unchanged
         | Fail

instance Print Res where
  pPrint Fail = PP.text "Fail"
  pPrint Unchanged = PP.text "Unchanged"
  pPrint (Changed θ) = PP.text "Changed" <+> pPrint θ

-- The state maintains the current substitution and the auxiliary variables.

data PhaseState = PS { _aux :: Set Var
                     , csubst :: Θ 
                     }

data Phase = Phase 
  { func :: (MonadState s m, HasFresh Var s, Log m) => [Eqn3] -> StateT PhaseState m Res
  , invertible :: Bool
  , phaseId :: String
  }

instance Print Phase where
  pPrint = PP.text . phaseId

-- @* Util

-- Split a world at a variable.
splitVar :: World -> Maybe (World, Var, World)
splitVar s = case viewl s of
  EmptyL -> Nothing
  V x :< s' -> Just (ε, x, s')
  x :< s' -> case splitVar s' of 
    Nothing -> Nothing
    Just (s1, v, s2) -> Just (x <| s1, v, s2)

-- @* S4

trace :: Phase -> Phase
trace (Phase pfun inv pid) = Phase pfun' inv pid
 where 
  pfun' eqs = do
    res <- pfun eqs
    M.lift $ Log.debugM' pid (PP.text "in :" <+> pPrint eqs $$ 
                              PP.text "out:" <+> pPrint res)
    return res 

{- 
{ε = ε | ε} → {}, {}
-} 

phaseS4_1 :: Phase
phaseS4_1 = Phase pfun True "S4_1"
 where 
  pfun eqs = 
    case List.findRem ffn eqs of
      Nothing -> return Unchanged
      Just (_, eqs') -> return $ Changed (ι, eqs')
  ffn (w1, w2, w3) = null w1 && null w2 && null w3

{- 
{ε = ε | t+} → {t+ = ε | ε}, {}
-} 

phaseS4_2 :: Phase
phaseS4_2 = Phase pfun True "S4_2"
 where 
  pfun eqs =
    case List.findRem ffn eqs of
      Nothing -> return Unchanged
      Just ((w1, w2, w3), eqs') -> return $ Changed (ι, (w3, w1, w2) : eqs')
  ffn (w1, w2, w3) = null w1 && null w2 && not (null w3)

{- 
{X s = ε | X t} → {s = ε | t}, {}
-} 

phaseS4_3 :: Phase
phaseS4_3 = Phase pfun True "S4_3"
 where 
  pfun eqs = 
    case List.findRemFirst ffn eqs of
      Nothing -> return Unchanged
      Just (eq, eqs') -> return $ Changed (ι, eq : eqs')
  ffn (w1, eps, w3) | null eps = case (viewl w1, viewl w3) of
    (x :< s, x' :< t) | x == x' -> Just (s, eps, t)
    _ -> Nothing
  ffn _ = Nothing

{- 
{C s = ε | V t} → {V t = ε | C s}, {}
-} 

phaseS4_4 :: Phase
phaseS4_4 = Phase pfun True "S4_4"
 where
  pfun eqs = 
    case List.findRemFirst ffn eqs of
      Nothing -> return Unchanged
      Just (eq, eqs') -> return $ Changed (ι, eq : eqs')
  ffn (w1, eps, w3) | null eps = case (viewl w1, viewl w3) of
    (c :< _, v :< _) | isConst c && isVar v -> Just (w3, eps, w1)
    _ -> Nothing
  ffn _ = Nothing

{- 
We split phase 5 into two phases.  One that's deterministic when z ≡ ε,
and a nondeterministic analog where z ≠ ε 

{V s = z | ε} → {s = ε | ε}, {V ↦ z}

{V s = z | ε} → {s = ε | ε}, {V ↦ z}
-} 

phaseS4_5a :: Phase
phaseS4_5a = Phase pfun True "S4_5a"
 where
  pfun eqs = 
    case List.findRemFirst ffn eqs of
      Nothing -> return Unchanged
      Just (θ, eqs') -> return $ Changed (θ, eqs')
  ffn (w1, z, eps) | null eps && null z = case viewl w1 of
    V v :< _ -> Just (v ↦ encode ε)
    _ -> Nothing
  ffn _ = Nothing

phaseS4_5 :: Phase
phaseS4_5 = Phase pfun False "S4_5"
 where
  pfun eqs = 
    case List.findRemFirst ffn eqs of
      Nothing -> return Unchanged
      Just ((eq, θ), eqs') -> return $ Changed (θ, eq : eqs')
  ffn (w1, z, eps) | null eps = case viewl w1 of
    V v :< s -> Just ((s, eps, eps), v ↦ encode z)
    _ -> Nothing
  ffn _ = Nothing

{- 
{V s = ε | C t} → {s = ε | C t}, {V ↦ ε}
-} 

phaseS4_6 :: Phase
phaseS4_6 = Phase pfun False "S4_6"
 where 
  pfun eqs =
    case List.findRemFirst ffn eqs of
      Nothing -> return Unchanged
      Just ((eq, θ), eqs') -> return $ Changed (θ, eq : eqs')
  ffn (w1, eps, w3) | null eps = case (viewl w1, viewl w3) of
    (V v :< s, c :< _t) | isConst c -> Just ((s, eps, w3), v ↦ encode ε)
    _ -> Nothing
  ffn _ = Nothing

{- 
{V s = z | C1 C2 t} → {s = ε | C2 t}, {V ↦ z C1}
-} 

phaseS4_7 :: Phase
phaseS4_7 = Phase pfun False "S4_7"
 where
  pfun eqs = 
    case List.findRemFirst ffn eqs of
      Nothing -> return Unchanged
      Just ((eq, θ), eqs') -> return $ Changed (θ, eq : eqs')
  ffn (w1, z, w3) = case (viewl w1, viewl2 w3) of
    (V v :< s, (c1, c2) ::< t) 
     | isConst c1 && isConst c2 -> 
       Just ((s, ε, c2 <| t), v ↦ encode (z |> c1))
    _ -> Nothing

{- 
{V s+ = ε | V1 t} → {V1 t = V | s+}, {}
-} 

phaseS4_8 :: Phase
phaseS4_8 = Phase pfun False "S4_8"
 where
  pfun eqs = 
    case List.findRemFirst ffn eqs of
      Nothing -> return Unchanged
      Just (eq, eqs') -> return $ Changed (ι, eq : eqs')
  ffn (w1, eps, w3) 
   | null eps = case (viewl w1, viewl w3) of
     (V v :< s, V v1 :< _t) -> 
       -- phase3 should ensure that v ≠ v1
       if v == v1 then __IMPOSSIBLE__ else
       Just (w3, singleton $ V v, s)
     _ -> Nothing
   | otherwise = Nothing

{- 
{V s+ = z+ | V1 t} → {V1 t = V' | s+}, {V ↦ z+ V'}
-} 

phaseS4_9 :: Phase
phaseS4_9 = Phase pfun False "S4_9"
 where 
  pfun eqs = do
    r <- M.findRemFirstM ffn eqs 
    return $ case r of 
      Nothing -> Unchanged
      Just ((eq, θ), eqs') -> Changed (θ, eq : eqs')
  ffn (w1, z, w3) | not (null z) = case (viewl w1, viewl w3) of
    (V v :< s, V v1 :< _t) ->
      if v == v1 then __IMPOSSIBLE__ else do
      -- phase3 should ensure that v ≠ v1
      v' <- M.lift $ Fresh.fresh "w"
      -- add v' as an auxiliary variable
      State.modify (\(PS ρ θ) -> PS (Set.insert v' ρ) θ)
      return $ Just ((w3, singleton (V v'), s), v ↦ encode (z |> V v'))
    _ -> return Nothing
  ffn _ = return Nothing

{- 
{V s = z | X t} → {V s = z X | t}, {} where V ≠ X and s = ε or t ≠ ε or X is a constant
-} 

phaseS4_10 :: Phase
phaseS4_10 = Phase pfun False "S4_10"
 where
  pfun eqs = 
    case List.findRemFirst ffn eqs of
      Nothing -> return Unchanged
      Just (eq, eqs') -> return $ Changed (ι, eq : eqs')
  ffn (w1, z, w3) = case (viewl w1, viewl w3) of
    (V v :< s, x :< t) | V v /= x && (null s || not (null t) || isConst x) -> 
      Just (w1, z |> x, t)
    _ -> Nothing

-- @* D and K

{- 
Section 4.1

Note that for this theory, there are MGUs, so you should never end up
with constraints.

Rule R2

{V s = ε | X t} → {s = ε | t}, {V ↦ X}, V ≠ X
-} 

phaseK_2 :: Phase
phaseK_2 = Phase pfun True "K_2"
 where
  pfun eqs = 
    case List.findRemFirst ffn eqs of
      Nothing -> return Unchanged
      Just ((eq, θ), eqs') -> return $ Changed (θ, eq : eqs')
  ffn (w1, eps, w3) 
   | null eps = case (viewl w1, viewl w3) of
      (V v :< s, x :< t) | V v /= x -> Just ((s, ε, t), v ↦ encode x)
      _ -> Nothing
   | otherwise = Nothing

-- @* D4 and K4

{- 
Section 4.2 

{V s = z | ε} → {s = ε | ε}, {V ↦ z} where z ≠ ε or (V ∈ Vars' and R_V^{(σ)})

R_V^{(σ)} = False if Ṽ ↦ V ∈ σ for some Ṽ ∈ Vars
          = True otherwise 
-} 

phaseK4_5 :: Phase
phaseK4_5 = Phase pfun False "K4_5"
 where
  pfun eqs = do 
    PS ρ θ <- State.get
    let ffn (w1, z, eps) | null eps = case viewl w1 of
          V v :< s | not (null z) || checkK4 ρ θ v -> Just ((s, eps, eps), v ↦ encode z)
          _ -> Nothing
        ffn _ = Nothing
    case List.findRemFirst ffn eqs of
      Nothing -> return Unchanged
      Just ((eq, θ'), eqs') -> return $ Changed (θ', eq : eqs')

-- | checkK4 tests (V ∈ Vars' and R_V^{(σ)})

checkK4 :: Set Var -> Θ -> Var -> Bool
checkK4 ρ θ v = Set.member v ρ && not (List.any ffn $ Map.toList m)
 where
   m = Subst.toMap θ 
   ffn (v1, Var v') | v == v' = not (Set.member v1 ρ)
   ffn _ = False

{- 
Rule R6

{V s = ε | C t} → {s = ε | C t}, {V ↦ ε} where (V ∈ V' and R_{V}^{σ})
-} 

phaseK4_6 :: Phase
phaseK4_6 = Phase pfun False "K4_6"
 where
  pfun eqs = do
    PS ρ θ <- State.get
    let ffn (w1, eps, w3) | null eps = case (viewl w1, viewl w3) of
          (V v :< s, c :< _t) | isConst c && checkK4 ρ θ v -> Just ((s, eps, w3), v ↦ encode ε)
          _ -> Nothing
        ffn _ = Nothing
    case List.findRemFirst ffn eqs of
      Nothing -> return Unchanged
      Just ((eq, θ'), eqs') -> return $ Changed (θ', eq : eqs')

-- @* S5

{- 
Section 4.3

{V = ε | X} → {}, {V ↦ X}, where V ≠ X 
-}

phaseS5_1 :: Phase
phaseS5_1 = Phase pfun True "S5_1"
 where
  pfun eqs = case List.findRemFirst ffn eqs of
    Nothing -> return Unchanged
    Just (θ, eqs') -> return $ Changed (θ, eqs')
  ffn (w1, eps, w3) | null eps = case (viewl w1, viewl w3) of
    (V v :< s, x :< t) | null s && null t -> Just (v ↦ encode x)
    _ -> Nothing
  ffn _ = Nothing

{- 
Rule R2

{X = ε | X} → {}, {}
-} 

phaseS5_2 :: Phase
phaseS5_2 = Phase pfun True "S5_2"
 where
  pfun eqs = case List.findRem ffn eqs of
    Nothing -> return Unchanged
    Just (_, eqs') -> return $ Changed (ι, eqs')
  ffn (w1, eps, w3) | null eps = case (viewl w1, viewl w3) of
    (x :< s, x' :< t) | x == x' && null s && null t -> True
    _ -> False
  ffn _ = False

{- 
Rule R3

{C = ε | V} → {V = ε | C}, {}
-} 

phaseS5_3 :: Phase
phaseS5_3 = Phase pfun True "S5_3"
 where
  pfun eqs = case List.findRemFirst ffn eqs of
    Nothing -> return Unchanged
    Just (eq, eqs') -> return $ Changed (ι, eq : eqs')
  ffn (w1, eps, w3) | null eps = case (viewl w1, viewl w3) of
    (c :< s, V v :< t) | isConst c && null s && null t -> Just (singleton (V v), ε, singleton c)
    _ -> Nothing
  ffn _ = Nothing

-- @ T

{- 
Strings indexed with a v may only contain variables
Strings indexed with a c must contain at least one constant

Split phase 5 into two phases, one deterministic and one nondeterministic,
as in S4_5.

Rule R5

{V s = z_v | ε} → {s = ε | ε}, {V ↦ z_v}

-} 

phaseT_5a :: Phase
phaseT_5a = Phase pfun True "T_5a"
 where
  pfun eqs = case List.findRemFirst ffn eqs of
    Nothing -> return Unchanged
    Just (θ, eqs') -> return $ Changed (θ, eqs')
  ffn (w1, z, eps) | null eps && null z = case viewl w1 of
    V v :< _ -> Just (v ↦ encode z)
    _ -> Nothing
  ffn _ = Nothing

phaseT_5 :: Phase
phaseT_5 = Phase pfun False "T_5"
 where
  pfun eqs = case List.findRemFirst ffn eqs of
    Nothing -> return Unchanged
    Just ((eq, θ), eqs') -> return $ Changed (θ, eq : eqs')
  ffn (w1, z, eps) | null eps && allVars z = case viewl w1 of
    V v :< s -> Just ((s, ε, ε), v ↦ encode z)
    _ -> Nothing
  ffn _ = Nothing

{- 
Rule R6

{s1 V s2 = z | C t} ↦ {s1 = ε | z, s2 = ε | t}, {v ↦ C} ∪ σ_V^{σ}

The substitution σ_V^{σ} =
  {Ṽ ↦ ε | X ↦ t ∈ σ where t contains V and Ṽ for some Ṽ ∈ Vars ∪ Vars' with V ≠ Ṽ}
where σ is the substitution computed so far.

In other words, all variables besides V in terms including V get mapped to ε.
-} 

phaseT_6 :: Phase
phaseT_6 = Phase pfun False "T_6"
 where
  pfun eqs = do
    PS _ θ <- State.get
    let ffn (w1, z, w3) = case (splitVar w1, viewl w3) of
          (Just (s1, v, s2), c :< t) | isConst c -> Just ((s1, ε, z), (s2, ε, t), (v ⟼ encode c) (mkSub θ v))
          _ -> Nothing
    case List.findRemFirst ffn eqs of
      Nothing -> return Unchanged
      Just ((eq1, eq2, θ'), eqs') -> return $ Changed (θ', eq1 : eq2 : eqs')

mkSub :: Θ -> Var -> Θ
mkSub θ v = 
  let m = Subst.toMap θ 
      (_, ts) = unzip $ Map.toList m
      ts' = filter (Set.member v . vars) ts
      vs = vars ts'
  in Set.fold (\v' -> v' ⟼ encode ε) ι vs

{- 
Rule R7

{s_c+ = ε | t_v+} → {t_v+ = ε | s_c+}, {}
-} 

phaseT_7 :: Phase
phaseT_7 = Phase pfun False "T_7"
 where
  pfun eqs = case List.findRemFirst ffn eqs of
    Nothing -> return Unchanged
    Just (eq, eqs') -> return $ Changed (ι, eq : eqs')
  ffn (s, eps, t) | null eps && not (null s) && containsConst s && allVars t = Just $ (t, ε, s)
  ffn _ = Nothing

{- 
Rule R8

{V s_v+ = ε | V1 t_v} → {V1 t_v = V | s_v+}, {}
-} 

phaseT_8 :: Phase
phaseT_8 = Phase pfun False "T_8"
 where
  pfun eqs = case List.findRemFirst ffn eqs of
    Nothing -> return Unchanged
    Just (eq, eqs') -> return $ Changed (ι, eq : eqs')
  ffn (w1, eps, w3) | null eps = case (viewl w1, viewl w3) of
    (V v :< s, V v1 :< t) | not (null s) && allVars s && allVars t && v /= v1 -> Just (w3, singleton $ V v, s)
    _ -> Nothing
  ffn _ = Nothing

{- 
Rule R9

{V s_v+ = z+ | V1 t_v} → {V1 t_v = V' | s_v+}, {V ↦ z+ V'}
-} 

phaseT_9 :: Phase
phaseT_9 = Phase pfun False "T_9"
 where
  pfun eqs = do
    v' <- M.lift $ Fresh.fresh "w"
    State.modify (\(PS ρ θ) -> PS (Set.insert v' ρ) θ)
    let ffn (w1, z, w3) | not (null z) = case (viewl w1, viewl w3) of
          (V v :< s, V v1 :< t) | not (null s) && allVars s && allVars t && v /= v1 -> 
            Just ((w3, singleton $ V v', s), v ↦ (encode $ z |> V v'))
          _ -> Nothing
        ffn _ = Nothing
    case List.findRemFirst ffn eqs of
      Nothing -> return Unchanged
      Just ((eq, θ), eqs') -> return $ Changed (θ, eq : eqs')

{- 
Rule R10

{X s = z | V t} → {X s = z V | t}, {}, where X ≠ V and (s = ε or t ≠ ε)
-} 

phaseT_10 :: Phase
phaseT_10 = Phase pfun False "T_10"
 where
  pfun eqs = case List.findRemFirst ffn eqs of
    Nothing -> return Unchanged
    Just (eq, eqs') -> return $ Changed (ι, eq : eqs')
  ffn (w1, z, w3) = case (viewl w1, viewl w3) of
    (x :< s, V v :< t) | x /= V v && (null s || not (null t)) -> 
      Just (w1, z |> V v, t)
    _ -> Nothing

-- @ Phases

modePhases' :: Mode -> [Phase]
modePhases' m = case m of 
  None -> [ phaseS4_1 ]
  S4 -> [ phaseS4_1, phaseS4_2, phaseS4_3, phaseS4_4, phaseS4_5a, phaseS4_5
        , phaseS4_6, phaseS4_7, phaseS4_8, phaseS4_9, phaseS4_10 ]
  K -> k
  D -> k
  K4 -> k4
  D4 -> k4
  S5 -> [ phaseS5_1, phaseS5_2, phaseS5_3 ]
  -- Note there's no rule 4 on purpose.  The paper doesn't have a rule 4.
  T -> [ phaseS4_1, phaseS4_2, phaseS4_3, phaseT_5a, phaseT_5, phaseT_6
       , phaseT_7, phaseT_8, phaseT_9, phaseT_10 ]
 where 
  k = [ phaseS4_1, phaseK_2, phaseS4_3, phaseS4_4 ]
  k4 = [ phaseS4_1, phaseS4_2, phaseS4_3, phaseS4_4, phaseK4_5
       , phaseK4_6, phaseS4_7, phaseS4_8, phaseS4_9, phaseS4_10 ]

modePhases :: Mode -> [Phase]
modePhases = map trace . modePhases'

-- @ Unify

{- 
Run the full algorithm, generating all mgus.  This should only be used when
testing subsumption of the goal.
-} 

unify :: (MonadState s m, HasFresh Var s, Functor m, Log m) => Mode -> [Eqn3] -> m [Θ]
unify m eqs = do
  Log.debugM' "Modal.unify" $ PP.text "inp:" <+> pPrint (m, eqs)
  res <- unify' (modePhases m) eqs
  Log.debugM' "Modal.unify" $ PP.text "res:" <+> pPrint res
  return res

unify' :: forall s m. (MonadState s m, HasFresh Var s, Functor m, Log m) => [Phase] -> [Eqn3] -> m [Θ]
unify' phases = run (PS Set.empty ι)
 where
  (detPhases, nondetPhases) = List.partition invertible phases 
  -- run all deterministic phases
  runDet :: [Phase] -> [Eqn3] -> StateT PhaseState m (Maybe [Eqn3])
  runDet ps eqs = 
   case ps of 
    [] -> return $ Just eqs
    p:ps' -> do
      pres <- func p eqs
      case pres of
        Fail -> return Nothing
        Unchanged -> runDet ps' eqs
        Changed (θ', eqs') -> do
          State.modify $ \(PS ρ θ) -> PS ρ (θ ○ θ')
          runDet detPhases (eqs' ✴ θ')
  runNondet :: PhaseState -> Phase -> [Eqn3] -> m (Maybe (PhaseState, [Eqn3]))
  runNondet s p eqs = do
    (pres, PS ρ θ) <- State.runStateT (func p eqs) s
    case pres of 
      Fail -> return Nothing
      Unchanged -> return Nothing
      Changed (θ', eqs') -> do
        return $ Just (PS ρ (θ ○ θ'), eqs' ✴ θ')
  run :: PhaseState -> [Eqn3] -> m [Θ]
  run (PS _ θ) [] = return [θ]
  run s eqs = do
    (det, s') <- State.runStateT (runDet detPhases eqs) s
    case det of 
      Nothing -> return []
      Just [] -> return [csubst s']
      Just eqs' -> do
        eqss <- Maybe.catMaybes <$> mapM (\p -> runNondet s' p eqs') nondetPhases
        List.concat <$> mapM (uncurry run) eqss

{- 
Deterministic unification.  If there are equations left over, throw
them in the constraints.
-} 

simplify :: (MonadState s m, HasFresh Var s, HasMode s, Log m) => [Eqn3] -> m (Maybe (Θ, [Eqn3]))
simplify eqs = do
  m <- get
  Log.debugM' "Modal.simplify" $ pPrint eqs
  simplify' (modePhases m) eqs

simplify' :: forall s m. (MonadState s m, HasFresh Var s, HasMode s, Log m) => [Phase] -> [Eqn3] -> m (Maybe (Θ, [Eqn3]))
simplify' phases eqs = do
  m <- get
  res <- State.evalStateT (run detPhases eqs) (PS Set.empty ι)
  Log.debugM' "Modal.simplify'" $ pPrint (phases, eqs, res)
  case res of
    Nothing -> return Nothing
    -- K and D are unitary.  Any constraints are contradictory.
    Just (_, eqns) 
      | (m == K || m == D) && not (List.null eqns) -> return Nothing
      | otherwise -> return res
 where
  detPhases = List.filter invertible phases
  run :: [Phase] -> [Eqn3] -> StateT PhaseState m (Maybe (Θ, [Eqn3]))
  run [] eqns = do
    θ <- State.gets csubst
    return $ Just $ (θ, eqns)
  run (p:ps) eqns = do
    pres <- func p eqns
    case pres of
      Fail -> return Nothing
      Unchanged -> run ps eqns
      Changed (θ', eqns') -> do
        State.modify $ \(PS ρ θ) -> PS ρ (θ ○ θ')
        run detPhases (eqns' ✴ θ')

-- @ Worlds class

class Worlds a where
  worlds :: a -> Set World

instance Worlds World where
  worlds = Set.singleton

instance (Worlds a, Worlds b) => Worlds (a, b) where
  worlds (a, b) = worlds a ∪ worlds b

instance Worlds a => Worlds (Set a) where
  worlds = Set.fold (\x s -> worlds x ∪ s) (∅) 

instance (Worlds b) => Worlds (Map a b) where
  worlds = Map.fold (\x s -> worlds x ∪ s) (∅) 

instance (Worlds a1, Worlds a2, Worlds a3) => Worlds (a1, a2, a3) where
  worlds (a1, a2, a3) = Set.unions [worlds a1, worlds a2, worlds a3]

instance Worlds Atom where
  worlds (Rel _ _ w) = Set.singleton w

-- @ Paths

mkPath :: World -> Atom.Atom
mkPath w = Atom.Rel (Pred.make "path") [encode w]

isPath :: Atom.Atom -> Bool
isPath (Atom.Rel p _) | Pred.name p == "path" = True
                      | otherwise = False

destPath :: Atom.Atom -> World
destPath (Atom.Rel p [w]) | Pred.name p == "path" = decode w
                          | otherwise = error "Not a path"
destPath _ = error "Not a world"

mkEdge :: World -> Atom.Atom
mkEdge w = Atom.Rel (Pred.make "edge") [encode w]

isEdge :: Atom.Atom -> Bool
isEdge (Atom.Rel p _) | Pred.name p == "edge" = True
                      | otherwise = False

destEdge :: Atom.Atom -> World
destEdge (Atom.Rel p [w]) | Pred.name p == "edge" = decode w
                          | otherwise = error "Not an edge"
destEdge _ = error "Not a world"

mkEqn :: Eqn -> Atom.Atom
mkEqn (x, y) = Atom.Rel (Pred.make "≡") [encode x, encode y]

isEqn :: Atom.Atom -> Bool
isEqn (Atom.Rel p _) | Pred.name p == "≡" = True
                     | otherwise = False

destEqn :: Atom.Atom -> Eqn
destEqn (Atom.Rel p [x, y]) | Pred.name p == "≡" = (decode x, decode y)
                            | otherwise = error "Not an equation"
destEqn _ = error "Not an equation"

eqnToEqn3 :: Eqn -> Eqn3
eqnToEqn3 (x, y) = (x, ε, y)

eqn3ToEqn :: Eqn3 -> Eqn
eqn3ToEqn (x, y, z) = (x, y >< z)


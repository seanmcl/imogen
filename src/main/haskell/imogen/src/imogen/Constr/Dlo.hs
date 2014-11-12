-- FIXME: DOC
-- @ Pragmas

{-# LANGUAGE CPP, MultiParamTypeClasses #-} 

-- @ Signature

module Imogen.Constr.Dlo
  ( Tag(..)
  , solver
  , inferSig
  )
where

-- @ Imports

#include "../../undefined.h" 

import Imogen.Util.Prelude hiding (Real)
import qualified ATP.Dlo as Dlo
import qualified ATP.Formula as F
import qualified ATP.FormulaSyn as F
import qualified ATP.Skolem as Skolem
-- import qualified ATP.Util.Print as APrint
import qualified Control.Monad.State as State
import Control.Monad.State (State)
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Imogen.Atom as Atom
import Imogen.Atom (Atom(Rel))
import qualified Imogen.Constr as C
import qualified Imogen.Ctx as Ctx
import Imogen.Ctx (Γ)
import qualified Imogen.Func as Func
import qualified Imogen.Param as Param
import Imogen.PFormula (Pos(..), Neg(..))
import qualified Imogen.Pred as Pred
import qualified Imogen.Sig as Sig
import Imogen.Sig (Σ)
import qualified Imogen.Sort as Sort
import Imogen.Sort (Base)
import qualified Imogen.Subst as Subst
import Imogen.Subst (Θ)
import Imogen.Term(Term(..))
import qualified Imogen.Util.Log as Log
import Imogen.Util.Log (Log)
import qualified Imogen.Unif as Unif
import qualified Imogen.Util.Print as PP
import Imogen.Util.Print (Print, pPrint)
import qualified Imogen.Util.Set as Set
import Imogen.Util.Set ((∅))
import qualified Imogen.Var as Var
import Imogen.Var (Var)

-- @ Tag

data Tag = Tag
instance Sort.Tag Tag where

-- FIKME: Overlapping instance warning here

-- instance Sort.TagOf Tag Term where

instance Unif.Unif Term Tag where
  unify1 _ t1 t2 = Unif.unify1 Unif.U t1 t2
  match1 _ t1 t2 = Unif.match1 Unif.U t1 t2

instance Sort.Normalize Term Tag where
  normalize _ = id

instance Sort.OK Term Tag where
  ok _ _ = True

-- @ Marshalling to and from ATP Formulas

{- 
To marshal from formulas to ATP formulas, we need to remember which
ATP variables correspond to Imogen parameters and constants.  For
unmarshaling, we need to associate Imogen sorts with the ATP formulas.
-} 

data Env = Env { params :: Set F.Var
               , consts :: Set F.Var
               , _ד :: Γ
               , sorts :: Map F.Var Base
               }
  deriving (Eq, Ord, Show)

mkEnv :: Γ -> Env
mkEnv ד = Env (∅) (∅) ד Map.empty

addParam :: F.Var -> State Env ()
addParam x = do 
  env <- State.get
  State.put $ env { params = Set.insert x (params env) }

addConst :: F.Var -> State Env ()
addConst x = do 
  env <- State.get
  State.put $ env { consts = Set.insert x (consts env) }

isParam :: F.Var -> State Env Bool
isParam x = do 
  env <- State.get
  return $ Set.member x (params env)

isConst :: F.Var -> State Env Bool
isConst x = do 
  env <- State.get
  return $ Set.member x (consts env)

addSort :: String -> Base -> State Env ()
addSort x σ = do 
  env <- State.get
  let σs = sorts env
  case Map.lookup x σs of
    Nothing -> State.put $ env { sorts = Map.insert x σ σs }
    Just σ' | σ == σ' -> return ()
            | otherwise -> __IMPOSSIBLE__

class Marshal a b where
  marshal :: a -> State Env b
  unmarshal :: b -> State Env a

instance Marshal a b => Marshal [a] [b] where
  marshal = mapM marshal
  unmarshal = mapM unmarshal

-- Parameters become universally quantified variables.

instance Marshal Term F.Term where
  marshal t = case t of
    Var x -> return $ F.Var $ Var.marshal x 
    Param c -> do
      let c' = Param.marshal c 
      addParam c'
      return $ F.Var c'
    Fn n [] -> 
      let n' = Func.name n in
      if List.all Char.isDigit n' then return $ F.Fn n' [] 
      else do 
        let c = Func.marshal n
        addConst c
        return $ F.Var c
    _ -> error' $ PP.text "The DLO theory doesn't recognize the following term that arose during search:" 
                  $$ pPrint t
  unmarshal t = case t of 
    F.Var x -> do
      p1 <- isParam x
      p2 <- isConst x
      return $ 
       if p1 then Param $ Param.unmarshal x else
       if p2 then Fn (Func.unmarshal x) [] else 
       Var $ Var.unmarshal x
    F.Num n -> return $ Fn (Func.make $ show n) []
    F.Fn x τs 
      | τs == [] && all (\c -> Char.isDigit c || c == '-') x -> 
        return $ Fn (Func.make x) []
      | otherwise -> __IMPOSSIBLE__

instance Marshal Atom F.Rel where
  marshal a = case a of 
      Rel op [t1, t2] -> m (Pred.name op) [t1, t2]
      _ -> __IMPOSSIBLE__ 
    where m f ts = marshal ts >>= return . F.R f 
  unmarshal a = case a of 
      F.R p [t1, t2] -> do
        t1' <- unmarshal t1
        t2' <- unmarshal t2
        return $ Rel (Pred.make p) [t1', t2']
      _ -> __IMPOSSIBLE__

instance Marshal C.Formula F.Formula where
  marshal f = case f of
      C.Atom a -> marshal a >>= return . F.Atom
      C.Top -> return F.Top
      C.Bot -> return F.Bot
      C.Not p -> marshal p >>= return . F.Not
      C.And p q -> binop p q F.And
      C.Or p q -> binop p q F.Or
      C.Imp p q -> binop p q F.Imp
      C.Iff p q -> binop p q F.Iff
      C.All x σ p -> exop x σ p F.All
      C.Ex x σ p -> exop x σ p F.Ex
      _ -> __IMPOSSIBLE__
    where binop p q op = do
            p' <- marshal p
            q' <- marshal q
            return $ op p' q'
          exop x σ p op = do
            let x' = Var.marshal x
            addSort x' σ 
            p' <- marshal p
            return $ op x' p'
  unmarshal f = case f of 
      F.Atom a -> unmarshal a >>= return . C.Atom
      F.Top -> return C.Top
      F.Bot -> return C.Bot
      F.Not p -> unmarshal p >>= return . C.Not
      F.And p q -> binop p q C.And 
      F.Or p q -> binop p q C.Or
      F.Imp p q -> binop p q C.Imp 
      F.Iff p q -> binop p q C.Iff
      F.All x p -> exop x p C.All
      F.Ex x p -> exop x p C.Ex
    where binop p q op = do
            p' <- unmarshal p
            q' <- unmarshal q
            return $ op p' q'
          exop x p op = do
            let x' = Var.unmarshal x
            p' <- unmarshal p
            return $ op x' Sort.Real p'

toATP :: Γ -> C.Formula -> (F.Formula, Env)
toATP ctx f = 
  let (f', env) = State.runState (marshal f) (mkEnv ctx)
      ρs = Set.toList $ params env
  in (F.listAll ρs f', env)

fromATP :: Env -> F.Formula -> C.Formula
fromATP env f = State.evalState (unmarshal f) env

-- @ Constraints

valid :: Monad m => Σ -> C.Formula -> StateT Γ m Bool
valid _ ψ = do
  ד <- State.get
  return $ Dlo.valid $ fst $ toATP ד ψ 

simplify :: (Monad m, Log m) => Σ -> C.Formula -> StateT Γ m (Maybe (Θ, C.Formula))
simplify _ f = do 
  ד :: Γ <- State.get
  let (f', env) = toATP ד f
  -- Log.debugM' "Dlo.simplify" $ APrint.pPrint f'
  let f'' = fromATP env $ Skolem.simplify f'
  -- Log.debugM' "Dlo.simplify" $ pPrint f''
  let res = if f == f'' then Nothing else Just $ (Subst.ι, f'')
  -- Log.debugM' "Dlo.simplify" $ pPrint (f, res)
  return res

solver :: C.Solver
solver = C.Solver "DLO" simplify valid

-- @ Sort inference

{- 

  Γ, x : σ ⊢ A ⇒ Σ            Γ ⊢ A ⇒ Σ1  Γ ⊢ B ⇒ Σ2     
-------------------------   -----------------------------
 Γ ⊢ ∀ (x : σ). A ⇒ Σ           Γ ⊢ A ∧ B ⇒ Σ1 ∪ Σ2      


  Γ ⊢ s ⇐ Real ⇒ Σ1  Γ ⊢ t ⇐ Real ⇒ Σ2           Γ ⊢ ts ⇒ σs, Σ   
-----------------------------------------  -----------------------------
       Γ ⊢ s = t ⇒ Σ1 ∪ Σ2                 Γ ⊢ p(ts) ⇒ {p ↦ σs → o, Σ}


                       Γ ⊢ t ⇒ σ, Σ1   Γ ⊢ ts ⇒ σs, Σ2
------------------   -------------------------------------
  Γ ⊢ [] ⇒ [], ι         Γ ⊢ t : ts ⇒ σ : σs, Σ1 ∪ Σ2


  Γ(x) = σ              Γ(x) = σ     
--------------    -------------------
 Γ ⊢ x ⇒ σ, ι       Γ ⊢ x ⇐ σ ⇒ ι


         Γ ⊢ ts ⇒ σs, Σ                         Γ ⊢ ts ⇒ σs, Σ 
-------------------------------------  ------------------------------------
  Γ ⊢ f(ts) ⇐ σ ⇒ {f ↦ σs → σ, Σ}      Γ ⊢ f(ts) ⇒ {f ↦ σs → Real, Σ}    

-} 

type Ctx = [(Var, Base)]

inferSig :: Neg -> Σ 
inferSig = infN []
  where infP  :: Ctx -> Pos -> Σ
        infN  :: Ctx -> Neg -> Σ
        infA  :: Ctx -> Atom.Atom -> Σ
        infT  :: Ctx -> Term -> (Base, Σ)
        infT' :: Ctx -> Term -> Base -> Σ
        infA ctx (Atom.Rel p ts) 
          | (Pred.name p == "=" || Pred.name p == "<") && length ts == 2 =
            let sig1 = infT' ctx (ts !! 0) Sort.Real 
                sig2 = infT' ctx (ts !! 1) Sort.Real
            in Sig.join sig1 sig2
          | otherwise =
            let (σs, sigs) = unzip $ map (infT ctx) ts
            in Sig.insert p (Sort.Rel σs) (Sig.joins sigs) 
        infT ctx (Var x) = case lookup x ctx of 
          Nothing -> error "term not closed"
          Just σ -> (σ, Sig.empty) 
        infT ctx (Fn f ts) = 
          let (σs, sigs) = unzip $ map (infT ctx) ts in
          (Sort.Real, Sig.insert f (Sort.Fun σs Sort.Real) (Sig.joins sigs))
        infT _ _ = __IMPOSSIBLE__ 
        infT' ctx (Var x) σ = case lookup x ctx of 
          Nothing -> error "term not closed"
          Just σ' -> if σ == σ' then Sig.empty else 
                      error $ "conflicting sorts for var: " ++ show x
        infT' ctx (Fn f ts) σ = 
          let (σs, sigs) = unzip $ map (infT ctx) ts in
          Sig.insert f (Sort.Fun σs σ) (Sig.joins sigs)
        infT' _ _ _ = 
          __IMPOSSIBLE__
        infN ctx f = case f of
          NAtom a -> infA ctx a
          And p q -> Sig.join (infN ctx p) (infN ctx q)
          Top -> Sig.empty
          Imp p q -> Sig.join (infP ctx p) (infN ctx q)
          Iff p q -> Sig.join (infN ctx p) (infN ctx q)
          All x σ p -> infN ((x, σ) : ctx) p
          Up p -> infP ctx p
          Box _ _ -> error "Unimplemented" 
        infP ctx f = case f of
          PAtom a -> infA ctx a
          Tensor p q -> Sig.join (infP ctx p) (infP ctx q)
          One -> Sig.empty
          Or p q -> Sig.join (infP ctx p) (infP ctx q)
          Bot -> Sig.empty
          Ex x σ p -> infP ((x, σ) : ctx) p
          Down p -> infN ctx p
          Diamond _ _ -> error "Unimplemented" 

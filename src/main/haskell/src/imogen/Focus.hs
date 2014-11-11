
{- | Focusing

-- @ Calculus

Positive formulas P ::= p⁺ | P ⊗ P | 1 | P ⊕ P | 0 | ◇ P | ∃ x. P | ↓ N 
Negative formulas N ::= p⁻ | N & N | ⊤ | P ⊃ N | □ N | ∀ x. P | ↑ P 
Constraints       ψ ::= ⊤ | ψ ∧ ψ | ∀ e. edge(e) ⊃ ψ | path(π) 

-- @@ Right Inversion

In right inversion, we input a labeled proposition for the consequent.
We return 

  1) The n stable sequents that are the leaves of the tree
  2) A constraint with n holes, one for each leaf sequent
  3) A list of the parameters generated during the ∀-R and ∃-L rules
  4) A list of edges generated during the □-R and ◇-L rules

rinvert :: Cons -> ([(Ants, Cons)], Ψ, [Param], [Edge])


  Ψ | · ; Γ ⊢ · ; p⁻ @ π           Ψ1 | Γ ⊢ A @ π ; ·    Ψ2 | Γ ⊢ B @ π ; ·    
--------------------------        --------------------------------------------  
    Ψ | Γ ⊢ p⁻ @ π ; ·                    Ψ1 ∧ Ψ2 | Γ ⊢ A & B @ π ; ·                

  
                                  Ψ | Γ, A @ π ⊢ B @ π ; ·  
-------------------------      ---------------------------- 
 path(π) | Γ ⊢ ⊤ @ π ; ·           Ψ | Γ ⊢ A ⊃ B @ π ; ·    


   Ψ | Γ ⊢ A @ π · e ; ·                            Ψ | Γ ⊢ A(a) @ π; ·         
--------------------------------------[e]     ----------------------------------[a] 
  ∀e. edge(e) ⊃ Ψ | Γ ⊢ □ A @ π ; ·           ∀ x. Ψ | Γ ⊢ ∀ x. A(x) @ π ; ·  


   Ψ | · ; Γ ⊢ · ; A
------------------------
   Ψ | Γ ⊢ ↑ A ; · 

-- @@ Left Inversion

In left inversion, we input the antecedents and consequent.  We
return the same stuff as in right inversion.


   Ψ | Δ, p⁺ @ π ; Γ ⊢ · ; γ               Ψ | Δ ; Γ, A @ π, B @ π ⊢ · ; γ    
--------------------------------         -----------------------------------  
  Ψ | Δ ; Γ, p⁺ @ π ⊢ · ; γ                   Ψ | Γ, A ⊗ B @ π ⊢ · ; γ        


     Ψ | Δ ; Γ ⊢ · ; γ           
---------------------------          --------------------------------  
  Ψ | Δ ; Γ, 1 @ π ⊢ · ; γ             path(π) | Γ, 0 @ π ⊢ · ; γ      


  Ψ1 | Δ ; Γ, A @ π ⊢ · ; γ      Ψ2 | Δ ; Γ, B @ π ⊢ · ; γ 
------------------------------------------------------------
              Ψ1 ∧ Ψ2 | Γ, A ⊕ B @ π ⊢ · ; γ


  Ψ | Δ ; Γ, A @ π · e ⊢ · ; γ                  Ψ | Δ ; Γ, A(a) @ π ⊢ · ; γ            
-------------------------------------[e]    -------------------------------------[a]   
  Ψ | Δ ; Γ, ◇ A @ π ⊢ · ; γ                  Ψ | Δ ; Γ, ∃ x. A(x) @ π ⊢ · ; γ        


  Ψ | Δ, A @ π ; Γ ⊢ · ; γ 
--------------------------------
  Ψ | Δ ; Γ, ↓ A @ π ⊢ · ; γ 

-- @@ Stable 

The leaves of the inversion tree are stable sequents.  

   Ψ | Δ ⊢ γ               Ψ | Δ ⊢ [γ]          Ψ | Δ ; [γ] ⊢ γ'   
--------------------    -----------------    ----------------------
 Ψ | Δ ; · ⊢ · ; γ         Ψ | Δ ⊢ γ            Ψ | Δ, γ ⊢ γ'      

-- @@ Right Focus

In focusing, we input a labeled proposition for the formula in focus.  We return
the same stuff we returned for inversion.


                                           Ψ1 | Γ1 ⊢ [A @ π]     Ψ1 | Γ2 ⊢ [B @ π]   
---------------------------------        -------------------------------------------- 
   path(π) | p @ π ⊢ [p⁺ @ π]                 Ψ1 ∧ Ψ2 | Γ1, Γ2 ⊢ [A ⊗ B @ π]            


                                    Ψ | Γ ⊢ [A @ π]     
--------------------------       ---------------------- 
  path(π) | · ⊢ [1 @ π]            Ψ | Γ ⊢ [A ⊕ B @ π]  


                            Ψ | Γ ⊢ [A @ π ⋆ π']         
 No rule for 0       ------------------------------------
                       Ψ ∧ path(π ⋆ π') | Γ ⊢ [◇ A @ π]  


   Ψ | Γ ⊢ [A(t) @ π]                 Ψ | Γ ⊢ A @ π ; ·      
-----------------------------      --------------------------  
  Ψ | Γ ⊢ [∃ x. A(x) @ π]           Ψ | Γ ⊢ [↓ A @ π]       


-- @@ Left Focus

-------------------------------------
  path(π) | · ; [p⁻ @ π] ⊢ p⁻ @ π 

   Ψ | Γ ; [A @ π] ⊢ γ  
-----------------------------    
  Ψ | Γ ; [A & B @ π] ⊢ γ 


 No rule for ⊤


   Ψ1 | Γ ; [B @ π] ⊢ γ    Ψ2 | · ⊢ [A @ π]
------------------------------------------------
      Ψ1 ∧ Ψ2 | Γ ; [A ⊃ B @ π] ⊢ γ 


        Ψ | Γ ; [A @ π ⋆ π'] ⊢ γ  
---------------------------------------    
  path(π ⋆ π') ∧ Ψ | Γ ; [□ A @ π] ⊢ γ 


    Ψ | Γ ; [A(t) @ π] ⊢ γ  
---------------------------------
  Ψ | Γ ; [∀x. A(x) @ π] ⊢ γ 


   Ψ | Γ ; A @ π ⊢ γ  
-----------------------------    
  Ψ | Γ ; [↑ A @ π] ⊢ γ 

-}

-- @ Pragmas

{-# OPTIONS_GHC -fno-warn-name-shadowing #-} 
{-# LANGUAGE CPP, Rank2Types, MultiParamTypeClasses, FunctionalDependencies #-} 

-- @ Signature

module Imogen.Focus 
  ( focus
  , Foci(..)
  , Class
  ) 
where

-- @ Imports

#include "../undefined.h" 

import Imogen.Util.Prelude hiding (id)
import qualified Control.Monad.State as State
import Control.Monad.State (StateT)
import qualified Control.Monad.Reader as Reader
import Control.Monad.Reader (ReaderT)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
--import Debug.Trace (trace)
import qualified Imogen.Ants as Ants
import Imogen.Ants (Ants)
import qualified Imogen.Atom as Atom
import Imogen.Atom (Atom(..))
import qualified Imogen.Cons as Cons
import qualified Imogen.Constr as C
import Imogen.Constr (Ψ, (∧), (⊤), (⊃))
import qualified Imogen.Ctx as Ctx
import Imogen.Ctx (Γ)
import qualified Imogen.Func as Func
import qualified Imogen.Global as Global
import Imogen.Global (Global)
import qualified Imogen.Modal as Modal
import Imogen.Modal (HasMode, World)
import qualified Imogen.Param as Param
import Imogen.Param (Param, Params, Petrify(..))
import qualified Imogen.Path as Path
import Imogen.Path (Path, PathElem(..), Atoms)
import qualified Imogen.Pred as Pred
import qualified Imogen.PFormula as F
import Imogen.PFormula (Pos(..), Neg(..))
import qualified Imogen.Print as Print
import Imogen.Print (Print, pPrint)
import qualified Imogen.Rule as Rule
import qualified Imogen.Sig as Sig
import qualified Imogen.Seq as Seq
import qualified Imogen.Sort as Sort
import Imogen.Subst (Apply, (↦), (✴))
import qualified Imogen.Term as Term
import Imogen.Term (Term(..))
import qualified Imogen.Util.Fresh as Fresh
import qualified Imogen.Util.Log as Log
import Imogen.Util.Log (Log)
import qualified Imogen.Util.Monad as M
import qualified Imogen.Util.Print as PP
import qualified Imogen.Util.Set as Set
import Imogen.Util.Set ((∅), (∪))
import qualified Imogen.Var as Var
import Imogen.Var (Var, Vars)

-- @ Class 

class (Rule.Class s m, HasMode s) => Class s m | m -> s where

-- @ Focusing sequents and inference rules

{- 
Focusing sequents differ from sequents used during proof search since
they store entire subformulas.  Search sequents only store names.
-} 

data Left' = Latom Atom World
           | Lneg Neg World

instance Apply Left' where
  Latom a w ✴ θ = Latom (a ✴ θ) (w ✴ θ)
  Lneg a w ✴ θ = Lneg (a ✴ θ) (w ✴ θ)

instance Petrify Left' where
  petrify ρ (Latom a w) = Latom (petrify ρ a) (petrify ρ w)
  petrify ρ (Lneg n w) = Lneg (petrify ρ n) (petrify ρ w)

type Left = (Path, Left')

data Right' = Ratom Atom World
            | Rpos Pos World
            | Ξ

instance Apply Right' where
  Ratom a w ✴ θ = Ratom (a ✴ θ) (w ✴ θ)
  Rpos a w ✴ θ = Rpos (a ✴ θ) (w ✴ θ)
  Ξ ✴ _ = Ξ

instance Petrify Right' where
  petrify ρ (Ratom a w) = Ratom (petrify ρ a) (petrify ρ w)
  petrify ρ (Rpos n w) = Rpos (petrify ρ n) (petrify ρ w)
  petrify _ Ξ = Ξ

type Right = (Path, Right')

data Seq = Seq Ψ [Left] Right 

data RSeq = RSeq [Left] Right

instance Petrify RSeq where
  petrify ρ (RSeq ls r) = RSeq (petrify ρ ls) (petrify ρ r)

instance Apply RSeq where
  RSeq ants cons ✴ θ = RSeq (ants ✴ θ) (cons ✴ θ)

data Rule = Rule { params :: Set Param
                 , constrs :: Ψ 
                 , hyps :: [RSeq]
                 , concl :: RSeq
                 }

-- @ Conversions

{- 
Conversions between the Focusing rules and sequents and the search
rules and sequents.
-} 

antsAnts :: Class s m => [Left] -> m Ants
antsAnts ד = mapM mkAnt ד >>= return . Ants.fromList
 where 
  mkAnt (π, f) = do 
    p <- Path.label π
    return $ case f of 
     Latom a w -> Modal.Rel p (Atom.args a) w
     Lneg (NAtom a) w -> Modal.Rel p (Atom.args a) w
     Lneg f' w -> Modal.Rel p (map Var (Set.toList $ Var.free f') 
                           ++ map Param (Set.toList $ Param.params f')) w

{- 
Note that because the globals are put into the state after the
focusing ends, we have to manually remove global antecedents from the
initial sequents.  While this is a bit painful, I don't see another
way without moving the Globals out of the State and using a Global
parameter everywhere.
-} 

rseqRSeq :: Class s m => Ants -> RSeq -> m Rule.RSeq
rseqRSeq glob (RSeq ד (π, γ)) = do 
  l <- Path.label π
  γ' <- return $ case γ of 
    Ξ -> Cons.X
    Ratom a w -> Cons.Rel $ Modal.Rel l (Atom.args a) w
    Rpos (PAtom a) w -> Cons.Rel $ Modal.Rel l (Atom.args a) w
    Rpos p w -> Cons.Rel $ Modal.Rel l (map Var (Set.toList $ Var.free p) 
                                ++ map Param (Set.toList $ Param.params p)) w
  ד' <- antsAnts ד >>= return . flip Ants.difference glob
  return $ Rule.RSeq ד' γ'

seqSeq :: Class s m => Ants -> Seq -> StateT Γ m Seq.Seq
seqSeq glob (Seq ψ ד γ) = do 
  Rule.RSeq ד' γ' <- M.lift $ rseqRSeq glob (RSeq ד γ)
  ctx <- State.get
  M.lift $ Seq.new ctx ψ ד' γ'

ruleRule :: Class s m => Ants -> Rule -> StateT Γ m Rule.Rule
ruleRule glob (Rule ρ ψ ζ δ) = do
  ζ' <- M.lift $ mapM (rseqRSeq glob) ζ
  δ' <- M.lift $ rseqRSeq glob δ
  ctx <- State.get
  M.lift $ Rule.new ζ' δ' ρ ψ ctx

-- @ Monads

-- @@ Both phases

{- 
The state we maintain across focusing boundaries includes the context
of variables and parameters.  This assumes that bound variables have
been uniquely renamed before focusing begins.
-} 

type GState = Γ 

evalGState :: Monad m => StateT GState m a -> m a
evalGState = flip State.evalStateT Ctx.empty

type Glob m = StateT GState m

-- @@ Inversion 

{- 
The state we maintain during a single inversion phase consists of the

 1) the parameters generated during that particular phase.  
 2) a counter for the constraint holes.

-}

type InvState = (Set Param, Int)

runInvState :: StateT InvState m a -> m (a, InvState)
runInvState = flip State.runStateT ((∅), 0)

type Inv m = StateT InvState (Glob m)

-- @@ Focusing

{-
Focusing carries the black and white atoms along for generating
initial sequents.
-} 

type Foc m = ReaderT Atoms (Glob m)

-- @ Inversion

-- invertR implements the judgement Γ ⊢ A.  If A needs to be decomposed,
-- we do it.  If not, we switch to invertL to decompose Γ.

pathQ :: World -> Ψ
pathQ = C.Atom . Modal.mkPath

edgeQ :: World -> Ψ
edgeQ = C.Atom . Modal.mkEdge

invertR :: Class s m => [Left] -> [(Path, Pos, World)] -> (Path, Neg, World) -> Inv m ([RSeq], Ψ)
invertR א πs (π, f, w) = do
  case f of 
    NAtom a -> invertL א πs (π, Ratom a w)
    And p q -> do 
      (l1, ψ1) <- invertR א πs (PAndL : π, p, w)
      (l2, ψ2) <- invertR א πs (PAndR : π, q, w)
      return (l1 ++ l2, ψ1 ∧ ψ2)
    Top -> return ([], pathQ w)
    Imp p q -> invertR א ((PImpL : π, p, w) : πs) (PImpR : π, q, w)
    Iff p q -> do 
      (l1, ψ1) <- invertR ((PIffL : π, Lneg p w) : א) πs (PIffR : π, q, w)
      (l2, ψ2) <- invertR ((PIffR : π, Lneg q w) : א) πs (PIffL : π, p, w)
      return $ (l1 ++ l2, ψ1 ∧ ψ2)
    All α σ p -> do
      -- Make a fresh parameter
      ç <- M.lift2 $ Fresh.fresh "ç"
      -- Add the new parameter to the generated parameter set
      State.modify (\(ρ, n) -> (Set.insert ç ρ, n))
      -- Add the sort of the new parameter
      M.lift2 $ Sig.modify (Sig.insert ç $ Sort.Fun [] σ)
      let θ = α ↦ Param ç
          p' = p ✴ θ
      invertR א πs (PAll : π, p', w) 
    Up p -> invertL א πs (PUp : π, Rpos p w)
    Box t p -> do
      -- Make a fresh edge parameter
      e :: Param <- M.lift2 $ Fresh.fresh "e"
      -- Add the new parameter to the generated parameters and the generated world sets
      State.modify (\(ρ, n) -> (Set.insert e ρ, n))
      -- Add the sort of the new parameter
      M.lift2 $ Sig.modify (Sig.insert e $ Sort.Fun [] Sort.MWorld)
      -- Get a new variable to stand for e
      x :: Var <- M.lift2 $ Fresh.fresh "x"
      (l, ψ) <- invertR א πs (PBox t : π, p, Modal.extend w e) 
      let ψ' = Param.thaw (Set.singleton e) ψ 
      return (l, C.All x Sort.MWorld (edgeQ x ⊃ ψ'))

-- invertL implements the judgment Γ ⊢ s-.  If any rules apply to Γ, it
-- applies them.

invertL :: Class s m => [Left] -> [(Path, Pos, World)] -> Right -> Inv m ([RSeq], Ψ)
-- If there are no unstable antecedents, we have a stable sequent.
invertL א [] r = do
  -- Get next hole index
  (_, n) <- State.get 
  -- Update the index
  State.modify (\(b, n) -> (b, n+1))
  -- Return the hole
  return ([RSeq א r], C.Hole n)
-- Otherwise, decompose the first unstable antecedent.
invertL א ((π, f, w) : πs) r = 
  case f of 
    -- An atom is stable
    PAtom a -> invertL ((π, Latom a w) : א) πs r
    Down p -> invertL ((PDown : π, Lneg p w) : א) πs r
    Tensor p q -> invertL א ((PTensorL : π, p, w) : (PTensorR : π, q, w) : πs) r
    One -> invertL א πs r
    Or p q -> do
      (seqs1, ψ1) <- invertL א ((POrL : π, p, w) : πs) r
      (seqs2, ψ2) <- invertL א ((POrR : π, q, w) : πs) r
      return (seqs1 ++ seqs2, ψ1 ∧ ψ2)
    Bot -> return ([], pathQ w)
    Ex x σ p -> do 
      -- Generate a fresh parameter
      ç <- M.lift2 $ Fresh.fresh "ç"
      -- Add the new parameter to the generated parameter set
      State.modify (\(ρ, n) -> (Set.insert ç ρ, n))
      -- Add the sort to the context
      M.lift2 $ Sig.modify (Sig.insert ç (Sort.Fun [] σ))
      let θ = x ↦ Param ç
          p' = p ✴ θ
      invertL א ((PEx : π, p', w) : πs) r
    Diamond t p -> do
      -- Make a fresh parameter
      e <- M.lift2 $ Fresh.fresh "e"
      -- Add the new parameter to the generated parameter and world sets
      State.modify (\(ρ, n) -> (Set.insert e ρ, n))
      -- Add the sort of the new parameter
      M.lift2 $ Sig.modify (Sig.insert e $ Sort.Fun [] Sort.MWorld)
      -- Get a new variable to stand for e
      x :: Var <- M.lift2 $ Fresh.fresh "x"
      (l, ψ) <- invertL א ((PDiamond t : π, p, Modal.extend w e) : πs) r
      let ψ' = Param.thaw (Set.singleton e) ψ 
      return (l, C.All x Sort.MWorld (edgeQ x ⊃ ψ'))

{- 
Note that we don't yet implement the atomic rule

    s- ⊑ s0-      
 ---------------- 
  Γ; [s-] ⊢ s0-   

correctly.  We instead implement

 ---------------- 
  Γ; [s-] ⊢ s-   

since all of the theories besides bunched implication use the rule

    s- ≡ s0-      
 ---------------- 
  Γ; [s-] ⊢ s0-   

which is equivalent to the simplified rule we implement.  When we finally
decide to get dirty with BI, we'll revisit this.
-} 

legal :: (Vars a, Params a) => Γ -> a -> Bool
legal ctx f = Set.all (flip Ctx.member ctx) (Var.free f)

-- @ Focusing

{- 
Atomic right focus

The difficulty here is that we must unify atoms from disjoint parts of the formula
to make an initial sequent.  Since in general the sorts for the variables of other
parts of the formula aren't known, we need to gather them before the focusing
begins, and plop them in the State of the focusing algorithm.

XXX FIXME: Check to be sure the atom occurs negatively as well.
-} 

atomicR :: Class s m => Path -> Atom -> World -> Foc m Rule
atomicR π a@(Rel _ _) w = do 
  sig <- M.lift2 Sig.get
  ctx <- M.lift Ctx.get
  if legal ctx a then return () 
    else do Log.emergencyM' "Focus.atomicR" $ PP.text "Bad context:" <+> pPrint sig ctx (a, ctx)
            __IMPOSSIBLE__ 
  if Atom.isConstr a 
    then return $ Rule { params = (∅) 
                       , constrs = C.Atom a
                       , hyps = [] 
                       , concl = RSeq [] (π, Rpos (PAtom a) w)
                       } 
    else return $ Rule 
                  { params = (∅)
                  , constrs = pathQ w
                  , hyps = []
                  , concl = (RSeq [(π, Latom a w)] (π, Rpos (PAtom a) w))
                  } 

-- | Right focus

focusR :: Class s m => Right -> Foc m [Rule]
focusR (π, f) = case f of 
  -- No right focusing on negative atoms
  Ratom _ _ -> return []
  Rpos f' w -> do
    case f' of 
      PAtom a -> atomicR π a w >>= return . list
      Tensor p q -> do
        seqs1 <- focusR (PTensorL : π, Rpos p w)
        seqs2 <- focusR (PTensorR : π, Rpos q w) 
        return $ [merge sq1 sq2 | sq1 <- seqs1, sq2 <- seqs2]
          where merge (Rule ρ1 ψ1 hyps1 (RSeq ants1 _))
                      (Rule ρ2 ψ2 hyps2 (RSeq ants2 _)) =
                       Rule (ρ1 ∪ ρ2) (ψ1 ∧ ψ2) 
                        (hyps1 ++ hyps2) (RSeq (ants1 ++ ants2) (π, f)) 
      One -> return [ Rule (∅) (pathQ w) [] (RSeq [] (π, f)) ] 
      Or p q -> do
        seqs1 <- focusR (POrL : π, Rpos p w)
        seqs2 <- focusR (POrR : π, Rpos q w)
        let mapfn (Rule ρ ψ ζ (RSeq ד _)) = Rule ρ ψ ζ (RSeq ד (π, f))
        return $ map mapfn (seqs1 ++ seqs2)
      Bot -> return []
      Ex x σ p -> do
        -- Add the variable to the context
        M.lift $ State.modify (Ctx.insert x σ)
        rs <- focusR (PEx : π, Rpos p w)
        return $ map mapFn rs
          where mapFn (Rule ρ ψ ζ (RSeq ד _)) = Rule ρ ψ ζ (RSeq ד (π, f))
      Down n -> do
        ((hyps, ψ), (ρ, _)) <- M.lift $ runInvState (invertR [] [] (PDown : π, n, w))
        return $ [ Rule ρ ψ hyps (RSeq [] (π, f)) ]
      Diamond t p -> do
        -- Make a fresh variable
        ω <- M.lift2 $ Fresh.fresh "ω"
        -- Add variable to the context
        M.lift $ State.modify (Ctx.insert ω Sort.MWorld)
        let ω' = Modal.extend w ω
        rs <- focusR (PDiamond t : π, Rpos p ω')
        let mapFn (Rule ρ ψ ζ (RSeq ד _)) = Rule ρ (ψ ∧ pathQ ω') ζ (RSeq ד (π, f))
        return $ map mapFn rs
  Ξ -> return []

-- | Atomic left focus

atomicL :: Class s m => Path -> Atom -> World -> Foc m Rule
atomicL π a@(Rel _ _) w = 
  return $ Rule { params = (∅)
                , constrs = (pathQ w)
                , hyps = []
                , concl = RSeq [(π, Latom a w)] (π, Rpos (PAtom a) w)
                } 

{- |
Left focus

focusL implements Γ ; [A] ⊢ s-
-} 
focusL :: Class s m => Left -> Foc m [Rule]
focusL (_, Latom _ _) = return []
focusL (π, Lneg f w) = do
  case f of 
    NAtom a -> atomicL π a w >>= return . list
    And p q -> do
      rs1 <- focusL (πp, Lneg p w)
      rs2 <- focusL (πq, Lneg q w)
      return $ map f1 rs1 ++ map f2 rs2
        where πp = PAndL : π
              πq = PAndR : π
              f1 (Rule ρ ψ ζ (RSeq ד γ)) = 
                let ד' = (π, Lneg f w) : filter (\(π', _) -> π' /= πp) ד in
                Rule ρ ψ ζ (RSeq ד' γ)
              f2 (Rule ρ ψ ζ (RSeq ד γ)) = 
                let ד' = (π, Lneg f w) : filter (\(π', _) -> π' /= πq) ד in
                Rule ρ ψ ζ (RSeq ד' γ) 
    Top -> return [] 
    Imp p q -> do
      rs1 <- focusL (πq, Lneg q w)
      rs2 <- focusR (πp, Rpos p w)
      return $ [merge' r1 r2 | r1 <- rs1, r2 <- rs2]
        where πp = PImpL : π
              πq = PImpR : π
              merge' (Rule ρ1 ψ1 ζ1 (RSeq ד1 γ1)) (Rule ρ2 ψ2 ζ2 (RSeq ד2 _γ2)) = 
                let ד = (π, Lneg f w) : filter (\(π', _) -> π' /= πq) (ד1 ++ ד2) in
                Rule (ρ1 ∪ ρ2) (ψ1 ∧ ψ2) (ζ1 ++ ζ2) (RSeq ד γ1)
    Iff p q -> do
      let πp = PIffL : π
          πq = PIffR : π
      rs1 <- focusL (πq, Lneg q w)
      ((ζ1, ψ1), (rparams1, n)) <- M.lift $ runInvState (invertR [] [] (πp, p, w)) 
      rs2 <- focusL (πp, Lneg p w)
      ((ζ2, ψ2), (rparams2, _)) <- M.lift $ State.runStateT (invertR [] [] (πq, q, w)) ((∅), n)
      let 
          mapfn1 (Rule ρ ψ ζ (RSeq ד γ)) = Rule (ρ ∪ rparams1) (ψ ∧ ψ1) (ζ ++ ζ1) (RSeq ד' γ) 
            where ד' = (π, Lneg f w) : filter (\(π', _) -> π' /= πq) ד 
          mapfn2 (Rule ρ ψ ζ (RSeq ד γ)) = Rule (ρ ∪ rparams2) (ψ ∧ ψ2) (ζ ++ ζ2) (RSeq ד' γ) 
            where ד' = (π, Lneg f w) : filter (\(π', _) -> π' /= πp) ד 
      return $ map mapfn1 rs1 ++ map mapfn2 rs2 
    All x σ p -> do
      -- Add variable to the context
      M.lift $ State.modify (Ctx.insert x σ)
      rs <- focusL (πp, Lneg p w)
      return $ map mapFn rs
        where πp = PAll : π
              mapFn (Rule ρ ψ ζ (RSeq ד γ)) = 
                let ד' = (π, Lneg f w) : filter (\(π', _) -> π' /= πp) ד in
                Rule ρ ψ ζ (RSeq ד' γ)
    Up p -> do 
      ((hyps, ψ), (ρ, _)) <- M.lift $ runInvState (invertL [] [(PUp : π, p, w)] ([], Ξ))
      return $ [Rule ρ ψ hyps (RSeq [(π, Lneg f w)] ([], Ξ)) ]
    Box t p -> do
      -- Make a fresh variable
      ω <- M.lift2 $ Fresh.fresh "ω"
      let ω' = Modal.extend w ω
      -- Add variable to the context
      M.lift $ State.modify (Ctx.insert ω Sort.MWorld)
      rs <- focusL (PBox t : π, Lneg p ω')
      let πp = PBox t : π
          mapFn (Rule ρ ψ ζ (RSeq ד γ)) = 
            let ד' = (π, Lneg f w) : filter (\(π', _) -> π' /= πp) ד 
                ψ' = ψ ∧ pathQ ω'
            in Rule ρ ψ' ζ (RSeq ד' γ)
      return $ map mapFn rs

-- @ Top level rule generation

-- Initial stabilization phase.

stabilize :: Class s m => Neg -> Glob m ([RSeq], Ψ, Set Param)
stabilize n = do
  ((seqs, ψ), (ρ, _)) <- runInvState (invertR [] [] ([], n, Modal.ε))
  return (seqs, ψ, ρ)

-- Generate initial rules and sequents.

initial :: Class s m => RSeq -> Foc m ([Seq], [Rule])
initial (RSeq ד γ) = do 
  rs <- genRules (Right γ : map Left ד)
  return $ classify rs

-- Recursively generate inference rules.

genRules :: Class s m => [Either Left Right] -> Foc m [Rule]
genRules [] = return []
genRules (f:fs) = do
  rs1 <- foc f
  let ts = concat $ map newRuleTargets rs1
  rs2 <- genRules $ ts ++ fs
  return $ rs1 ++ rs2
    where --foc :: Class s m => Either Left Right -> m ([Rule], Γ)
          foc (Left l) = focusL l
          foc (Right r) = focusR r
          newRuleTargets :: Rule -> [Either Left Right]
          newRuleTargets = concat . map newSeqTargets . hyps 
          newSeqTargets :: RSeq -> [Either Left Right]
          newSeqTargets (RSeq ד γ) = Right γ : map Left ד 

-- If a rule has no premeses, it is an initial sequent.  Otherwise
-- it is an initial rule. 

classify :: [Rule] -> ([Seq], [Rule])
classify [] = ([], [])
classify (Rule ρ ψ [] (RSeq ד γ) : rest) 
  | ρ == (∅) = ((Seq ψ ד γ) : qs, rs)
  | otherwise = error "Generated parameters in sequent"
  where (qs, rs) = classify rest
classify (r : rest) = (qs, r:rs)
  where (qs, rs) = classify rest

-- @ Interface

data Foci = 
  Foci { _goal :: Seq.Seq
       , _iseqs :: [Seq.Seq]
       , _irules :: [Rule.Rule]
       , _global :: Global
       } 
  deriving Show

-- Note: each foci has a different Global structure. 
-- While we focus, we collect the path signatures.

focus :: forall s m. Class s m => Neg -> m [Foci]
--focus f = trace "Debug.focus" $ do
focus f = do
  -- Log.debugM "Focus.focus" "Entering focus"
  let ats = Path.atoms f 
  sig <- Sig.get
  (rseqs, ψ, ρ) <- evalGState $ stabilize f
  Log.debugM' "Focus.focus" $ PP.vcat [ PP.text "Atoms: " <+> pPrint sig Ctx.empty ats
                                      , PP.text "Stable: " <+> pPrint sig Ctx.empty rseqs ]
  -- Petrify the generated constants
  let fs = iterate Func.next Func.start
      pets = zip (Set.toList ρ) fs
      pet = Map.fromList pets
      rseqs' = Param.petrify pet rseqs
      ψ' = Param.petrify pet ψ
      -- ωs' = Param.petrify pet ωs
      -- Add the petrified constants to the signature    
      ins (ç, func) s = case Sig.lookup ç s of
        Nothing -> __IMPOSSIBLE__ 
        Just σ -> Sig.insert func σ s
  Sig.modify (\s -> foldr ins s pets) 
  -- focus on a single stable sequent
  let init' :: RSeq -> StateT GState m Foci
      init' rseq@(RSeq ד γ) = do 
        -- convert the global antecedents
        ד' <- M.lift $ (antsAnts ד :: m Ants)
        (qs, rs) <- Reader.runReaderT (initial rseq) ats
        qs' <- mapM (seqSeq ד') qs 
        rs' <- mapM (ruleRule ד') rs
        goal <- seqSeq ד' (Seq (⊤) [] γ)
        -- Remember to add the contradictory initial sequent!
        return $ Foci goal (Seq.contradictory : filterInits qs') rs' (Global.make ד' (C.simp ψ'))
  -- focus on all stable sequents
  State.evalStateT (mapM init' rseqs') Ctx.empty

-- Get rid of duplicate initial sequents.
filterInits :: [Seq.Seq] -> [Seq.Seq]
filterInits = List.nubBy filt
 where
  filt q1 q2 = Seq.constrs q1 == Seq.constrs q2 
            && Seq.ants q1 == Seq.ants q2 
            && Seq.cons q1 == Seq.cons q2 

-- @ Printing

instance Print Left' where
  pPrint sig ctx (Latom p w) = pPrint sig ctx p <+> PP.text "@" <+> pPrint sig ctx w
  pPrint sig ctx (Lneg n w) = pPrint sig ctx n <+> PP.text "@" <+> pPrint sig ctx w

instance Print Right' where
  pPrint sig ctx (Ratom n w) = pPrint sig ctx n <+> PP.text "@" <+> pPrint sig ctx w
  pPrint sig ctx (Rpos p w) = pPrint sig ctx p <+> PP.text "@" <+> pPrint sig ctx w
  pPrint _ _ Ξ = PP.text "Ξ"

instance Print Seq where
  pPrint sig ctx (Seq ψ ד (_, γ)) = 
    PP.hsep [pPrint sig ctx ψ, PP.text "|", ppList (map (pPrint sig ctx . snd) ד), 
             PP.text "⊢", pPrint sig ctx γ]

instance Print Foci where
  pPrint sig ctx (Foci goal qs rs gs) = 
    PP.vcat [ PP.hang (PP.text "Goal:") 2 (pPrint sig ctx goal)
            , PP.hang (PP.text "Globals:") 2 (pPrint sig ctx gs)
            , PP.hang (PP.text "Seqs:") 2 (pPrint sig ctx qs)
            , PP.hang (PP.text "Rules:") 2 (pPrint sig ctx rs)
            ] 

instance Print Rule where
  pPrint sig ctx (Rule ρ ψ δ ξ) = 
    PP.vcat [ ppList (map (pPrint sig ctx) δ)
            , PP.hsep [PP.text "----------------------", pPrint sig ctx ρ]
            , PP.hsep [pPrint sig ctx ψ, PP.text "|", pPrint sig ctx ξ]
            ] 

instance Print RSeq where
  pPrint sig ctx (RSeq ד (_, γ)) = PP.sep [ ppList (map (pPrint sig ctx . snd) ד)
                                  , PP.text "⊢", pPrint sig ctx γ]

ppList :: [PP.Doc] -> PP.Doc
ppList [] = PP.text "○" 
ppList l = PP.brackets (PP.cat (List.intersperse (PP.text ", ") l))

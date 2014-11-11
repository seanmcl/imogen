
-- | Focused inverse method prover

{-# LANGUAGE CPP, MultiParamTypeClasses, FlexibleInstances, Rank2Types, FlexibleContexts #-} 

-- @ Signature

module Imogen.Prover 
  ( -- * Provers 
    uninterpProve
  , dloProve
  , linearProve
  , modalProve
  , orderedProve
  , tptpProve
  , tptpProveClassical
    -- * Test\/Debug
  , startState
  )
where 

-- @ Imports

#include "../undefined.h" 

import Imogen.Util.Prelude hiding (init)
import qualified Control.Monad.State as State
import qualified Imogen.Class as Class
import qualified Imogen.Constr as Constr
import Imogen.Constr(HasSolver, Solver)
import qualified Imogen.Constr.Dlo as Dlo
import qualified Imogen.Ctx as Ctx
import qualified Imogen.Database as Db
import qualified Imogen.Focus as Focus
import Imogen.Focus (Foci(..))
import qualified Imogen.Formula as Formula
import Imogen.Formula (Formula)
import qualified Imogen.Global as Global 
import Imogen.Global (Global)
import qualified Imogen.Linear as LL
import qualified Imogen.Misc as Misc
import Imogen.Misc (Input(..), Output(..))
import qualified Imogen.Modal as Modal
import Imogen.Modal (Mode)
import qualified Imogen.Modal.Solver as MSolver
import qualified Imogen.Ordered as OL
import qualified Imogen.Param as Param
import Imogen.Param (Param)
import qualified Imogen.Parse as Parse
import qualified Imogen.Path as Path
import Imogen.Path (PathLabels)
import qualified Imogen.PFormula as F
import Imogen.PFormula (Neg)
import Imogen.Print (Print, pPrint)
import qualified Imogen.Proof as Proof
import qualified Imogen.Rename as Rename
import qualified Imogen.Rule as Rule
import Imogen.Rule (Rule)
import qualified Imogen.Seq as Seq
import Imogen.Seq(Seq)
import qualified Imogen.Sig as Sig
import Imogen.Sig (Σ)
import qualified Imogen.Uninterp as Uninterp
import qualified Imogen.Util.Counter as Counter
import Imogen.Util.Counter (Counter, HasCounter)
import qualified Imogen.Util.Fresh as Fresh
import Imogen.Util.Fresh (HasFresh)
import qualified Imogen.Util.Lex as Lex
import qualified Imogen.Util.List as List
import qualified Imogen.Util.Log as Log
import Imogen.Util.Log (Log)
import qualified Imogen.Util.Monad as M
import Imogen.Util.Monad (FileReader)
import qualified Imogen.Util.Print as PP
import qualified Imogen.Util.Parse as P
import Imogen.Util.Parse (Parser)
import qualified Imogen.Var as Var
import Imogen.Var (Var)


-- FIXME: Weird.  This gives an unused warning when it's in its correct alphabetical position.
import Imogen.Parse (Parse)


-- @ State

{- 
The state that will be carried around during the prover computation.
Note that the Global component must be updated for each stable sequent.
We want the globals in the prover state rather than in the database so
we can make new sequents in the prover state monad, rather than
needing to pass a reference to the globals or the database.
-} 

data ProverState = S { varCtr :: Var
                     , paramCtr :: Param
                     , seqCtr :: Counter Seq
                     , ruleCtr :: Counter Rule
                     , labels :: PathLabels
                     , signat :: Σ
                     , solver :: Solver
                     , global :: Global
                     , mode :: Mode
                     }
  deriving Show

instance PP.Print ProverState where
  pPrint = PP.text . show

startState :: Σ -> Solver -> Global -> Mode -> ProverState
startState = S (Var.start "X") (Param.start "ç") Counter.start Counter.start Path.emptyLabels

instance HasFresh Var ProverState where
  nextFresh name st = (x', st { varCtr = x' })
    where x' = Var.next name $ varCtr st

instance HasFresh Param ProverState where
  nextFresh name st = (p', st { paramCtr = p' })
    where p' = Param.next name $ paramCtr st

instance HasCounter Seq ProverState where
  nextCtr st = (n', st { seqCtr = n' })
    where n' = Counter.tickCtr $ seqCtr st

instance HasCounter Rule ProverState where
  nextCtr st = (m', st { ruleCtr = m' })
    where m' = Counter.tickCtr $ ruleCtr st

instance Sig.Has ProverState where
  fromState = signat
  toState sig s = s {signat = sig}

instance Modal.HasMode ProverState where
  state = mode

instance HasSolver ProverState where
  getSolver = solver

instance Global.Has ProverState where
  state = global

instance Path.HasLabels ProverState where
  state = labels

instance Log m => Class.UnifClass ProverState (StateT ProverState m) where

instance Log m => Seq.Class ProverState (StateT ProverState m) where

instance Log m => Rule.Class ProverState (StateT ProverState m) where

instance Log m => Focus.Class ProverState (StateT ProverState m) where

class (Log m, FileReader m) => Class m where

instance Class IO where

-- @ Prover

data Prover a = Prover { pid          :: String
                       , parser       :: Parser a
                       , srcTrans     :: Class m => a -> StateT ProverState m a
                       , encode       :: Class m => a -> StateT ProverState m Neg
                       , targetTrans  :: Class m => Neg -> StateT ProverState m Neg
                       , psolver      :: Constr.Solver
                       , pmode         :: Mode
                       , sigInfer     :: a -> Σ 
                       }

prove :: forall a m. Class m => Prover a -> Input -> m Output
prove prov inp = do
  str <- case inp of 
    String s -> return s
    File file -> M.readFile file
  let f :: a = Lex.makeParser (parser prov) str 
      sig = sigInfer prov f
  Log.debugM "Prover.prove" $ show (pid prov)
  Log.debugM' "Prover.prove" $ pPrint sig Ctx.empty inp
  let s = startState sig (psolver prov) Global.empty (pmode prov)
  State.evalStateT (proveM prov f) s

proveM :: Class m => Prover a -> a -> StateT ProverState m Output
proveM prov inp = do
  f1 <- srcTrans prov inp
  f2 <- encode prov f1
  f <- targetTrans prov f2
  sig <- Sig.get
  Log.debugM' "Prover.proveM" $ pPrint sig Ctx.empty f
  let labs = Path.labels f
  -- set the paths label.  We can't do this sooner since the
  -- linear translation must take place in the monad, and to 
  -- run the monad you need a value already in the monad...
  State.modify $ \s -> s { labels = labs }
  -- Rename the bound variables 
  f' <- Rename.doit f
  Log.infoM' "Prover.proveM" $ PP.vcat [ PP.text "Input  :" <+> pPrint sig Ctx.empty f
                                       , PP.text "Labels :" <+> PP.pPrint labs ]
  -- Focus to get the initial rules and sequents
  foci <- Focus.focus f'
  -- sig' <- Sig.get
  -- Log.infoM' "Prover.proveM" $ PP.text "Sig  :" <+> PP.pPrint sig'
  Log.infoM' "Prover.proveM" $ PP.hang (PP.text "Foci") 2 (pPrint sig Ctx.empty foci)
  -- Prove all stable sequents
  res <- mapM prove1 foci
  -- Return True ⇔ they're all Yes!
  return $ 
   if all Misc.isYes res then Yes Proof.dummy
   else if any Misc.isNo res then No
   else Maybe

-- | Prove a single stable sequent
prove1 :: Log m => Focus.Foci -> StateT ProverState m Output
prove1 foc@(Focus.Foci goal seqs rules glob) = do
  sig <- Sig.get
  Log.infoM' "Prover.prove1" $ pPrint sig Ctx.empty foc
  -- Update the globals
  State.modify (\s -> s { global = glob })
  -- Saturate the database
  Db.prove seqs rules goal

-- @ Uninterpreted functions

uninterpProver :: Prover Neg
uninterpProver = Prover "Uninterp" P.parser (return . id) (return . id) (return . id)
                   Constr.dummySolver Modal.None Uninterp.inferSig

-- | Prove an uninterpreted formula.
uninterpProve :: Class m => Input -> m Output
uninterpProve = prove uninterpProver

-- @ Modal logic

modalProver :: Mode -> Prover Neg
modalProver m = Prover "Modal" P.parser (return . id) (return . id) (return . id)
                  MSolver.solver m Uninterp.inferSig

-- | Prove a modal logic formula.
modalProve :: Class m => Mode -> Input -> m Output
modalProve m = prove (modalProver m)

-- @ Dlo

dloProver :: Prover Neg
dloProver = Prover "Dlo" P.parser (return . id) (return . id) (return . id)
              Dlo.solver Modal.None Dlo.inferSig

-- | Prove a formula in the dense linear order theory.
dloProve :: Class m => Input -> m Output
dloProve = prove dloProver

-- @ Linear logic

linearProver :: Prover LL.Neg
linearProver = Prover "Linear" P.parser (return . id) LL.translate (return . id) 
                 LL.solver Modal.None LL.inferSig

-- | Prove a linear logic formula.
linearProve :: Class m => Input -> m Output
linearProve = prove linearProver

-- @ Ordered logic

orderedProver :: Prover OL.Neg
orderedProver = Prover "Ordered" P.parser (return . id) OL.translate (return . id) 
                 OL.solver Modal.None OL.inferSig

-- | Prove an ordered logic formula.
orderedProve :: Class m => Input -> m Output
orderedProve = prove orderedProver

-- @ Tptp

tptpProver :: Prover Formula
tptpProver = Prover "Tptp" (fst <$> Parse.tptp) (return . id) (return . Formula.polarize) (return . id)
                Constr.dummySolver Modal.None Formula.inferSig

-- | Prove a formula in TPTP syntax.
tptpProve :: Class m => Input -> m Output
tptpProve = prove tptpProver

tptpCProver :: Prover Formula
tptpCProver = Prover "TptpClassical" (fst <$> Parse.tptp) (return . Formula.doubleNegate Formula.AGO) (return . Formula.polarize) (return . id)
                   Constr.dummySolver Modal.None Formula.inferSig

-- | Prove a formula in TPTP syntax, double negated for classic truth.
tptpProveClassical :: Class m => Input -> m Output
tptpProveClassical = prove tptpCProver

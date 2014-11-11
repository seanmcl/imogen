-- FIXME: DOC
-- @ Signature

module Imogen.Uninterp
  ( inferSig )
where

-- @ Imports

import Prelude
import qualified Data.Set as Set
import qualified Imogen.Func as Func
import qualified Imogen.Pred as Pred
import qualified Imogen.PFormula as F
import Imogen.PFormula (Neg)
import qualified Imogen.Sig as Sig
import qualified Imogen.Sort as Sort
import Imogen.Sig (Σ)

-- @ Sort inference

inferSig :: Neg -> Σ 
inferSig form = 
  let fs = Set.toList $ Func.arityFuncs form
      fs' = map (\(f, n) -> (Sig.Func f, Sort.Fun (replicate n Sort.U) Sort.U)) fs 
      ps = Set.toList $ Pred.arityPreds form
      ps' = map (\(p, n) -> (Sig.Pred p, Sort.Rel (replicate n Sort.U))) ps 
  in Sig.fromList $ fs' ++ ps'


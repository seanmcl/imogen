
-- | Pretty printing.

-- @ Pragmas

{-# LANGUAGE CPP, OverlappingInstances #-} 

-- @ Signature

module Imogen.Print 
  ( Print(..) )
where

-- @ Imports

#include "../undefined.h" 

import Imogen.Util.Prelude 
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Imogen.Ants as Ants
import Imogen.Ants (Ants)
import Imogen.Atom (Atom(Rel))
import qualified Imogen.Cons as Cons
import Imogen.Cons (Cons)
import qualified Imogen.Constr as C
import Imogen.Constr (Solver)
import qualified Imogen.CSubst as CSubst
import qualified Imogen.Ctx as Ctx
import Imogen.Ctx (Γ)
import qualified Imogen.Formula as Formula
import Imogen.Formula (Formula)
import qualified Imogen.Func as Func
import qualified Imogen.Global as Global
import Imogen.Global (Global)
import Imogen.Linear.Head (Head)
import qualified Imogen.Linear.Frame as LFrame
import qualified Imogen.Linear.World as LWorld
import qualified Imogen.Ordered.Frame as OFrame
import qualified Imogen.Ordered.World as OWorld
import qualified Imogen.Misc as Misc
import qualified Imogen.Modal as Modal
import Imogen.Modal (Mode)
import Imogen.Param (Param)
import qualified Imogen.Path as Path
import Imogen.Path (PathAtom(Π))
import qualified Imogen.PFormula as F
import Imogen.PFormula (Pos, Neg)
import qualified Imogen.Pred as Pred
import Imogen.Pred (Pred)
import qualified Imogen.Subst as Subst
import qualified Imogen.Sig as Sig
import Imogen.Sig (Σ)
import qualified Imogen.Sort as Sort
import qualified Imogen.Term as T
import Imogen.Term (Term)
import qualified Imogen.Util.Debug as Debug
import qualified Imogen.Util.Parse as P
import qualified Imogen.Util.Print as PP
import Imogen.Util.Print(Doc)
import qualified Imogen.Util.Seq as Sequence
import Imogen.Var(Var)

-- @ Class

{- | 
Just as unification and normalization can be different from sort to
sort, so can printing.  Since a is generally an abstract type that has
been @encoded@ into a term, we need a way to print it in its natural
form.  For instance, the term

> Fn "+" [x, y, z] 

might print as +(x, y, z) at the uninterpreted function sort, but as x
+ y + z at an arithmetic sort.
-} 
class Print a where
  pPrint :: Σ -> Γ -> a -> PP.Doc
  pPrintCtx :: (MonadState s m, Ctx.Has s) => Σ -> a -> m PP.Doc
  pPrintCtx sig x = do
    ctx <- Ctx.get
    return $ pPrint sig ctx x
  pPrintSigCtx :: (MonadState s m, Sig.Has s, Ctx.Has s) => a -> m PP.Doc
  pPrintSigCtx x = do
    sig <- Sig.get
    ctx <- Ctx.get
    return $ pPrint sig ctx x    

instance Print Bool where
  pPrint _ _ b = PP.pPrint b

instance Print a => Print (Maybe a) where
  pPrint _ _ Nothing = PP.text "Nothing"
  pPrint sig ctx (Just x) = PP.text "Just" <+> pPrint sig ctx x

instance Print a => Print [a] where
  pPrint sig ctx = PP.list . map (pPrint sig ctx)

instance Print a => Print (Sequence.Seq a) where
  pPrint sig ctx = PP.list . map (pPrint sig ctx) . Sequence.toList

instance Print a => Print (Set a) where
  pPrint sig ctx = PP.set . map (pPrint sig ctx) . Set.toList

instance (Print a, Print b) => Print (Map a b) where
  pPrint sig ctx = PP.set . map (pPrint sig ctx) . Map.toList

instance (Print a, Print b) => Print (Either a b) where
  pPrint sig ctx (Left a) = PP.text "Left" <+> pPrint sig ctx a
  pPrint sig ctx (Right b) = PP.text "Right" <+> pPrint sig ctx b

instance (Print a, Print b) => Print (a, b) where
  pPrint sig ctx (a, b) = PP.tuple [pPrint sig ctx a, pPrint sig ctx b]

instance (Print a, Print b, Print c) => Print (a, b, c) where
  pPrint sig ctx (a, b, c) = PP.tuple [pPrint sig ctx a, pPrint sig ctx b, pPrint sig ctx c]

instance (Print a, Print b, Print c, Print d) => Print (a, b, c, d) where
  pPrint sig ctx (a, b, c, d) = PP.tuple [pPrint sig ctx a, pPrint sig ctx b, pPrint sig ctx c, pPrint sig ctx d]

instance (Print a, Print b, Print c, Print d, Print e) => Print (a, b, c, d, e) where
  pPrint sig ctx (a, b, c, d, e) = PP.tuple [pPrint sig ctx a, pPrint sig ctx b, pPrint sig ctx c, pPrint sig ctx d, pPrint sig ctx e]

instance (Print a, Print b, Print c, Print d, Print e, Print f) => Print (a, b, c, d, e, f) where
  pPrint sig ctx (a, b, c, d, e, f) = PP.tuple [pPrint sig ctx a, pPrint sig ctx b, pPrint sig ctx c, pPrint sig ctx d, pPrint sig ctx e, pPrint sig ctx f]

-- @ Terms

instance Print Var where
  pPrint _ _ = PP.pPrint

instance Print Param where
  pPrint _ _ = PP.pPrint

instance Print Term where
  pPrint sig ctx tm = case tm of 
    T.Var x -> case Ctx.lookup x ctx of 
      Nothing -> Debug.error $ PP.hsep [ PP.text "No sort for variable:", PP.pPrint x, PP.text "in context:", PP.pPrint ctx]
      Just σ -> print σ tm 
    T.Param c -> case Sig.lookup c sig of
      Nothing -> error $ "No sort for parameter: " ++ show c
      Just (Sort.Fun [] σ) -> print σ tm
      _ -> __IMPOSSIBLE__ 
    T.Fn f _ -> case Sig.lookup f sig of 
      Nothing -> error $ "No sort for function symbol: " ++ show tm
      Just (Sort.Fun _ σ) -> print σ tm
      _ -> __IMPOSSIBLE__ 

   where 

    print σ t = case σ of 
      Sort.Head -> PP.pPrint $ (T.decode t :: Head)
      Sort.LWorld -> PP.pPrint $ (T.decode t :: LWorld.World)
      Sort.LFrame -> PP.pPrint $ (T.decode t :: LFrame.Frame)
      Sort.OWorld -> PP.pPrint $ (T.decode t :: OWorld.World)
      Sort.OFrame -> PP.pPrint $ (T.decode t :: OFrame.Frame)
      Sort.Principal -> ppTerm t
      Sort.Int -> ppTerm t
      Sort.Real -> ppTerm t
      Sort.U -> ppTerm t
      Sort.MWorld -> PP.pPrint $ (T.decode t :: Modal.World)

    ppTerm = ppTerm' 0

    ppTerm' pr t = case t of
      T.Var x -> PP.pPrint x
      T.Param c -> PP.pPrint c
      T.Fn f [] -> PP.pPrint f
      T.Fn f [t1, t2] | elem (Func.name f) (map fst Misc.infixFuncs) ->  
        case lookup (Func.name f) Misc.infixFuncs of
          Nothing -> __IMPOSSIBLE__ 
          Just (n, P.AssocRight) -> 
            PP.paren (n < pr) (PP.sep [ppTerm' (n+1) t1, PP.pPrint f, ppTerm' n t2])
          Just (n, P.AssocLeft) -> 
            PP.paren (n < pr) (PP.sep [ppTerm' n t1, PP.pPrint f, ppTerm' (n+1) t2])
          Just (n, P.AssocNone) -> 
            PP.paren (n < pr) (PP.sep [ppTerm' n t1, PP.pPrint f, ppTerm' (n+1) t2])
      T.Fn f ts -> 
        PP.pPrint f <> PP.parens (PP.fcat $ 
            List.intersperse (PP.text ", ") (map PP.pPrint ts))

-- @ Atoms

instance Print Pred where
  pPrint _ _ = PP.pPrint

instance Print Atom where
  pPrint sig ctx a@(Rel p args)
   | null args = PP.pPrint p
   | otherwise =
      case Sig.lookup p sig of
        Nothing -> error $ "No sort for predicate: " ++ show p
          -- let σs = map getSort args in
          -- ppAtom p (map (uncurry ($)) (zip (map mkf σs) args))
        Just (Sort.Rel σs) | length σs == length args -> 
            ppAtom p $ map (pPrint sig ctx) args
        σ -> error' $ PP.text "Bad sort for predicate:" <+> PP.pPrint (p, σ, a)
   where 

    ppAtom, ppAtom' :: Pred -> [PP.Doc] -> PP.Doc
    ppAtom p' [s, t]
     | elem (Pred.name p') Misc.infixBinopStrings = PP.sep [s, PP.pPrint p', t]
     | otherwise = ppAtom' p' [s, t]
    ppAtom p' ts = ppAtom' p' ts

    ppAtom' p' ts = PP.pPrint p' <> PP.parens 
      (PP.fcat $ List.intersperse (PP.text ", ") ts)

    -- getSort t = case t of
    --   T.Var x -> case Ctx.lookup x ctx of
    --     Nothing -> error $ "No sort for var: " ++ show x
    --     Just σ -> σ
    --   T.Param c -> case Sig.lookup c sig of
    --     Nothing -> error $ "No sort for param: " ++ show c
    --     Just (Sort.Fun [] σ) -> σ
    --     _ -> __IMPOSSIBLE__ 
    --   T.Fn f _ -> case Sig.lookup f sig of
    --     Nothing -> error $ "No sort for func: " ++ show f
    --     Just (Sort.Fun _ σ) -> σ
    --     _ -> __IMPOSSIBLE__ 

-- @ Modal atoms

instance Print Modal.Atom where
  pPrint sig ctx (Modal.Rel p ts w) = pPrint sig ctx (Rel p ts) <+> ppWorld w
   where 
    ppWorld w' = PP.text "@" <+> PP.pPrint w'

instance Print Modal.World where
  pPrint _ _ = PP.pPrint

-- @ Paths

instance Print PathAtom where
  pPrint sig ctx (Π π ts) = PP.pPrint π <> PP.tuple (map (pPrint sig ctx) ts)

-- @ Constraints

-- instance Print Ψ where
--   pPrint = print pPrint

-- instance Print Formula where
--   pPrint = printFormula pPrint

data Quant = All | Ex

instance Print C.Formula where
  pPrint sig = print' 0
   where
    print' pr ctx f = case f of 
      C.Atom a -> pPrint sig ctx a
      C.Top -> PP.text "⊤"
      C.Bot -> PP.text "⊥"
      C.Not p -> PP.paren (pr > notPrec) p'
        where p' = PP.text "¬" <+> print' (notPrec+1) ctx p
      C.And p q -> PP.paren (pr > andPrec) pq
        where pq = PP.sep [PP.hsep[p', PP.text "∧"],
                           q'] 
              p' = print' (andPrec+1) ctx p
              q' = print' andPrec ctx q
      C.Or p q -> PP.paren (pr > orPrec) pq
        where pq = PP.sep [PP.hsep[p', PP.text "∨"],
                           q'] 
              p' = print' (orPrec+1) ctx p
              q' = print' orPrec ctx q
      C.Imp p q -> PP.paren (pr > impPrec) pq
        where pq = PP.sep [PP.hsep[p', PP.text "⊃"],
                           q'] 
              p' = print' (impPrec+1) ctx p
              q' = print' impPrec ctx q
      C.Iff p q -> PP.paren (pr > iffPrec) pq
        where pq = PP.sep [PP.hsep[p', PP.text "⇔"],
                           q'] 
              p' = print' (iffPrec+1) ctx p
              q' = print' iffPrec ctx q
      C.All _ σ _ -> ppQuant "∀" All pr σ ctx f
      C.Ex _ σ _ -> ppQuant "∃" Ex pr σ ctx f
      C.Hole n -> PP.braces $ PP.int n

    ppQuant sym quant pr σ ctx p = 
      let (xs, p') = gather σ quant p 
          p'' = print' quantPrec (Ctx.insertMany xs σ ctx) p'
          f' = PP.hang (PP.text sym <+> PP.hsep (map (pPrint sig ctx) xs) <+> PP.colon <+> PP.pPrint σ <> PP.dot)
                       2 p''
      in PP.paren (pr > quantPrec) f'

    gather :: Sort.Base -> Quant -> C.Formula -> ([Var], C.Formula)
    gather σ All (C.All x' σ' p) | σ == σ' = (x':xs, p')
     where (xs, p') = gather σ All p
    gather σ Ex (C.Ex x' σ' p) | σ == σ' = (x':xs, p')
     where (xs, p') = gather σ Ex p
    gather _ _ p = ([], p)

    notPrec, andPrec, orPrec, impPrec, iffPrec, quantPrec :: Int
    notPrec = 9
    andPrec = 8
    orPrec = 7
    impPrec = 6
    iffPrec = 5
    quantPrec = 2

-- @ Non-polarized formulas

instance Print Formula where
  pPrint sig ctx = Formula.print (pPrint sig ctx)

-- @ Consequents

instance Print Cons where
  pPrint _ _ Cons.X = PP.text "·"
  pPrint sig ctx (Cons.Rel p) = pPrint sig ctx p

-- @ Antecedents

instance Print Ants where
  pPrint sig ctx = Ants.print (pPrint sig ctx)

-- @ Substitutions

instance Print Subst.Θ where
  pPrint sig ctx = Subst.print (pPrint sig ctx)

instance Print CSubst.Θ where
  pPrint sig ctx θ = pPrint sig ctx $ CSubst.dest θ 

-- @ Formulas

instance Print Pos where
  pPrint sig ctx = F.printPos (pPrint sig ctx) 0

instance Print Neg where
  pPrint sig ctx = F.printNeg (pPrint sig ctx) 0

-- @ Globals

instance Print Global where
  pPrint sig ctx glob = 
    PP.tuple [ pPrint sig ctx $ Global.ants glob
             , pPrint sig ctx $ Global.constrs glob ]

-- @ Lifted from the default Print class

instance Print Γ where
  pPrint _ _ = PP.pPrint

instance Print Σ where
  pPrint _ _ = PP.pPrint

instance Print Mode where
  pPrint _ _ = PP.pPrint

instance Print Solver where
  pPrint _ _ = PP.pPrint

instance Print Misc.Input where
  pPrint _ _ = PP.pPrint

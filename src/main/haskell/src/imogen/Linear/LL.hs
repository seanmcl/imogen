
-- | Focused linear logic.

-- @ Pragmas

{-# LANGUAGE CPP #-} 

-- @ Signature

module Imogen.Linear.LL
  ( -- * Focused linear logic formulas
    PProp(..)
  , NProp(..)
  , Pos(..)
  , Neg(..)
    -- * Syntax
  , (⊸), (⊗), (⊕), (&), (!), (↑), (↓), (↑↓), (↓↑)
    -- * Util
  , listLolli
  , inferSig
  )
where

-- @ Imports

#include "../../undefined.h" 

import Imogen.Util.Prelude 
import qualified Data.Char as Char
import qualified Imogen.Func as Func
import qualified Imogen.Param as Param
import Imogen.Param(Params)
import qualified Imogen.Rename as Rename
import Imogen.Rename(Rename, rename)
import qualified Imogen.Sort as Sort
import qualified Imogen.Sig as Sig
import Imogen.Sig (Σ)
import qualified Imogen.Term as Term
import Imogen.Term(Encode, Term(..))
import qualified Imogen.Util.Lex as Lex
import qualified Imogen.Util.Parse as P
import Imogen.Util.Parse (Parse, Parser, parser, (<|>), (<?>))
import qualified Imogen.Util.Print as PP
import Imogen.Util.Print (Print, pPrint)
import qualified Imogen.Var as Var
import Imogen.Var(Var, Vars)

-- @ Positive propositional variables 

{- |
Positive atomic propositions.  

In a small abuse of notation, a linear atomic proposition is represented
as a 'Term'.  For instance, the atomic formula p(X) == Rel p [X], is instead
represented as the term Fn p [X].  This representation makes the translation 
to intuitionistic logic easier, though it is admittedly a little strange.
-} 
data PProp = PProp { pt :: Term }
  deriving (Eq, Ord, Show)

-- @@ Instances

instance Vars PProp where
  vars = Var.vars . pt
  free = Var.free . pt

instance Params PProp where
  params = Param.params . pt

instance Rename PProp where
  rename p = rename (pt p) >>= return . PProp

instance Encode PProp where
  encode = pt
  decode = PProp

-- @ Negative propositional variables

-- | Negative atomic propositions.  See the comments for 'PProp'
data NProp = NProp { nt :: Term }
  deriving (Eq, Ord, Show)

-- @@ Instances

instance Vars NProp where
  vars = Var.vars . nt
  free = Var.free . nt

instance Params NProp where
  params = Param.params . nt

instance Rename NProp where
  rename p = rename (nt p) >>= return . NProp

instance Encode NProp where
  encode = nt 
  decode = NProp

-- @ Formulas

-- | Positive formulas
data Pos = PAtom PProp      -- ^ Atoms
         | Tensor Pos Pos   -- ^ p ⊗ q
         | One              -- ^ 1
         | Sum Pos Pos      -- ^ p ⊕ q
         | Zero             -- ^ 0
         | Down Neg         -- ^ ↓ p
         | Bang Neg         -- ^ ! p
         | Ex Var Pos       -- ^ ∃ x. p
  deriving (Eq, Ord, Show)

-- | Negative formulas
data Neg = NAtom NProp      -- ^ Atoms
         | With Neg Neg     -- ^ p & q
         | Top              -- ^ ⊤
         | Lolli Pos Neg    -- ^ p ⊸ q
         | Up Pos           -- ^ ↑ p
         | All Var Neg      -- ^ ∀ x. p
  deriving (Eq, Ord, Show)

-- @ Util

-- Negative syntax

( ⊸ ) :: Pos -> Neg -> Neg
( ⊸ ) = Lolli

( & ) :: Neg -> Neg -> Neg
( & ) = With

( ↑ ) :: Pos -> Neg
( ↑ ) = Up

( ↑↓ ) :: Neg -> Neg
( ↑↓ ) = (↑) . (↓)

-- Positive syntax

( ⊗ ) :: Pos -> Pos -> Pos
( ⊗ ) = Tensor

( ⊕ ) :: Pos -> Pos -> Pos
( ⊕ ) = Sum

( ↓ ) :: Neg -> Pos
( ↓ ) = Down

( ! ) :: Neg -> Pos
( ! ) = Bang

( ↓↑ ) :: Pos -> Pos
( ↓↑ ) = (↓) . (↑)

listLolli :: [Pos] -> Neg -> Neg
listLolli l n = foldr Lolli n l

-- @ Parsing

instance Parse PProp where
  parser = term >>= return . PProp

instance Parse NProp where
  parser = term >>= return . NProp

instance Parse Neg where
  parser = pre >>= return . reifyN

instance Parse Pos where
  parser = pre >>= return . reifyP

-- Preformulas with flat syntax.

data Pre = PAtom' Term
         | NAtom' Term
         | Tensor' Pre Pre
         | One'
         | Sum' Pre Pre
         | Zero'
         | With' Pre Pre
         | Top'
         | Lolli' Pre Pre
         | Down' Pre
         | Up' Pre
         | Bang' Pre
         | All' Var Pre
         | Ex' Var Pre

-- Convert a preformula into a polarized formula 

reifyN :: Pre -> Neg
reifyN f = case f of 
  NAtom' s -> NAtom (NProp s)
  With' p q -> With (reifyN p) (reifyN q)
  Top' -> Top
  Lolli' p q -> Lolli (reifyP p) (reifyN q)
  Up' p -> Up (reifyP p) 
  All' x p -> All x (reifyN p) 
  _ -> Up (reifyP f)

reifyP :: Pre -> Pos
reifyP f = case f of
  PAtom' s -> PAtom (PProp s)
  Tensor' p q -> Tensor (reifyP p) (reifyP q)
  One' -> One
  Sum' p q -> Sum (reifyP p) (reifyP q)
  Zero' -> Zero
  Down' p -> Down (reifyN p)
  Bang' p -> Bang (reifyN p)
  Ex' x p -> Ex x (reifyP p)
  _ -> Down (reifyN f)

-- Parse a preformula

pre :: Parser Pre
pre = P.buildExpressionParser preTable atomicPre <?> "pre" 

preTable :: P.OperatorTable Char () Pre
preTable = [ [ op "&" With' P.AssocRight
             , op "⊗" Tensor' P.AssocRight
             , op "*" Tensor' P.AssocRight ]
           , [ op "⊕" Sum' P.AssocRight
             , op "+" Sum' P.AssocRight]
           , [ op "⊸" Lolli' P.AssocRight
             , op "-o" Lolli' P.AssocRight
             , op "⊃" (\p q -> Lolli' (Bang' p) q) P.AssocRight
             , op "->" (\p q -> Lolli' (Bang' p) q) P.AssocRight
             , op "o-" (flip Lolli') P.AssocLeft]
           , [ op "≗" bilolli P.AssocRight   
             , op "o--o" bilolli P.AssocRight 
             ]
           ]
  where op s f assoc = P.Infix (do { Lex.reservedOp s; return f }) assoc 
        bilolli p q = With' (Lolli' p q) (Lolli' q p)

atomicPre :: Parser Pre
atomicPre = (Lex.reserved "⊤" >> return Top')
        <|> (Lex.reserved "0" >> return Zero')
        <|> (Lex.reserved "1" >> return One')
        <|> unop  "↓" Down'
        <|> unop "!" Bang'
        <|> unop "↑" Up'
        <|> quant "∀" All'
        <|> quant "!" All'
        <|> quant "∃" Ex'
        <|> quant "?" Ex'
        <|> atom
        <|> Lex.parens pre
        <?> "atomic formula"
 where unop op con = do
         Lex.reservedOp op
         p <- atomicPre
         return $ con p

term :: Parser Term
term = P.try (Lex.upperIdentifier >>= return . Var . Var.make)
   <|> P.try (do 
         s <- Lex.lowerIdentifier 
         τs <- P.option [] (P.tuple term)
         return $ Fn (Func.make s) τs)
   <?> "term"

atom :: Parser Pre
atom = do
  t <- term
  case t of
    Var x -> return $ if Char.isLower $ head $ x' then PAtom' $ Fn f [] else NAtom' $ Fn f []
      where x' = Var.name x
            f = Func.make x'
    Fn f _ -> return $ if Char.isLower $ head $ Func.name f then PAtom' t else NAtom' t
    _ -> error "proposition parameter" 

quant :: String -> (Var -> Pre -> Pre) -> Parser Pre
quant s op = do
  Lex.reservedOp s
  xs <- P.many1 Lex.identifier >>= return . map Var.make
  Lex.dot
  p <- pre
  return $ foldr op p xs 

-- @ Printing

instance Print PProp where
  pPrint = pPrint . pt

instance Print NProp where
  pPrint = pPrint . nt

instance Print Pos where
  pPrint = ppPos 0

instance Print Neg where
  pPrint = ppNeg 0

andPrec, orPrec, impPrec, arrowPrec, quantPrec :: Int
arrowPrec = 9
andPrec = 8
orPrec = 7
impPrec = 6
quantPrec = 2

ppPos :: Int -> Pos -> PP.Doc
ppPos pr f = case f of
  PAtom a -> pPrint a
  Tensor p q -> PP.paren (pr > andPrec) pq 
    where pq = PP.sep [PP.hsep[p', PP.text "⊗"],
                       q']
          p' = ppPos (andPrec+1) p
          q' = ppPos andPrec q
  One -> PP.text "1"
  Sum p q -> PP.paren (pr > orPrec) pq 
    where pq = PP.sep [PP.hsep[p', PP.text "⊕"],
                       q']
          p' = ppPos (orPrec+1) p
          q' = ppPos orPrec q
  Zero -> PP.text "0"
  Down p -> PP.paren (pr > arrowPrec) (PP.sep[PP.text "↓", p'])
    where p' = ppNeg arrowPrec p
  Ex x p -> PP.paren (pr > quantPrec) q
    where q = PP.sep [ PP.sep [ PP.text "∃", pPrint x <> PP.dot ]
                     , p']
          p' = ppPos quantPrec p
  Bang p -> PP.paren (pr > arrowPrec) (PP.sep[PP.text "!", p'])
    where p' = ppNeg arrowPrec p

ppNeg :: Int -> Neg -> PP.Doc
ppNeg pr f = case f of
  NAtom a -> pPrint a
  With p q -> PP.paren (pr > andPrec) pq 
    where pq = PP.sep [PP.hsep[p', PP.text "&"],
                       q']
          p' = ppNeg (andPrec+1) p
          q' = ppNeg andPrec q
  Top -> PP.text "⊤"
  Lolli p q -> PP.paren (pr > impPrec) pq 
    where pq = PP.sep [PP.hsep[p', PP.text "⊸"],
                       q']
          p' = ppPos (impPrec+1) p
          q' = ppNeg impPrec q
  All x p -> PP.paren (pr > quantPrec) q
    where q = PP.sep [ PP.sep [ PP.text "∀", pPrint x <> PP.dot ]
                     , p']
          p' = ppNeg quantPrec p
  Up p -> PP.paren (pr > arrowPrec) (PP.sep[PP.text "↑", p'])
    where p' = ppPos arrowPrec p

-- @ Util

inferSig :: Neg -> Σ
inferSig = infN Sig.empty
  where infP sig fm = case fm of 
          PAtom (PProp (Fn f ts)) -> Sig.insert f (Sort.Fun (replicate (length ts) Sort.U) Sort.U) sig
          PAtom (PProp _) -> __IMPOSSIBLE__ 
          Tensor p q -> infP (infP sig p) q
          One -> sig
          Sum p q -> infP (infP sig p) q
          Zero -> sig
          Ex _ p -> infP sig p
          Down p -> infN sig p
          Bang p -> infN sig p
        infN sig fm = case fm of 
          NAtom (NProp (Fn f ts)) -> Sig.insert f (Sort.Fun (replicate (length ts) Sort.U) Sort.U) sig
          NAtom (NProp _) -> __IMPOSSIBLE__ 
          With p q -> infN (infN sig p) q
          Top -> sig
          Lolli p q -> infN (infP sig p) q
          All _ p -> infN sig p
          Up p -> infP sig p

-- @ Examples

_examples :: [Neg]
_examples = map P.parse
  [ "0"
  , "1"
  , "⊤"
  , "p"
  , "P"
  , "p ⊗ q"
  , "P ⊗ Q"
  , "p & q"
  , "P & Q"
  , "p ⊕ q"
  , "P ⊕ Q"
  , "1 ⊸ 1"
  , "p ⊸ 1 ⊗ q"
  , "(p ⊸ 1) ⊗ q"
  , "(P ⊸ 1) ⊗ Q"
  , "φ ⊗ ρ"
  ]

_main :: IO ()
_main = pprint _examples

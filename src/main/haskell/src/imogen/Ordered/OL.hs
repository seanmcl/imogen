
-- | Focused ordered logic. 

-- @ Pragmas

{-# LANGUAGE CPP #-} 

-- @ Signature

module Imogen.Ordered.OL
  ( -- * Focused ordered logic formulas
    PProp(..)
  , NProp(..)
  , Pos(..)
  , Neg(..)
  -- * Syntax
  , (>->), (->>), (∧), (⊤), (●), (⊕), (!), (¡), (↑), (↓), (↑↓), (↓↑)
  -- * Util
  , inferSig
  )
where

-- @ Imports

#include "../../undefined.h" 

import Imogen.Util.Prelude 
import qualified Data.Char as Char
import qualified Imogen.Func as Func
import Imogen.Linear.LL(PProp(..), NProp(..))
import qualified Imogen.Sort as Sort
import qualified Imogen.Sig as Sig
import Imogen.Sig (Σ)
import qualified Imogen.Term as Term
import Imogen.Term(Term(..))
import qualified Imogen.Util.Lex as Lex
import qualified Imogen.Util.Parse as P
import Imogen.Util.Parse (Parse, Parser, parser, (<|>), (<?>))
import qualified Imogen.Util.Print as PP
import Imogen.Util.Print (Print, pPrint)
import qualified Imogen.Var as Var
import Imogen.Var(Var)

-- @ Formulas

-- Positive

data Pos = PAtom PProp     -- ^ Atoms
         | Dot Pos Pos     -- ^ p ● q
         | One             -- ^ 1
         | Sum Pos Pos     -- ^ p ⊕ q
         | Zero            -- ^ 0
         | Down Neg        -- ^ ↓ p
         | Bang Neg        -- ^ ! p
         | UBang Neg       -- ^ ¡ p
         | Ex Var Pos      -- ^ ∃ x. p
  deriving (Eq, Ord, Show)

-- Negative

data Neg = NAtom NProp     -- ^ Atoms
         | And Neg Neg     -- ^ p ∧ q
         | Top             -- ^ ⊤
         | ImpL Pos Neg    -- ^ p >-> q 
         | ImpR Pos Neg    -- ^ p ->> q
         | Up Pos          -- ^ ↑ p
         | All Var Neg     -- ^ ∀ x. p
  deriving (Eq, Ord, Show)

-- @ Util

-- @@ Negative syntax

( >-> ) :: Pos -> Neg -> Neg
( >-> ) = ImpL

( ->> ) :: Pos -> Neg -> Neg
( ->> ) = ImpR

( ∧ ) :: Neg -> Neg -> Neg
( ∧ ) = And

(⊤) :: Neg
(⊤) = Top

( ↑ ) :: Pos -> Neg
( ↑ ) = Up

-- | Double shift.
( ↑↓ ) :: Neg -> Neg
( ↑↓ ) = (↑) . (↓)

-- @@ Positive syntax

( ● ) :: Pos -> Pos -> Pos
( ● ) = Dot

( ⊕ ) :: Pos -> Pos -> Pos
( ⊕ ) = Sum

( ! ) :: Neg -> Pos
( ! ) = Bang

( ¡ ) :: Neg -> Pos
( ¡ ) = UBang

( ↓ ) :: Neg -> Pos
( ↓ ) = Down

-- | Double shift.
( ↓↑ ) :: Pos -> Pos
( ↓↑ ) = (↓) . (↑)

-- @ Parsing

-- Ordered logic

instance Parse Neg where
  parser = pre >>= return . reifyN

instance Parse Pos where
  parser = pre >>= return . reifyP

-- Preformulas with flat syntax.

data Pre = PAtom' Term
         | NAtom' Term
         | Dot' Pre Pre
         | One'
         | Sum' Pre Pre
         | Zero'
         | And' Pre Pre
         | Top'
         | ImpL' Pre Pre
         | ImpR' Pre Pre
         | Down' Pre
         | Up' Pre
         | Bang' Pre
         | UBang' Pre
         | All' Var Pre
         | Ex' Var Pre

-- Convert a preformula into a polarized formula 

reifyN :: Pre -> Neg
reifyN f = case f of 
  NAtom' s -> NAtom (NProp s)
  And' p q -> And (reifyN p) (reifyN q)
  Top' -> Top
  ImpL' p q -> ImpL (reifyP p) (reifyN q)
  ImpR' p q -> ImpR (reifyP p) (reifyN q)
  Up' p -> Up (reifyP p) 
  All' x p -> All x (reifyN p)
  _ -> Up (reifyP f)

reifyP :: Pre -> Pos
reifyP f = case f of
  PAtom' s -> PAtom (PProp s)
  Dot' p q -> Dot (reifyP p) (reifyP q)
  One' -> One
  Sum' p q -> Sum (reifyP p) (reifyP q)
  Zero' -> Zero
  Down' p -> Down (reifyN p)
  Bang' p -> Bang (reifyN p)
  UBang' p -> UBang (reifyN p)
  Ex' x p -> Ex x (reifyP p)
  _ -> Down (reifyN f)

-- Parse a preformula

pre :: Parser Pre
pre = P.buildExpressionParser preTable atomicPre <?> "pre" 

preTable :: P.OperatorTable Char () Pre
preTable = [ [ op "∧" And' P.AssocRight
             , op "●" Dot' P.AssocRight
             , op "*" Dot' P.AssocRight 
             ]
           , [ op "⊕" Sum' P.AssocRight
             , op "+" Sum' P.AssocRight 
             ]
           , [ op ">->" ImpL' P.AssocRight
             , op "->>" ImpR' P.AssocRight
             , op "<-<" (flip ImpR') P.AssocLeft
             , op "<<-" (flip ImpL') P.AssocLeft
             ]
           ]
  where op s f assoc = P.Infix (do { Lex.reservedOp s; return f }) assoc 

atomicPre :: Parser Pre
atomicPre = (Lex.reserved "⊤" >> return Top')
        <|> (Lex.reserved "0" >> return Zero')
        <|> (Lex.reserved "1" >> return One')
        <|> do Lex.reservedOp "!"
               p <- atomicPre
               return $ Bang' p
        <|> do Lex.reservedOp "¡"
               p <- atomicPre
               return $ UBang' p
        <|> do Lex.reservedOp "↓"
               p <- atomicPre
               return $ Down' p
        <|> do Lex.reservedOp "↑"
               p <- atomicPre
               return $ Up' p
        <|> Lex.parens pre
        <|> quant "∀" All'
        <|> quant "!" All'
        <|> quant "∃" Ex'
        <|> quant "?" Ex'
        <|> atom
        <?> "atomic formula"

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
  Dot p q -> PP.paren (pr > andPrec) pq 
    where pq = PP.sep [PP.hsep[p', PP.text "●"],
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
  Bang p -> PP.paren (pr > arrowPrec) (PP.sep[PP.text "!", p'])
    where p' = ppNeg arrowPrec p
  UBang p -> PP.paren (pr > arrowPrec) (PP.sep[PP.text "¡", p'])
    where p' = ppNeg arrowPrec p
  Ex x p -> PP.paren (pr > quantPrec) q
    where q = PP.sep [ PP.sep [ PP.text "∃", pPrint x <> PP.dot ]
                     , p']
          p' = ppPos quantPrec p

ppNeg :: Int -> Neg -> PP.Doc
ppNeg pr f = case f of
  NAtom a -> pPrint a
  And p q -> PP.paren (pr > andPrec) pq 
    where pq = PP.sep [PP.hsep[p', PP.text "∧"],
                       q']
          p' = ppNeg (andPrec+1) p
          q' = ppNeg andPrec q
  Top -> PP.text "⊤"
  ImpL p q -> PP.paren (pr > impPrec) pq 
    where pq = PP.sep [PP.hsep[p', PP.text ">->"],
                       q']
          p' = ppPos (impPrec+1) p
          q' = ppNeg impPrec q
  ImpR p q -> PP.paren (pr > impPrec) pq 
    where pq = PP.sep [PP.hsep[p', PP.text "->>"],
                       q']
          p' = ppPos (impPrec+1) p
          q' = ppNeg impPrec q
  Up p -> PP.paren (pr > arrowPrec) (PP.sep[PP.text "↑", p'])
    where p' = ppPos arrowPrec p
  All x p -> PP.paren (pr > quantPrec) q
    where q = PP.sep [ PP.sep [ PP.text "∀", pPrint x <> PP.dot ]
                     , p']
          p' = ppNeg quantPrec p

-- @ Util

inferSig :: Neg -> Σ
inferSig = infN Sig.empty
  where infP sig fm = case fm of 
          PAtom (PProp (Fn f ts)) -> Sig.insert f (Sort.Fun (replicate (length ts) Sort.U) Sort.U) sig
          PAtom (PProp _) -> __IMPOSSIBLE__ 
          Dot p q -> infP (infP sig p) q
          One -> sig
          Sum p q -> infP (infP sig p) q
          Zero -> sig
          Down p -> infN sig p
          Bang p -> infN sig p
          UBang p -> infN sig p
          Ex _ p -> infP sig p
        infN sig fm = case fm of 
          NAtom (NProp (Fn f ts)) -> Sig.insert f (Sort.Fun (replicate (length ts) Sort.U) Sort.U) sig
          NAtom (NProp _) -> __IMPOSSIBLE__ 
          And p q -> infN (infN sig p) q
          Top -> sig
          ImpL p q -> infN (infP sig p) q
          ImpR p q -> infN (infP sig p) q
          Up p -> infP sig p
          All _ p -> infN sig p

-- @ Examples

_examples :: [Neg]
_examples = map P.parse
  [ "0"
  , "1"
  , "⊤"
  , "p"
  , "P"
  , "p ∧ q"
  , "P ∧ Q"
  , "p ● q"
  , "P ● Q"
  , "p ⊕ q"
  , "P ⊕ Q"
  , "1 >-> 1"
  , "p ->> 1 >-> q"
  , "(p >-> 1) ● q"
  , "(P ->> 1) >-> Q"
  , "↑ φ ● ¡ ρ"
  ]

_main :: IO ()
_main = pprint _examples

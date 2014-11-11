
-- | Parsing.

-- @ Pragmas

{-# LANGUAGE FlexibleInstances #-} 
{-# OPTIONS_GHC -fno-warn-orphans #-} 

-- @ Signature

module Imogen.Parse 
  ( Parse -- re-export
  , tptp
  , tptpFile 
  ) 
where

-- @ Imports

import Imogen.Util.Prelude 
import qualified Data.Char as Char
import qualified Data.Either as Either
import qualified Imogen.Atom as Atom
import Imogen.Atom (Atom(Rel))
import qualified Imogen.Ants as Ants
import Imogen.Ants (Ants)
import qualified Imogen.Cons as Cons
import Imogen.Cons (Cons)
import qualified Imogen.Constr as C
import qualified Imogen.CSubst as CSubst
import qualified Imogen.Ctx as Ctx
import Imogen.Ctx (Γ)
import qualified Imogen.Formula as Formula
import Imogen.Formula (Formula)
import qualified Imogen.Func as Func
import qualified Imogen.Linear as Linear
import qualified Imogen.Misc as Misc
import qualified Imogen.Modal as Modal
import qualified Imogen.Ordered as Ordered
import qualified Imogen.Param as Param
import Imogen.Param (Param)
import qualified Imogen.Path as Path
import Imogen.Path (PathElem, PathAtom(Π))
import qualified Imogen.PFormula as F
import Imogen.PFormula (Pos, Neg)
import qualified Imogen.Pred as Pred
import qualified Imogen.Rule as Rule
import Imogen.Rule (Rule, RSeq)
import qualified Imogen.Seq as Seq
import Imogen.Seq (Seq)
import qualified Imogen.Sig as Sig
import Imogen.Sig (Σ, Entry)
import qualified Imogen.Sort as Sort
import Imogen.Sort (Base, Sort)
import qualified Imogen.Subst as Subst
import Imogen.Subst (Θ)
import qualified Imogen.Term as Term
import qualified Imogen.Term as T
import Imogen.Term (Term)
import qualified Imogen.Util.Lex as Lex
import qualified Imogen.Util.List as List
import qualified Imogen.Util.Parse as P
import Imogen.Util.Parse (Parse, Parser, parser, (<|>), (<?>))
import qualified Imogen.Var as Var
import Imogen.Var (Var)

-- @ Variables

-- Identifiers starting with 'varIds' are always parsed as variables.
varIds :: String
varIds = "αβγδρφ"

-- TPTP vars can have the form $VAR(N)
tptpVar :: Parser Var
tptpVar = do Lex.reservedOp "$"
             Lex.reservedOp "VAR"
             n <- Lex.parens Lex.int
             return $ Var.make $ "var" ++ show n

instance Parse Var where
  parser = do c <- Lex.identifier 
              if elem (head c) varIds || Char.isUpper (head c) 
                then return $ Var.make c
                else fail ""
       <|> tptpVar

-- @ Parameters

-- Identifiers starting with 'paramIds' are always parsed as parameters.
paramIds :: String
paramIds = "çĉ_"

instance Parse Param where
  parser = do c <- Lex.identifier 
              if elem (head c) paramIds 
                then return $ Param.make c
                else fail ""

-- @ Terms

instance Parse Term where 
  parser = P.buildExpressionParser table atomic <?> "term"

table :: P.OperatorTable Char () Term
table = map (map mkInfix) $ List.groupBy grp Misc.infixFuncs
  where mkInfix (s, (_, assoc)) = 
          P.Infix (Lex.reservedOp s >> return (\x y -> T.Fn (Func.make s) [x, y])) assoc
        -- group the infixes by precedence
        grp (_, (n, _)) (_, (m, _)) = n == m

atomic :: Parser Term
atomic = P.try (do 
            c <- Lex.identifier 
            if elem c Misc.consts then return $ T.Fn (Func.make c) [] else fail "")
     <|> (Lex.integer >>= \n -> return $ T.Fn (Func.make (show n)) [])
     <|> P.try (parser >>= return . T.Var)
     <|> P.try (parser >>= return . T.Param)
     <|> P.try (do 
           s <- Lex.lowerIdentifier 
           τs <- P.option [] (P.tuple P.parser)
           return $ T.Fn (Func.make s) τs)
     <|> Lex.parens parser
     <?> "term"

-- @ Atomic formulas

{- | 
We need a special lexical class for some unicode operators that aren't
parsed by Lex.operator or Lex.identifier.
-} 
unicodeBinops :: Parser String
unicodeBinops = P.choice $ map Lex.symbol ["≡", "◂", "◃"]

instance Parse Atom where
  parser = P.try (do t1 <- parser
                     op <- Lex.operator <|> Lex.identifier <|> unicodeBinops
                     case lookupInfix op of 
                       Nothing -> fail ""
                       Just f -> do
                         t2 <- parser 
                         return $ f t1 t2)
       <|> do p <- Lex.identifier 
              τs <- P.option [] (P.tuple parser)
              return $ Rel (Pred.make p) τs
       <?> "atom"

   where

    lookupInfix s = look Misc.infixBinops
      where look [] = Nothing
            look ((l, p):t) | elem s l = Just p
                            | otherwise = look t

-- @ Formulas

{- |
We first parse to a preformula that isn't careful about arrows.  We then
reify the preformula into a syntactically correct negative formula.
-} 

instance Parse Neg where
  parser = pre >>= return . reifyN

instance Parse Pos where
  parser = pre >>= return . reifyP

data Pre = Atom' Atom
           -- Negative 
         | And' Pre Pre
         | Top'
         | Imp' Pre Pre
         | Iff' Pre Pre
         | All' Var Base Pre
         | Up' Pre
         | Box' Term Pre
           -- Positive
         | Tensor' Pre Pre
         | One'
         | Or' Pre Pre
         | Bot'
         | Ex' Var Base Pre
         | Down' Pre
         | Diamond' Term Pre

reifyN :: Pre -> Neg
reifyN f = case f of 
  Atom' a -> if Atom.isPos a then F.Up $ F.PAtom a else F.NAtom a
  And' p q -> F.And (reifyN p) (reifyN q)
  Top' -> F.Top
  Imp' p q -> F.Imp (reifyP p) (reifyN q)
  Iff' p q -> F.Iff (reifyN p) (reifyN q)
  All' x s p -> F.All x s (reifyN p)
  Box' w p -> F.Box w (reifyN p)
  Up' p -> F.Up (reifyP p)
  _ -> F.Up (reifyP f)

reifyP :: Pre -> Pos
reifyP f = case f of
  Atom' a -> if Atom.isPos a then F.PAtom a else F.Down $ F.NAtom a
  Tensor' p q -> F.Tensor (reifyP p) (reifyP q)
  One' -> F.One
  Or' p q -> F.Or (reifyP p) (reifyP q)
  Bot' -> F.Bot
  Ex' x s p -> F.Ex x s (reifyP p)
  Down' p -> F.Down (reifyN p)
  Diamond' w p -> F.Diamond w (reifyP p)
  _ -> F.Down (reifyN f)

pre :: Parser Pre
pre = P.buildExpressionParser preTable atomicPre <?> "pre" 

preTable :: P.OperatorTable Char () Pre
preTable = [ [ op "∧" And' P.AssocRight
             , op "⊗" Tensor' P.AssocRight 
             , op "&" And' P.AssocRight 
             ]
           , [ op "∨" Or' P.AssocRight 
             , op "|" Or' P.AssocRight 
             ]
           , [ op "=>" Imp' P.AssocRight
             , op "⊃" Imp' P.AssocRight 
             , op "<=" (flip Imp') P.AssocLeft 
             , op "⊂" (flip Imp') P.AssocLeft 
             ]
           , [ op "<=>" Iff' P.AssocRight
             , op "⇔" Iff' P.AssocRight 
             ]
           ]
  where op s f assoc = P.Infix (do { Lex.reservedOp s; return f }) assoc 

-- Note that the scope of TPTP quantifiers is the smallest possible,
-- while that of Imogen formulas is the largest possible.
tptpQuant :: String -> (Var -> Base -> Pre -> Pre) -> Parser Pre
tptpQuant s op = do
  Lex.reservedOp s
  xs <- P.list parser
  Lex.colon
  p <- atomicPre
  return $ foldr (flip op Sort.U) p xs 

imoQuant :: String -> (Var -> Base -> Pre -> Pre) -> Parser Pre
imoQuant s op = do
  Lex.reservedOp s
  xs <- P.commas parser
  σ <- (Lex.colon >> parser) <|> return Sort.U
  Lex.dot
  p <- pre
  return $ foldr (flip op σ) p xs 

quant :: String -> (Var -> Base -> Pre -> Pre) -> Parser Pre
quant s f = P.try (imoQuant s f) <|> tptpQuant s f

mode :: String -> (Term -> Pre -> Pre) -> Parser Pre
mode s f = do
  Lex.reservedOp s
  p <- atomicPre
  return $ f Term.defaultPrincipal p

atomicPre :: Parser Pre
atomicPre = do Lex.reserved "⊤"
               return Top'
        <|> do Lex.reserved "⊥"
               return Bot'
        <|> do Lex.reservedOp "↓"
               p <- atomicPre
               return $ Down' p
        <|> do Lex.reservedOp "↑"
               p <- atomicPre
               return $ Up' p
        <|> do Lex.reservedOp "¬"
               p <- atomicPre
               return $ Imp' p Bot'
        <|> quant "∀" All'
        <|> quant "!" All'
        <|> quant "∃" Ex'
        <|> quant "?" Ex'
        <|> mode "□" Box'
        <|> mode "box" Box'
        <|> mode "◇" Diamond'
        <|> mode "dia" Diamond'
        <|> (parser >>= return . Atom')
        <|> P.try (do Lex.reserved "1"
                      return One')
        <|> Lex.parens pre
        <?> "atomic formula"

-- @ Constraints

instance Parse C.Formula where
  parser = constr

constr :: Parser C.Formula
constr = P.buildExpressionParser constrTable atomicConstr <?> "constr" 

constrTable :: P.OperatorTable Char () C.Formula
constrTable = [ [ op "&" C.And P.AssocRight 
                , op "∧" C.And P.AssocRight 
                ] 
              , [ op "∨" C.Or P.AssocRight 
                , op "|" C.Or P.AssocRight 
                ] 
              , [ op "->" C.Imp P.AssocRight
                , op "<-" (flip C.Imp) P.AssocLeft 
                , op "⊃" C.Imp P.AssocRight 
                , op "⊂" (flip C.Imp) P.AssocLeft
                ]
              , [ op "<=>" C.Iff P.AssocRight
                , op "⇔" C.Iff P.AssocRight
                ]
              ] 
  where op s f assoc = P.Infix (do { Lex.reservedOp s; return f }) assoc 

atomicConstr :: Parser C.Formula
atomicConstr = 
         do Lex.reserved "⊤" <|> Lex.reserved "true" 
            return C.Top
     <|> do Lex.reserved "∀"
            (x, σ) <- Lex.parens (do x <- Lex.upperIdentifier
                                     Lex.colon
                                     (σ::Base) <- parser
                                     return (x, σ))
            Lex.dot
            p <- constr
            return $ C.All (Var.make x) σ p
     <|> do Lex.reserved "∃"
            (x, σ) <- Lex.parens (do x <- Lex.upperIdentifier
                                     Lex.colon
                                     (σ::Base) <- parser
                                     return (x, σ))
            Lex.dot
            p <- constr
            return $ C.Ex (Var.make x) σ p
     <|> Lex.parens constr
     <|> (parser >>= return . C.Atom)
     <|> (Lex.int >>= return . C.Hole)
     <?> "atomic formula"

-- @ Modal atoms

instance Parse Modal.Atom where
  parser = do Rel p ts <- parser
              w <- do Lex.reservedOp "@"
                      parser
               <|> return Modal.ε 
              return $ Modal.Rel p ts w

instance Parse Modal.World where
  parser = parser >>= return . Term.decode

-- @ Consequents

instance Parse Cons where
  parser = (Lex.reserved "·" >> return Cons.X)
       <|> (P.parser >>= return . Cons.Rel)
       <?> "Cons" 

-- @ Antecedents

instance Parse Ants where
  parser = (Lex.reserved "·" >> return Ants.empty)
       <|> P.try (parser >>= return . flip Ants.insert Ants.empty)
       <|> (P.list parser >>= return . foldr Ants.insert Ants.empty)

-- @ Contexts

instance Parse Γ where
  parser = do 
      pmaps <- P.braces pmap
      return . Ctx.fromList $ concatMap expand pmaps
    where pmap :: Parser ([Var], Base)
          pmap = do xs <- P.commas parser
                    Lex.reservedOp ":"
                    σ <- parser
                    return $ (xs, σ)

expand :: ([a], b) -> [(a, b)]
expand (xs, σ) = map (\x -> (x, σ)) xs

-- @ Signatures

instance Parse Σ where
  parser = do
    pmaps <- P.braces pmap 
    return . Sig.fromList $ concatMap expand pmaps
   where
     pmap :: Parser ([Entry], Sort)
     pmap = do xs <- P.commas Lex.identifier
               Lex.reservedOp ":"
               σ <- parser
               return $ case σ of 
                 Sort.Rel _ -> (map (Sig.Pred . Pred.make) xs, σ)
                 Sort.Fun _ _ -> (map classify xs, σ)

     classify f | elem (head f) paramIds = Sig.Param $ Param.make f
                | otherwise = Sig.Func $ Func.make f

-- @ Sequents

{- 
Sequents are parsed as ψ | ד ⊢ γ.  Since they are only parsed
for testing, we can safely use a dummy counter.
-} 

instance Parse Seq where
  parser = do ψ <- parser
              Lex.reservedOp "⎜" -- Note, this is unicode u239C.  (Cf. ⎜ |)
              ד <- parser
              Lex.reservedOp "⊢"
              γ <- parser
              ctx <- parser
              return $ Seq.unsafeMake ψ ד γ ctx
                     
-- @ Inference Rules

-- ([H1, …, Hn], H, ρ, ψ)

instance Parse RSeq where
  parser = do ד <- parser
              Lex.reservedOp "⊢"
              γ <- parser
              return $ Rule.RSeq ד γ

instance Parse Rule where
  parser = do ((▵), δ, ρ, ψ, ctx) <- parser
              return $ Rule.unsafeMake (▵) δ ρ ψ ctx

-- @ Substitutions

instance Parse Θ where
  parser = P.braces vmap >>= return . Subst.fromList
    where vmap :: Parser (Var, Term)
          vmap = do 
            x <- parser
            Lex.reservedOp "↦"
            t <- parser
            return (x, t)

-- @ Constrainted substitutions

instance Parse CSubst.Θ where
  parser = Lex.parens $ do
    θ <- parser
    Lex.comma
    ψ <- parser
    return $ CSubst.make θ ψ

-- @ Paths

instance Parse PathElem where
  parser = (Lex.reserved "∧l" >> return Path.PAndL)
       <|> (Lex.reserved "∧r" >> return Path.PAndR)
       <|> (Lex.reserved "⊃l" >> return Path.PImpL)
       <|> (Lex.reserved "⊃r" >> return Path.PImpR)
       <|> (Lex.reserved "∀" >> return Path.PAll)
       <|> (Lex.reserved "□" >> parser >>= return . Path.PBox)
       <|> (Lex.reserved "◇" >> parser >>= return . Path.PDiamond)
       <?> "PathElem"

instance Parse [PathElem] where
  parser = P.list parser

instance Parse PathAtom where
  parser = do π <- P.parser
              args <- P.option [] P.parser
              return $ Π π args

-- @ Linear logic

instance Parse Linear.World where
  parser = do
    x :: Term <- P.parser 
    return $ T.decode x

-- @ Ordered

instance Parse Ordered.World where
  parser = do
    x :: Term <- P.parser 
    return $ T.decode x

-- @ Tptp

instance Parse Formula where
  parser = formula

formula :: Parser Formula
formula = P.buildExpressionParser formulaTable atomicFormula <?> "Formula" 

formulaTable :: P.OperatorTable Char () Formula
formulaTable = [ [ op "&" Formula.And P.AssocRight 
                , op "∧" Formula.And P.AssocRight 
                ] 
              , [ op "∨" Formula.Or P.AssocRight 
                , op "|" Formula.Or P.AssocRight 
                ] 
              , [ op "=>" Formula.Imp P.AssocRight
                , op "->" Formula.Imp P.AssocRight
                , op "<=" (flip Formula.Imp) P.AssocLeft 
                , op "<-" (flip Formula.Imp) P.AssocLeft 
                , op "⊃" Formula.Imp P.AssocRight 
                , op "⊂" (flip Formula.Imp) P.AssocLeft
                ]
              , [ op "<=>" Formula.Iff P.AssocRight
                , op "⇔" Formula.Iff P.AssocRight
                ]
              ] 
  where op s f assoc = P.Infix (do { Lex.reservedOp s; return f }) assoc 

atomicFormula :: Parser Formula
atomicFormula = 
         do P.try $ Lex.reserved "⊤" <|> (P.optional (Lex.reservedOp "$") >> Lex.reserved "true")
            return Formula.Top
     <|> do Lex.reserved "⊥" <|> (P.optional (Lex.reservedOp "$") >> Lex.reserved "false")
            return Formula.Bot
     <|> do Lex.reservedOp "~"
            p <- atomicFormula
            return $ Formula.Not p
     <|> do Lex.reserved "!"
            xs <- P.list Lex.upperIdentifier
            Lex.colon
            p <- atomicFormula
            return $ Formula.listAll (map Var.make xs) (replicate (length xs) Sort.U) p
     <|> do Lex.reserved "?"
            xs <- P.list Lex.upperIdentifier
            Lex.colon
            p <- atomicFormula
            return $ Formula.listEx (map Var.make xs) (replicate (length xs) Sort.U) p
     <|> Lex.parens formula
     <|> (parser >>= return . Formula.Atom)
     <?> "atomic formula"

{- | 
Parse a file into a Formula.  Also returns a boolean indicating
whether there was a conjecture or not.
-} 
tptp :: Parser (Formula, Bool)
tptp = arrange <$> P.many clause

 where

  ann :: Parser ()
  ann = Lex.reservedOp "fof" <|> Lex.reservedOp "cnf"

  clause :: Parser (Either Formula Formula)
  clause = do
   ann
   (cid, f) <- Lex.parens $ do 
     Lex.lowerIdentifier
     Lex.comma
     cid <- Lex.lowerIdentifier
     Lex.comma
     f <- parser
     return (cid, f)
   Lex.dot
   if elem cid ["axiom", "hypothesis", "negated_conjecture", "lemma"]
    then return $ Left f
    else if cid == "conjecture" then return $ Right f
    else error $ "Unknown clause id: " ++ cid

  arrange :: [Either Formula Formula] -> (Formula, Bool)
  arrange clauses = 
    let (hyps, conjs) = Either.partitionEithers clauses in
    case conjs of
      [] -> (Formula.listImp hyps (Formula.⊥), False)
      [c] -> (Formula.listImp hyps c, True)
      _ -> error "tptp parsing: multiple conjectures"

-- @ Interface

-- | Parse a TPTP problem file in the TPTP syntax. 
tptpFile :: FilePath -> IO (Formula, Bool)
tptpFile = Lex.makeFileParser tptp

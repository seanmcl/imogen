
{- | 
/Sorts/.

Variables and parameters in Imogen are sorted.  The sorts are not just
for creating subsets of a set of individuals as is sometimes seen, but
are fundamental.  For instance, different unification, normalization,
and pretty printing algorithms apply at different sorts.  

We associate sorts with types via the notion of /Tags/.
-}

-- @ Pragmas

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, DeriveDataTypeable #-}

-- @ Signature

module Imogen.Sort 
  ( -- * Sorts
    Base(..)
  , Sort(..) 
    -- * Tags
  , Tag
  , TagOf
    -- * Normalization
  , Normalize(..)
  , OK(..)
  )
where 

-- @ Imports

import Imogen.Util.Prelude hiding (Int, Real)
import Control.Monad as M
import qualified Imogen.Util.Lex as Lex
import qualified Imogen.Util.Parse as P
import Imogen.Util.Parse (Parse, parser, (<|>), (<?>))
import qualified Imogen.Util.Print as PP
import Imogen.Util.Print (Print, pPrint)

-- @ Sorts

-- | Base sorts.

data Base = U           -- ^ Uninterpreted function symbols 
          | Real        -- ^ Real arithmetic
          | Int         -- ^ Integer arithmetic
          | Principal   -- ^ Modal principals
          | Head        -- ^ Heads (variables or constants) for linear and ordered logic
          | LWorld      -- ^ Linear logic worlds
          | LFrame      -- ^ Linear logic frames
          | OWorld      -- ^ Ordered logic worlds
          | OFrame      -- ^ Ordered logic frames
          | MEdge       -- ^ Modal edge
          | MWorld      -- ^ Modal world 
  deriving (Eq, Ord, Read, Show, Data, Typeable)

-- | Higher order sorts.

data Sort = Rel [Base]       -- ^ Relations
          | Fun [Base] Base  -- ^ Functions
  deriving (Eq, Ord, Show)

-- @ Tags 

{- |
A (strictly nominal) class for identifying sort tags.  

We use tags for dynamic dispatch based on the sort of the term.
-} 
class Tag σ where

{- |
A class for associating sort tags with (abstract) types for unification.

Notice that the functional dependencies force that there can be at
most one association between a tag and a type.
-} 
class Tag σ => TagOf σ a | σ -> a, a -> σ where

-- @ Normalization

{- |
Normal forms will differ from sort to sort.  For instance, if ε is the identity
of the @⋆@ operator on a type, then the normal form of @a ✴ ε@ will probably be @a@.
For uninterpreted functions it will be something like @a ✴ ε@.
 -} 
class Tag σ => Normalize a σ where
  normalize :: σ -> a -> a

{- |
Some normal forms are illegal due to semantic considerations.  For example, in
linear logic you can't have a frame of the form f ◃ p ⋆ p.  Noticing illegal
unifications (for instance) during proof search can have a big impact on performance.
Of course, what is allowed differs from sort to sort.  Thus we associate an
@ok@ function with each sort.
-} 
class Tag σ => OK a σ where
  ok :: σ -> a -> Bool

-- @ Parsing

instance Parse Base where
  parser = P.choice (map (\(s, σ) -> Lex.reserved s >> return σ) pairs) <?> "base sort"
   where pairs = [ ("U", U)
                 , ("Real", Real)
                 , ("Int", Int)
                 , ("Principal", Principal)
                 , ("Head", Head)
                 , ("LWorld", LWorld)
                 , ("LFrame", LFrame)
                 , ("OWorld", OWorld)
                 , ("OFrame", OFrame)
                 , ("MWorld", MWorld)
                 , ("MEdge", MEdge)
                 ]

instance Parse Sort where
  parser = (Lex.reserved "Prop" >> (return $ Rel []))
       <|> do σ <- parser
              σs :: [Base] <- P.many (P.try (arrow >> parser))
              prop <- P.optionMaybe (arrow >> Lex.reserved "Prop")
              case prop of 
                Just () -> return . Rel $ σ : σs
                Nothing -> case reverse σs of 
                  [] -> return $ Fun [] σ 
                  σ' : σs' -> return $ Fun (σ : reverse σs') σ'
   where 
    arrow = Lex.symbol "→" <|> Lex.symbol "->"

-- @ Printing

instance Print Base where
  pPrint σ = case σ of 
    MWorld -> PP.text "ω"
    _ -> PP.text $ show σ

instance Print Sort where
  pPrint (Rel σs) = PP.hcat $ PP.punctuate (PP.text " ↦ " :: Doc) (map pPrint σs ++ [PP.text "o"] :: [Doc])
  pPrint (Fun σs σ) = PP.hcat $ PP.punctuate (PP.text " ↦ ") (map pPrint $ σs ++ [σ])

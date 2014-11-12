
-- @ Pragmas

{-# LANGUAGE OverlappingInstances #-} 

-- @ Signature

module Imogen.Util.Parse.Parse
  ( Parse(..)
  , tuple
  , list
  , commas
  , braces
  )
where

-- @ Imports

import Imogen.Util.Prelude hiding (list)
import qualified Data.Set as Set
import Text.ParserCombinators.Parsec as P
import qualified Imogen.Util.Lex as Lex

-- @ Utils 

commas :: Parser a -> Parser [a]
commas p = P.sepBy p Lex.comma

tuple :: Parser a -> Parser [a]
tuple = Lex.parens . commas

list :: Parser a -> Parser [a]
list = Lex.brackets . commas

braces :: Parser a -> Parser [a]
braces = Lex.braces . commas

-- @ Class

class Parse a where
  parser :: Parser a
  parse :: String -> a
  parse = Lex.makeParser parser
  parseFile :: FilePath -> IO a
  parseFile = Lex.makeFileParser parser

instance Parse Bool where
  parser = (Lex.reserved "True" >> return True)
       <|> (Lex.reserved "False" >> return False)

instance Parse a => Parse (Maybe a) where
  parser = (Lex.reserved "Nothing" >> return Nothing)
       <|> fmap Just (Lex.reserved "Just" >> parser)

instance (Ord a, Parse a) => Parse (Set a) where
  parser = fmap Set.fromList (braces parser)

instance Parse a => Parse [a] where
  parser = list parser

instance (Parse a, Parse b) => Parse (a, b) where
  parser = Lex.parens parser'
    where parser' :: Parser (a, b)
          parser' = do a <- parser
                       Lex.comma
                       b <- parser
                       return (a, b)

instance (Parse a, Parse b, Parse c) => Parse (a, b, c) where
  parser = Lex.parens parser'
    where parser' :: Parser (a, b, c)
          parser' = do a <- parser
                       Lex.comma
                       b <- parser
                       Lex.comma
                       c <- parser
                       return (a, b, c)

instance (Parse a, Parse b, Parse c, Parse d) => Parse (a, b, c, d) where
  parser = Lex.parens parser'
    where parser' :: Parser (a, b, c, d)
          parser' = do a <- parser
                       Lex.comma
                       b <- parser
                       Lex.comma
                       c <- parser
                       Lex.comma
                       d <- parser
                       return (a, b, c, d)

instance (Parse a, Parse b, Parse c, Parse d, Parse e) => Parse (a, b, c, d, e) where
  parser = Lex.parens parser'
    where parser' :: Parser (a, b, c, d, e)
          parser' = do a <- parser
                       Lex.comma
                       b <- parser
                       Lex.comma
                       c <- parser
                       Lex.comma
                       d <- parser
                       Lex.comma
                       e <- parser
                       return (a, b, c, d, e)

instance (Parse a, Parse b, Parse c, Parse d, Parse e, Parse f) => Parse (a, b, c, d, e, f) where
  parser = Lex.parens parser'
    where parser' :: Parser (a, b, c, d, e, f)
          parser' = do a <- parser
                       Lex.comma
                       b <- parser
                       Lex.comma
                       c <- parser
                       Lex.comma
                       d <- parser
                       Lex.comma
                       e <- parser
                       Lex.comma
                       f <- parser
                       return (a, b, c, d, e, f)


-- @ Signature

module Imogen.Util.Lex 
  ( -- @ Parser constructors
    genParser
  , makeParser
  , genFileParser
  , makeFileParser
    -- @ Lexers
  , upperIdentifier
  , lowerIdentifier
  , identifier
  , int
  , integer
  , reserved
  , reservedOp
  , operator
  , parens
  , brackets
  , braces
  , dot
  , comma
  , colon
  , symbol
  )
where

-- @ Imports

import Prelude 
import qualified Imogen.Util.Monad as M
import qualified Text.ParserCombinators.Parsec as P
import Text.ParserCombinators.Parsec (Parser)
import qualified Text.ParserCombinators.Parsec.Token as T
import qualified Text.ParserCombinators.Parsec.Language as L
import qualified Text.ParserCombinators.Parsec.Char as C

-- @ Lexer

-- Use the Haskell lexer for identifier conventions.

opChars :: String
opChars = "!@#$%^&*=+-:<>◃ⓕⓦ⊛⋆"

lang :: L.LanguageDef ()
lang = L.emptyDef 
  { L.reservedOpNames = [ "⊃", "->", "⊸", "o-o", "-o", "o-", "&", "∧"
                        , "⊗", "⊕", "□", "◇", "|", "⊢", "⊧", "≡", ":" ] 
  , L.reservedNames = [ "⊤", "⊥", "0", "1", "∀", "!", "↑", "↓", "·" ]
  , L.commentLine = "%"
  , L.opStart = C.oneOf opChars
  , L.opLetter = C.oneOf opChars
  }

lexer :: T.TokenParser () 
lexer = T.makeTokenParser lang

whiteSpace :: Parser ()
whiteSpace = T.whiteSpace lexer

comma, colon, dot :: Parser ()
comma = M.ignore $ T.comma lexer
colon = M.ignore $ T.colon lexer
dot = M.ignore $ T.dot lexer

parens, brackets, braces :: Parser a -> Parser a
parens = T.parens lexer
brackets = T.brackets lexer
braces = T.braces lexer

reservedOp :: String -> Parser ()
reservedOp = T.reservedOp lexer

operator :: Parser String
operator = T.operator lexer

identifier :: Parser String
identifier = T.identifier lexer

symbol :: String -> Parser String
symbol = T.symbol lexer

lowerIdentifier :: Parser String
lowerIdentifier = T.lexeme lexer 
                   (do x <- C.lower
                       xs <- P.many (L.identLetter lang)
                       return $ x : xs)

upperIdentifier :: Parser String
upperIdentifier = T.lexeme lexer
                   (do x <- C.upper
                       xs <- P.many (L.identLetter lang)
                       return $ x : xs)

reserved :: String -> Parser ()
reserved = T.reserved lexer

integer :: Parser Integer
integer = T.integer lexer

int :: Parser Int
int = fmap fromIntegral integer

-- @ Parsers

genParser :: Parser () -> Parser a -> String -> a
genParser space p input = 
  case P.runParser p' () "" input of
    Left err -> error $ "Parse error: " ++ show err
    Right e -> e
 where p' = do space
               x <- p
               P.eof 
               return x

makeParser :: Parser a -> String -> a
makeParser = genParser whiteSpace

genFileParser :: Parser () -> Parser a -> String -> IO a
genFileParser space p file = 
  do res <- P.parseFromFile p' file 
     case res of 
       Left err -> fail $ show err
       Right e -> return e
 where p' = do space
               x <- p
               P.eof 
               return x

makeFileParser ::Parser a -> String -> IO a
makeFileParser = genFileParser whiteSpace


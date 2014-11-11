
-- @ Pragmas

{-# LANGUAGE DeriveDataTypeable #-}

-- @ Signature

module Imogen.Util.Impossible
  ( Impossible(..)
  , throwImpossible
  , catchImpossible
  )
where

-- @ Imports

import Prelude 
import qualified Control.Exception as Exn
import Data.Typeable

-- @ Impossible

{- 
| "Impossible" errors, annotated with a file name and a line
  number corresponding to the source code location of the error.
-} 

data Impossible = Impossible String Integer 
  deriving Typeable

instance Show Impossible where
  show (Impossible file line) = unlines
   [ "An internal error has occurred. Please report this as a bug."
   , "Location of the error: " ++ file ++ ":" ++ show line
   ]

instance Exn.Exception Impossible where

-- | Abort by throwing an \"impossible\" error. You should not use this
--   function directly. Instead use the macro in @undefined.h@.

throwImpossible :: Impossible -> a
throwImpossible = Exn.throw

-- | Catch an "impossible" error, if possible.

catchImpossible :: IO a -> (Impossible -> IO a) -> IO a
catchImpossible = Exn.catch

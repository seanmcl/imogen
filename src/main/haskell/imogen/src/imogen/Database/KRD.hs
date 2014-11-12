
-- | Kept rule database.

-- @ Pragmas

{-# LANGUAGE CPP #-} 

-- @ Signature

module Imogen.Database.KRD
  ( -- * Database
    Db
  , empty
    -- * Operations
  , insert
  , insertMany
  , null
  , select
  , size
  , print
  )
where

-- @ Imports 

#include "../../undefined.h" 

import Imogen.Util.Prelude hiding (null) 
import Imogen.Print (Print, pPrint)
import qualified Imogen.Rule as Rule
import Imogen.Rule (Rule)
import Imogen.Sig (Σ)
import qualified Imogen.Util.Print as PP
import qualified Imogen.Util.Seq as Seq
import Imogen.Util.Seq (Seq, (|>), ViewL(..))

-- @ DB

-- | Kept rule database.
newtype Db = Db { rseq :: Seq Rule }
  deriving (Show)

-- | The empty database.
empty :: Db
empty = Db Seq.empty

-- | Number of rules in the database.
size :: Db -> Int
size = Seq.length . rseq

-- | Is the database empty?
null :: Db -> Bool
null = Seq.null . rseq

-- | Insert a rule into the database.
insert :: Rule -> Db -> Db
insert r (Db db) = Db $ db |> r

-- | Insert a list of rules into the database.
insertMany :: [Rule] -> Db -> Db
insertMany = flip (foldr insert)

-- | Grab the next rule from the database and remove it.
select :: Db -> (Rule, Db)
select (Db db) = case Seq.viewl db of
  r :< rs -> (r, Db rs)
  _ -> error "select: empty"

-- | Each sequent has its own context, so a database is not a 'Imogen.Print.Print' instance.
print :: Σ -> Db -> Doc
print sig (Db rs) = PP.vcat $ map (\r -> pPrint sig (Rule.context r) r) (Seq.toList rs)


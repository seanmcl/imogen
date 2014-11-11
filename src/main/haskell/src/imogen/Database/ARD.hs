
-- | Active rule database.

-- @ Signature

module Imogen.Database.ARD
  ( -- * Database 
    Db
  , empty
    -- * Operations
  , insert
  , apply
  , size
  , print
  )
where

-- @ Imports 

import Imogen.Util.Prelude
import Imogen.Print (Print, pPrint)
import qualified Imogen.Rule as Rule
import Imogen.Rule (Rule)
import Imogen.Sig (Σ)
import qualified Imogen.Seq as Seq
import Imogen.Seq (Seq)
import qualified Imogen.Util.Print as PP

-- @ ARD

-- | Active rule database.
newtype Db = Db [Rule]
  deriving (Show)

-- | The empty database.
empty :: Db
empty = Db []

-- | Number of rules currently stored.
size :: Db -> Int
size (Db l) = length l

-- | Insert a rule into the database.
insert :: Rule -> Db -> Db
insert r (Db db) = Db (r : db)

-- | Apply all the rules in the database to the given sequent.
apply :: Rule.Class s m => Seq -> Db -> m ([Seq], [Rule])
apply δ (Db db) = do
  res <- mapM (Rule.apply δ) db
  return $ foldr classify ([], []) res
    where classify (Left q) (qs, rs) = (q ++ qs, rs)
          classify (Right r) (qs, rs) = (qs, r ++ rs)

-- | Each sequent has its own context, so a database is not a 'Imogen.Print.Print' instance.
print :: Σ -> Db -> Doc
print sig (Db rs) = PP.vcat (map (\r -> pPrint sig (Rule.context r) r $$ PP.space) rs)

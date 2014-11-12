
-- | Kept sequent database.

-- @ Signature

module Imogen.Database.KSD
  ( -- * Database
    Db
  , empty
    -- * Operations
  , insert
  , null
  , select
  , size
  , subsumes
  , print
  )
where

-- @ Imports 

import Imogen.Util.Prelude hiding (null) 
import qualified Control.Monad as M
import qualified Data.Foldable as Fold
import Imogen.Print (Print, pPrint)
import qualified Imogen.Rule as Rule
import qualified Imogen.Seq as Seq
import Imogen.Seq (Seq)
import Imogen.Sig (Σ)
import qualified Imogen.Util.Print as PP
import qualified Imogen.Util.Seq as Sequence
import Imogen.Util.Seq (ViewL(..), (|>))
import Imogen.Util.Three (Three(..))

-- @ DB

type Q a = Sequence.Seq a

-- | Kept sequent database.
data Db = Db (Q Seq)
  deriving (Show)

-- | The empty database.
empty :: Db
empty = Db Sequence.empty

-- | Is the database empty?
null :: Db -> Bool
null (Db l) = Sequence.null l

-- | Number of sequents in the database.
size :: Db -> Int
size (Db l) = Sequence.length l

-- | Grab the next sequent from the database and remove it.
select :: Db -> (Seq, Db)
select (Db q) = case Sequence.viewl q of
  x :< xs -> (x, Db xs)
  _ -> error "select: empty" 

-- | Insert a sequent into the database
insert :: Seq -> Db -> Db
insert x (Db q) = Db $ q |> x

-- | Does the database subsume an input sequent.
subsumes :: Rule.Class s m => Db -> Seq -> m Three
subsumes (Db q) x = Fold.foldlM (\t x' -> fmap (foldfn t) (Seq.subsumes x' x)) No q
   where 
    foldfn _ Yes = Yes
    foldfn Yes _ = Yes
    foldfn Unsure _ = Unsure
    foldfn _ Unsure = Unsure
    foldfn No No = No

-- | Each sequent has its own context, so a database is not a 'Imogen.Print.Print' instance.
print :: Σ -> Db -> Doc
print sig (Db qs) = PP.vcat $ map (\q -> pPrint sig (Seq.context q) q) (Sequence.toList qs)


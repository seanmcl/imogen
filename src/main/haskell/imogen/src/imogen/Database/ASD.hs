
-- | Active sequent database.

-- @ Pragmas

{-# LANGUAGE CPP #-} 

-- @ Signature 

module Imogen.Database.ASD
  ( -- * Database 
    Db
  , empty
    -- * Operations
  , insert
  , delete
  , apply
  , size
  , subsumes
  , subsumed
  , mightSubsumeGoal
  , print
  )
where

-- @ Imports 

#include "../../undefined.h" 

import Imogen.Util.Prelude
import qualified Data.List as List
import Imogen.Print (Print, pPrint)
import qualified Imogen.Rule as Rule
import Imogen.Rule (Rule)
import qualified Imogen.Seq as Seq
import Imogen.Seq (Seq, Id)
import Imogen.Sig (Σ)
import qualified Imogen.Util.Monad as M
import qualified Imogen.Util.Print as PP
import qualified Data.Set as Set
import Imogen.Util.Three (Three(..))

-- @ DB

-- | Active sequent database.

-- The first list is all the sequents.  The second is that subset that
-- might subsume the goal.  In Db q q', q' ⊆ q 
data Db = Db [Seq] [Seq]
  deriving (Show)

-- | The empty database.
empty :: Db
empty = Db [] []

-- | The number of sequents in the database.
size :: Db -> Int
size (Db l _) = length l

-- | 'insert' (q, Unsure) adds q to the unsure list
insert :: Rule.Class s m => (Seq, Bool) -> Db -> m Db
insert (q, b) db = do
  sids <- subsumed q db
  let (Db qs qs') = deleteMany sids db
  return $ Db (q : qs) (if b then q : qs' else qs')

-- | Delete a sequent.
delete :: Id -> Db -> Db
delete uid (Db l1 l2) = Db (filter f l1) (filter f l2)
 where
  f q = Seq.uid q /= uid

-- | Delete a bunch of sequents.
deleteMany :: Set Id -> Db -> Db
deleteMany ρ db = Set.fold delete db ρ

-- | Apply a rule to all the sequents in the database.
apply :: Rule.Class s m => Rule -> Db -> m ([Seq], [Rule])
apply δ (Db db _) = do
  res <- mapM (flip Rule.apply δ) db
  return $ foldr classify ([], []) res
    where classify (Left q) (qs, rs) = (q ++ qs, rs)
          classify (Right r) (qs, rs) = (qs, r ++ rs)

-- | Return the sequents that may subsume the goal.
mightSubsumeGoal :: Db -> [Seq]
mightSubsumeGoal (Db _ qs) = qs

-- | Does the database subsume an input sequent?
-- | FIXME: Get out of this loop early if you hit a Yes.  Exceptions?
subsumes :: Rule.Class s m => Db -> Seq -> m Three
subsumes (Db qs _) q = do
  bs <- M.mapM (flip Seq.subsumes q) qs
  return $ List.foldl' foldfn No bs
   where 
    foldfn _ Yes = Yes
    foldfn Yes _ = Yes
    foldfn Unsure _ = Unsure
    foldfn _ Unsure = Unsure
    foldfn No No = No

-- | Return all sequents subsumed by the input sequent.
subsumed :: Rule.Class s m => Seq -> Db -> m (Set Id)
subsumed q (Db qs _) = do
  subQs <- M.filterM (fmap ((==) Yes) . Seq.subsumes q) qs
  return $ foldr (Set.insert . Seq.uid) Set.empty subQs
  
-- | Each sequent has its own context, so a database is not a 'Imogen.Print.Print' instance.
print :: Σ -> Db -> Doc
print sig (Db qs _) = PP.vcat $ map (\q -> pPrint sig (Seq.context q) q) qs

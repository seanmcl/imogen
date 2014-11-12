
-- | The Imogen database.

-- @ Pragmas

{-# LANGUAGE Rank2Types #-} 

-- @ Signature 

module Imogen.Database 
  ( Db
  , prove
  ) 
where

-- @ Imports

import Imogen.Util.Prelude 
import qualified Control.Monad.State as State
import qualified Imogen.Class as Class
import qualified Imogen.Constr as C
import qualified Imogen.Ctx as Ctx
import qualified Imogen.Database.ASD as ASD
import qualified Imogen.Database.ARD as ARD
import qualified Imogen.Database.KSD as KSD
import qualified Imogen.Database.KRD as KRD
import qualified Imogen.Global as Global
import qualified Imogen.Misc as Misc
import Imogen.Misc (Output)
import qualified Imogen.Proof as Proof
import Imogen.Print (Print, pPrint)
import qualified Imogen.Rule as Rule
import Imogen.Rule (Rule)
import qualified Imogen.Seq as Seq
import Imogen.Seq (Seq)
import qualified Imogen.Sig as Sig
import Imogen.Sig (Σ)
import qualified Imogen.Util.Log as Log
import qualified Imogen.Util.Monad as M
import qualified Imogen.Util.Print as PP
import Imogen.Util.Three (Three(..), (|||))

-- @ Mode 

-- A mode tells whether to select a sequent or a rule from the 
-- database.

data Mode = PickRule | PickSeq
  deriving Show

instance PP.Print Mode where
  pPrint = PP.text . show

oppm :: Mode -> Mode
oppm PickRule = PickSeq
oppm PickSeq = PickRule

-- @ Database 

{- |
A database has 4 collections.  

  1) Kept sequents are the sequents that have not yet been considered for
     matching.

  2) Active sequents have been fully applied to the existing active rules

  3) Kept rules are the rules that have not yet been considered for matching.

  4) Active rules are the rules that have been applied to the active sequents.

It also remembers what it should pick next, a rule or a sequent.
-} 

data Db = Db { keptSeqs :: KSD.Db
             , activeSeqs :: ASD.Db
             , keptRules :: KRD.Db
             , activeRules :: ARD.Db
             , goal :: Seq
             , mode :: Mode
             , iters :: Integer
             }
  deriving Show

empty :: Seq -> Db 
empty g = Db KSD.empty ASD.empty KRD.empty ARD.empty g PickSeq 0

saturated :: Db -> Bool
saturated db = KSD.null (keptSeqs db) && KRD.null (keptRules db)

getMode :: Db -> Mode
getMode db 
  | KSD.null (keptSeqs db) = PickRule 
  | KRD.null (keptRules db) = PickSeq 
  | otherwise = mode db

selectSeq :: Db -> (Seq, Db)
selectSeq db = 
  let (q, ksd) = KSD.select $ keptSeqs db
  in (q, db { keptSeqs = ksd })

selectRule :: Db -> (Rule, Db)
selectRule db = 
  let (r, krd) = KRD.select $ keptRules db
  in (r, db { keptRules = krd })

applySeq :: Rule.Class s m => Seq -> Db -> m ([Seq], [Rule])
applySeq q = ARD.apply q . activeRules

applyRule :: Rule.Class s m => Rule -> Db -> m ([Seq], [Rule])
applyRule r = ASD.apply r . activeSeqs

-- Check for sequent subsumption
insertSeq :: Rule.Class s m => Seq -> Db -> m Db
insertSeq q db = do
  sig <- Sig.get
  -- Check to see if the sequent is OK.  If not, toss it.
  if not $ Class.ok sig (Seq.context q) q then return db else do
  s <- subsumes db q
  case s of 
    Yes -> do
      Log.debugM' "Database.insertSeq" $ PP.hcat [PP.text "Seq ", PP.pPrint (Seq.uid q), PP.text " is subsumed"]
      return db
    _ -> return $ db { keptSeqs = KSD.insert q (keptSeqs db) }

-- Check for rule subsumption
insertRule :: forall s m. Rule.Class s m => Rule -> Db -> m Db
insertRule r db = do
  sig <- Sig.get
  c <- ruleConcl r
  -- Check to see if the sequent is OK.  If not, toss it.
  if not $ Class.ok sig (Seq.context c) c then return db else do
  s <- subsumes db c
  case s of 
    Yes -> do
      Log.debugM' "Database.insertRule" $ PP.hcat [PP.text "Rule ", PP.pPrint (Rule.uid r), PP.text " is subsumed"]
      return db
    _ -> return $ db { keptRules = KRD.insert r (keptRules db) }
 where
  ruleConcl :: Rule -> m Seq
  ruleConcl rule = 
    let Rule.RSeq ד γ = Rule.concl rule
        ψ = Rule.constrs rule
        ctx = Rule.context rule
    in Seq.new ctx ψ ד γ -- XXX add world constraints

tick :: Db -> Db
tick db = db { iters = iters db + 1, mode = oppm (mode db) }

subsumes :: Rule.Class s m => Db -> Seq -> m Three
subsumes db q = do
  -- Log.debugM "Database.subsumes" "trying active"
  b1 <- ASD.subsumes (activeSeqs db) q
  -- Log.debugM "Database.subsumes" "trying kept"
  b2 <- KSD.subsumes (keptSeqs db) q
  return $ b1 ||| b2

-- @ Step 

step :: forall s m. Rule.Class s m => Db -> m Output
step db = do
  sig <- Sig.get
  Log.noticeM' "Database.step" $ summary db
  Log.debugM' "Database.step" $ 
    PP.text "step>" <+> (PP.text "mode =" <+> PP.pPrint (mode db) $$ print sig db)
  -- If both kept databases are exhausted, the goal is unprovable.
  if saturated db then
    case ASD.mightSubsumeGoal (activeSeqs db) of
      [] -> return $ Misc.No 
      qs -> do 
        Log.infoM' "Database.step" $ PP.text "The following sequents might subsume the goal, but I can't tell." $$ PP.vcat (map (\q -> pPrint sig (Seq.context q) q) qs)
        return $ Misc.Maybe
   else 
  -- Otherwise make sure you draw an existing sequent or rule
   case getMode db of 
    -- Grab a kept sequent
    PickSeq -> let (q, db1) = selectSeq db in do
      Log.debugM' "Database.step" $ PP.text "Selected sequent:" <+> PP.pPrint (Seq.uid q)
      (newQs, newRs) <- applySeq q db1
      -- Note, use 'subsumes db' rather than 'subsumes db1' so we make sure new 
      -- sequents subsumed by q are excluded.
      (subsumedQs, newQs') <- M.partitionM (fmap (== Yes) . subsumes db) newQs
      Log.debugM' "Database.step" $ PP.text "Subsumed:" <+> PP.list (map (PP.pPrint . Seq.uid) subsumedQs)
      (newQs'', _) <- fmap (unzip . concat) (mapM Seq.contract newQs')
      b <- M.any (\q' -> fmap (== Yes) (Seq.subsumes q' $ goal db)) newQs''
      if b
       then do 
         Log.infoM "Database.step" "Found the goal"
         return $ Misc.Yes Proof.dummy
       else do
         db2 <- M.foldrM insertSeq db1 newQs''
         -- We need to insert the chosen sequent *after* doing the subsumption tests
         -- so that it doesn't subsume itself!
         s <- Seq.subsumes q (goal db)
         -- Backward subsumption happens here (in ASD.insert)
         asd <- ASD.insert (q, s == Unsure) (activeSeqs db)
         let db3 = db2 { activeSeqs = asd }
         db4 <- M.foldrM insertRule db3 newRs  
         step $ tick db4
    PickRule -> let (r, db1) = selectRule db in do
      Log.debugM' "Database.step" $ PP.text "Selected rule:" <+> PP.pPrint (Rule.uid r)
      (newQs, newRs) <- applyRule r db1
      (subsumedQs, newQs') <- M.partitionM (fmap (== Yes) . subsumes db) newQs
      Log.debugM' "Database.step" $ PP.text "Subsumed:" <+> PP.list (map (PP.pPrint . Seq.uid) subsumedQs)
      (newQs'', _) <- fmap (unzip . concat) (mapM Seq.contract newQs')
      b <- M.any (\q' -> fmap (== Yes) (Seq.subsumes q' $ goal db)) newQs''
      if b
       then do 
         Log.infoM "Database.step" "Found the goal"
         return $ Misc.Yes Proof.dummy
       else do
         db2 <- M.foldrM insertSeq db1 newQs''
         let db3 = db2 { activeRules = ARD.insert r (activeRules db) }
         db4 <- M.foldrM insertRule db3 newRs  
         step $ tick db4

-- @ Top

-- | Given a list of initial sequents and rules, search for the goal.
prove :: Rule.Class s m => [Seq] -> [Rule] -> Seq -> m Output
prove seqs rules gl = do
  -- First make sure the goal is non-contradictory
  sig <- Sig.get
  glob <- Global.get
  -- Log.debugM' "Database.prove" $ PP.text "Input sig" 
  cont <- State.evalStateT (C.contradictory sig $ (Global.constrs glob)) Ctx.empty
  if cont then do
    Log.infoM "Database.prove" "The globals antecedents are inconsistent!" 
    return (Misc.Yes Proof.dummy)
   else do
  -- Otherwise make the empty database
  let db0 = empty gl
  -- Add the rules
  db1 <- M.foldrM insertRule db0 rules
  -- Add the sequents
  (cseqs, _) <- fmap (unzip . concat) (mapM Seq.contract seqs)
  db <- M.foldrM insertSeq db1 cseqs
  -- Check if any of the initial sequents (or their contracts)
  -- subsume the goal
  Log.infoM "Database.prove" "Checking if the initial sequents subsume the goal." 
  s <- subsumes db gl
  case s of 
    Yes -> do 
      Log.infoM "Database.prove" "The goal is subsumed!"
      return (Misc.Yes Proof.dummy)
    -- Otherwise start proving
    _ -> do 
      Log.infoM "Database.prove" "Not subsumed! Starting the database loop..."
      step db

-- @ Printing 

summary :: Db -> Doc
summary db = 
  PP.text "[Summary]" <+> 
    PP.hcat [ PP.text "it: ", PP.integer $ iters db
            , PP.text ", ks: ", PP.int $ KSD.size $ keptSeqs db
            , PP.text ", as: ", PP.int $ ASD.size $ activeSeqs db
            , PP.text ", kr: ", PP.int $ KRD.size $ keptRules db
            , PP.text ", ar: ", PP.int $ ARD.size $ activeRules db
            ]

print :: Σ -> Db -> Doc
print sig db = 
    PP.vcat [ PP.text "[Database]" 
            , PP.space <+> PP.vcat
              [ summary db
              , PP.text "[Detail]"
              , PP.space <+> PP.vcat
                [ PP.text "[KS]" <+> KSD.print sig (keptSeqs db)
                , PP.text "[AS]" <+> ASD.print sig (activeSeqs db)
                , PP.text "[KR]" <+> KRD.print sig (keptRules db)
                , PP.text "[AR]" <+> ARD.print sig (activeRules db)
                , PP.text "[Mode]" <+> PP.pPrint (mode db)
                ]
              ]
            ]

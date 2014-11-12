
-- FIXME: DOC
-- | Antecedents                                                                 

-- @ Pragmas

{-# LANGUAGE Rank2Types #-} 

-- @ Signature

module Imogen.Ants 
  ( Ants 
  , empty
  , null
  , size
  , insert
  , fromList
  , singleton
  , select
  , union
  , difference
  , match
  , contract
  , unify
  , print
  )
where

-- @ Imports

import Imogen.Util.Prelude hiding (null)
import qualified Control.Monad.Reader as Reader
import Control.Monad.Reader (ReaderT)
import qualified Control.Monad.State as State
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Imogen.Class as Class
import Imogen.Class (UnifClass)
import Imogen.CSubst(Θ, (✴), (○), ι)
import qualified Imogen.Ctx as Ctx
import Imogen.Ctx(Γ)
import Imogen.Modal (Atom(..), Worlds(worlds))
import Imogen.Param (Params(..), Petrify(..))
import Imogen.Pred(Pred)
import Imogen.Rename (Rename, rename)
import qualified Imogen.Subst as Subst
import qualified Imogen.Util.List as List
import qualified Imogen.Util.Log as Log
import Imogen.Util.Log (Log)
import qualified Imogen.Util.Monad as M
import qualified Imogen.Util.Print as PP
import Imogen.Util.Print (Print, pPrint)
import qualified Imogen.Util.Set as Set
import Imogen.Util.Set((∅), (∪), (\\))
import qualified Imogen.Var as Var
import Imogen.Var (Vars)

-- @ Antecedents

-- | The antecedents Γ is represented by a map from paths to path atoms.  

newtype Ants = A { amap :: Map Pred (Set Atom) }
  deriving (Eq, Ord, Show)

instance Vars Ants where
  vars (A m) = Map.fold (Set.union . Var.vars) (∅) m
  free = Var.vars

instance Params Ants where
  params (A m) = Map.fold (Set.union . params) (∅) m

instance Rename Ants where
  rename (A m) = rename m >>= return . A

instance Class.Normalize Ants where
  normalize sig ctx (A m) = A $ Class.normalize sig ctx m

instance Class.OK Ants where
  ok sig ctx (A m) = Class.ok sig ctx m

instance Petrify Ants where
  petrify ρ (A m) = A $ Map.map (petrify ρ) m

instance Worlds Ants where
  worlds (A m) = worlds m

-- It's safe to use 32 bit Ints here.  We can't do much with sequents
-- that are over a gigabyte!

size :: Ants -> Int
size (A m) = Map.fold (\x y -> Set.size x + y) 0 m

empty :: Ants
empty = A Map.empty

null :: Ants -> Bool
null (A m) = Map.null m

insert :: Atom -> Ants -> Ants
insert a@(Rel p _ _) (A m) = A $ Map.insert p (Set.insert a set) m
  where set = Map.findWithDefault (∅) p m 

singleton :: Atom -> Ants
singleton = flip insert empty 

-- | Extend a sequent with more antecedents.

union :: Ants -> Ants -> Ants
union (A ד1) (A ד2) = A $ Map.unionWith Set.union ד1 ד2

difference :: Ants -> Ants -> Ants
difference (A ד1) (A ד2) = A $ Map.differenceWith f ד1 ד2
  where f s1 s2 = 
          let s = Set.difference s1 s2 in
          if Set.null s then Nothing
          else Just s

-- | Grab an arbitrary element out of ד 

select :: Ants -> Atom
select (A ד) = Set.findMin τs
  where (_, τs) = Map.findMin ד

-- | Since paths can contain variables, application can really mess
-- with antecedents by equating branches of the map.  This is a bit expensive.

instance Subst.Apply Ants where
  ד ✴ θ = foldr insert empty elems
    where elems = toList ד Subst.✴ θ

toList :: Ants -> [Atom]
toList (A ד) = concatMap Set.toList (Map.elems ד)

fromList :: [Atom] -> Ants
fromList = foldr insert empty

-- @ Matching 

{- 
Antecedent subsumption is a bit tricky.  We attempt to match
Γ1 to Γ2 by nondeterministically choosing an injective map from
Γ1 to Γ2, and then simultaneously matching the paired atoms.

match ד1 ד2 returns all substitutions θ such that θ(ד1) = ד2.

p (c)
p (X)
p (f(X))

q (d, X)
q (Y, d)

ts = [ [ [c], [X], [f(X)] ]
     , [ [d, X], [Y, d] ]
     ]     

ts' = [ [ [c], [X'], [f(X')] ]
      , [ [d, X'], [Y', d] ]
      ]     

eqs0 = [ [ [ ([c], [c])
           , ([X], [X'])    
           , ([f(X)], [f(X')])
           ] 
         , ...
         ]
        , [ [ ([d, X], [d, X'])
              ([Y, d], [Y', d])
            ]
        ,   [ ([d, X], [Y', d])
             ([Y, d], [d, X'])
            ]
           ]
        ]

eqs = [ [ ([c], [c])
        , ([X], [X'])    
        , ([f(X)], [f(X')])
        ] 
        ++
        [ ([d, X], [d, X'])
          ([Y, d], [Y', d])
        ]
      , [ ([c], [c])
        , ([X], [X'])    
        , ([f(X)], [f(X')])
        ] 
        ++
        [ ([d, X], [Y', d])
          ([Y, d], [d, X'])
        ]
      , ...
     ]
-} 

match :: UnifClass s m => Γ -> Ants -> Ants -> m [(Θ, Γ)]
match ctx _a1@(A ד1) _a2@(A ד2) = 
  let (ps, ats) = second (map Set.toList) $ unzip $ Map.toList ד1
      ats, ats' :: [[Atom]]
      ats' = map (maybe [] Set.toList . flip Map.lookup ד2) ps
      eqs0 :: [[[(Atom, Atom)]]]
      eqs0 = zipWith List.allInjectiveMaps ats ats'
      eqs :: [[(Atom, Atom)]]
      eqs = map concat $ List.allCombs eqs0
  in do 
    let fn qs = do
          (res, ctx') <- State.runStateT (Class.match qs) ctx
          case res of 
            Nothing -> return Nothing
            Just θ -> return $ Just (θ, ctx')
    θs <- mapM fn eqs
    let res = Maybe.catMaybes θs
    -- Log.debugM' "Ants.match" $ PP.vcat [ PP.text "Input:" <+> pPrint (a1, a2)
    --                                    , PP.text "ps   :" <+> pPrint ps
    --                                    , PP.text "ats  :" <+> pPrint ats
    --                                    , PP.text "ats' :" <+> pPrint ats'
    --                                    , PP.text "Eqs  :" <+> pPrint eqs 
    --                                    , PP.text "Res  :" <+> pPrint res ]
    return res

-- @ Contraction 

{- 
Contraction is an exponential operation.  We can eliminate some of the
blowup by being careful about when and how we branch.  The simplest
way to do contraction would be to take all partitions of the 
antecedents and return a unifier for each set of partitions.  This
is a bad idea.  We can instead be more selective.  

1) First separate the ground antecedents from those with free variables.
   We can add those to the list of global antecedents, since they need
   not be contracted (though they may be contracted \emph{against})

2) It suffices to assume that all the relations have the same head symbol.
   The Ants data structure separates them in this way, and if we find
   a θ that partitions the 'p1' antecedents, we can then apply θ to
   the remaining antcedents and recursively use the same procedure for
   the rest.  One caveat here. Suppose that we have already unified 
   the 'p1' antecedents, yielding (θ, [p1(t1), ..., p1(tn)]) where
   the ti are distinct, and we have appled θ to the 'p2' antecedents.
   Say we get θ' from one partition of the 'p2's.  If 
   [p1(t1), ..., p1(tn)] θ' has fewer than n elements, say t1 ≗ t2,
   we can throw out that partition, since this case will be handled by
   the partition of the 'p1's that unified t1 and t2.

3) To find all partitions of the 'p1', we take one out (p1(t0))
   and find all partitions of the rest.  One of these looks like
   (θ, [p1(t1), ..., p1(tn)]).  Then we create the partitions:

   (θ, [p1(t0) θ, p1(t1), ..., p1(tn)])
   (θ ○ θ1, [p1(t1), ..., p1(tn)] θ1) where t0 ≗ t1 ↦ θ1 
   (θ ○ θ2, [p1(t1), ..., p1(tn)] θ2) where t0 ≗ t2 ↦ θ2
   ⋮
   (θ ○ θn, [p1(t1), ..., p1(tn)] θn) where t0 ≗ tn ↦ θn

   Note that we throw out any of the partitions where 
   the length of {[p1(t1), ..., p1(tn)] θ1} < n for the reason
   given above (or length {[p1(t0) θ, p1(t1), ..., p1(tn)]} < n+1
   for the first case.)  We also throw out any θ ○ θi if 
   θ ○ θi unifies some terms in an 'earlier' partition.  

4) The picture given so far ignores one complication.  That is
   the ground antecedents that were separated in step 1.  We 
   need to be able to contract against those ground antecedents.
   We thus need, for each
   (θ ○ θi, [p1(t1), ..., p1(tn)] θi) to then try to unify the
   ti with the ground antecedents p1(c1), etc.

-} 

type Global = Ants

{- 
Cases:

p(X, X), p(X, c)
-----------------
    p(c, c)


p(X, c), p(X, d)
----------------
p(c, c), p(c, d)


p(X, c), p(X, d)
----------------
   p(c, c)
-} 

data R = R Global Γ (Θ -> Bool)

applyAndFilter :: Global -> Θ -> Ants -> Ants
applyAndFilter glob θ ants = difference (ants ✴ θ) glob

partitions :: forall s m. UnifClass s m => Pred -> (Θ, Γ, Set Atom) -> ReaderT R m [(Θ, Γ, Set Atom)]
partitions p inp@(θ, ctx, ts) = do 
  res <- do
   R glob _ ok <- Reader.ask
   -- separate the ground and non-ground antecedents
   let (gts, ngts) = Set.partition Var.ground ts
   -- if there are no non-ground antecedents, return
   if Set.null ngts then return [inp] else do
   -- otherwise grab a non-ground antecedent
   let (t, ngts') = Set.deleteFindMin ngts
   -- partition the remaining antecedents
   rest <- partitions p (θ, ctx, gts ∪ ngts')
   -- for each resulting partition, we either don't add t, or add it to exactly one equivalence class
   let -- mfn :: (Θ, Γ, Set Atom) -> ReaderT R m [(Θ, Γ, Set Atom)]
       mfn (θ1, ctx1, ts1) =
         -- one possibility is to not unify t with any of the ts1
         let ctx2 = Ctx.join ctx ctx1
             -- grab the global antecedents corresponding to 'p'
             globts = maybe Set.empty id (Map.lookup p $ amap glob)
             res1 = (θ1, ctx2, Set.insert (t ✴ θ1) ts1 \\ globts)
             -- otherwise we unify with exactly one of the ts1
             -- pfn :: Atom -> ReaderT R m (Maybe (Θ, Γ, Set Atom))
             pfn t' = do
               (res, ctx3) <- M.lift $ State.runStateT (Class.unify1 t t') ctx2
               case res of 
                 Nothing -> return Nothing
                 Just θ2 -> do
                   let θ3 = θ1 ○ θ2 
                       b = ok θ3 
                   -- Log.debugM' "Ants.partitions.ok" $ pPrint (θ3, b)
                   return $ if b then Just (θ3, ctx3, ts1 ✴ θ2 \\ globts) else Nothing
         in (res1 :) <$> M.mapMaybeM pfn (Set.toList $ ts1 ∪ gts ∪ globts)
   M.concatMapM mfn rest
  return res

contract1 :: forall s m. UnifClass s m => (Θ, Γ, Ants) -> Ants -> ReaderT R m [(Θ, Γ, Ants)]
contract1 acc@(θ, _, pants) ants = do
  res <- if null ants then return [acc] else do
         let ((p, ts), ants') = Map.deleteFindMin $ amap ants
         R glob ctx _ <- Reader.ask
         let globts = maybe Set.empty id (Map.lookup p $ amap glob)
         parts <- partitions p (θ, ctx, ts)
         let partfn (θ1, ctx1, ts1) = 
              let pants1 = applyAndFilter glob θ1 pants in
              if size pants1 /= size pants then return [] else
              let pants2 = A $ Map.insert p (ts1 \\ globts) (amap pants1) in
              contract1 (θ1, ctx1, pants2) (A ants')
         M.concatMapM partfn parts
  return res

contract :: UnifClass s m => Global -> (Θ -> Bool) -> Γ -> Ants -> m [(Θ, Γ, Ants)]
contract glob ok ctx ants = do
  res <- Reader.runReaderT (contract1 (ι, Ctx.empty, empty) ants) (R glob ctx ok)
  return res

-- @ Unification 

{- 
args1 = [ 1, 2, 3 ]
args2 = [ 4, 5 ]
sel1 = [ [], [1], [2], [3], [1, 2], [1, 3], [2, 3], [1, 2, 3] ]
eqs  = [ [[]]
       , [[(1, 4)], [(1, 5)]]
       , [[(2, 4)], [(2, 5)]]
       , [[(3, 4)], [(3, 5)]]
       , [[(1, 4), (2, 5)], [(1, 5), (2, 4)], 
       , [[(1, 4), (3, 5)], [(1, 5), (3, 4)], 
       , [[(2, 4), (3, 5)], [(3, 5), (2, 4)], 
       , []
       ]

gath = [ []
       , [(1, 4)]
       , [(1, 5)]
       , [(2, 4)]
       , [(2, 5)]
       , [(3, 4)]
       , [(3, 5)]
       , [(1, 4), (2, 5)]
       , [(1, 5), (2, 4)] 
       , [(1, 4), (3, 5)] 
       , [(1, 5), (3, 4)]
       , [(2, 4), (3, 5)]
       , [(3, 5), (2, 4)]
       ]
-} 

-- Return subsequences and their complements.
compSubsequences :: [a] -> [([a], [a])]
compSubsequences [] = [([], [])]
compSubsequences (x:xs) = 
  let subs = compSubsequences xs
      subs1 = map (second (x:)) subs
      subs2 = map (first (x:)) subs
  in subs1 ++ subs2

gather :: [Atom] -> [Atom] -> [([(Atom, Atom)], [Atom])]
gather args1 args2 = 
  let sel1 = compSubsequences args1 in
  concat $ map (\(used, unused) -> map (\x -> (x, unused)) (List.allInjectiveMaps used args2)) sel1

{- 
For simplicity, we would rather simply return substitutions and do
the set difference at the end.  Unfortunately this doesn't work in
the presence of constraints.  An equation like p(w1) ≗ p(w2) may
generate the constraints w1 ≡ w2, but the resulting substitution
will not make p(w1) ≡ p(w2).  Thus we have to mark, say, p(w1) as
being matched.

[Ants.unify: DEBUG]
  a1 : [↑, ∧l, ↓, ⊃l, ⊃r], [↑, ∧r, ↓, ⊃l, ⊃r]
  a2 : [↑, ∧l, ↓, ⊃l, ⊃r]
  res: []

ps  = [ [↑, ∧l, ↓, ⊃l, ⊃r],  [↑, ∧r, ↓, ⊃l, ⊃r] ]
ts  = [ [[]], [[]] ]
ts' = [ [[]], [] ]
cs  = [ [], [[]], [[]], [ [[]], [[]] ] ]
-} 

unify :: UnifClass s m => Γ -> Ants -> Ants -> m [(Θ, Γ, Ants)]
unify ctx qants@(A דq) rants@(A דr) = 
  let (ps, qats) = second (map Set.toList) $ unzip $ Map.toList דq
      qats, rats :: [[Atom]]
      rats = map (maybe [] Set.toList . flip Map.lookup דr) ps
      ws :: [[([(Atom, Atom)], [Atom])]]
      ws = zipWith gather qats rats
      eqs :: [([(Atom, Atom)], [Atom])]
      eqs = map (\l -> let (eqns, unused) = unzip l in (concat eqns, concat unused)) $ List.allCombs ws
      ufn (qs, unused) = do 
        (res, ctx') <- State.runStateT (Class.unify qs) ctx
        case res of
          Nothing -> return Nothing
          Just θ -> return $ Just (θ, ctx', fromList unused)
  in do 
    res <- Maybe.catMaybes <$> mapM ufn eqs
    Log.debugM' "Ants.unify" $ PP.vcat [ PP.text "qants :" <+> pPrint qants
                                       , PP.text "rants :" <+> pPrint rants
                                       , PP.text "ws    :" <+> pPrint ws
                                       , PP.text "eqs   :" <+> pPrint eqs
                                       -- , PP.text "res   :" <+> pPrint res
                                       ]
    return res

-- @ Printing

print :: (Atom -> PP.Doc) -> Ants -> PP.Doc
print f (A m) = if Map.null m then PP.text "·" else
    if length pps == 0 then PP.text ("Error: " ++ show m) else
    if length pps == 1 then head pps else
    PP.cat $ PP.commas pps
    where pps = concatMap (map f . Set.toList) (Map.elems m)

instance Print Ants where
  pPrint = print PP.pPrint

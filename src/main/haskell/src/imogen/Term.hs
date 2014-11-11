
{- | 
/Uninterpreted terms./

The terms type defined in this module is one of the primary data structures in Imogen.
Terms are represented as a simple tree.  No attempt was made at sharing or any
other efficiency optimization.  At least, not yet.  

The term type defined here is the /Rosetta Stone/ of the different languages 
that Imogen handles.  For instance, while frames and worlds in linear logic 
may be represented as some clever abstract type, they are translated to 'Term'
for storage.  The reason is that we need to mix terms of different sorts in
the same data structure, and this seemed like the obvious choice.  

Note that the infix function symbols are hard-coded.  Users cannot define
infix functions.
-} 

-- @ Pragmas

-- Orphan warning for parsing Vars and Params.  Ignore it.

{-# OPTIONS_GHC -fno-warn-orphans  #-} 
{-# LANGUAGE CPP, DeriveDataTypeable #-} 

-- @ Signature 

module Imogen.Term
  ( -- * Terms
    Term(..)
  , Args
  , func
    -- * Encoding\/Decoding
  , Encode(..)
    -- * Defaults
  , defaultWorld
  , defaultPrincipal
  )
where

-- @ Imports

#include "../undefined.h"

import Imogen.Util.Prelude 
import qualified Control.Monad as M
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Imogen.Func as Func
import Imogen.Func (Func, Funcs(..))
import qualified Imogen.Param as Param
import Imogen.Param (Param, Params, Freeze(..), Petrify(..))
import Imogen.Rename (Rename(rename))
import qualified Imogen.Util.Print as PP
import Imogen.Util.Print (Print, pPrint)
import qualified Imogen.Util.Set as Set
import Imogen.Util.Set ((∅))
import qualified Imogen.Var as Var
import Imogen.Var (Var, Vars)

-- @ Terms

-- | Terms as a tree.
data Term = Var Var
          | Param Param
          | Fn Func Args
  deriving (Eq, Ord, Show, Data, Typeable)

-- | A handy abbreviation when the types get large.
type Args = [Term]

-- | Grab the function symbol.
func :: Term -> Func
func (Fn f _) = f
func _ = __IMPOSSIBLE__ 

-- @@ Instances

instance Vars Term where
  vars (Var v) = Var.vars v
  vars (Param _) = (∅)
  vars (Fn _ τs) = foldr Set.union (∅) $ map Var.vars τs
  free = Var.vars

instance Params Term where
  params (Var _) = (∅)
  params (Param p) = Param.params p
  params (Fn _ τs) = foldr Set.union (∅) $ map Param.params τs

instance Funcs Term where
  arityFuncs (Fn f τs) = Set.insert (f, length τs) (arityFuncs τs)
  arityFuncs _ = Set.empty

instance Rename Term where
  rename (Var x) = rename x >>= return . Var 
  rename (Fn f ts) = rename ts >>= return . Fn f 
  rename a@(Param _) = return a

instance Freeze Term where
  freeze t = case t of
    Var x -> Param $ Var.freeze x
    Param _ -> t
    Fn f ts -> Fn f $ map freeze ts
  thaw ps t = case t of
    Var _ -> t
    Param p -> if Set.member p ps then Var $ Var.thaw p else t
    Fn f ts -> Fn f $ map (thaw ps) ts

instance Petrify Term where
  petrify ρ t = case t of
    Var _ -> t
    Param c -> maybe t (\f -> Fn f []) (Map.lookup c ρ)
    Fn f ts -> Fn f $ map (petrify ρ) ts

-- @ Encoding

{- | 
Encoding to and from a 'Term'.  This operation is fundamental to
encode morally different types like (commutative) linear worlds
(noncommutative) ordered worlds) in a single type.
-} 
class Encode a where
  encode :: a -> Term
  decode :: Term -> a

-- @ Printing

{- 
We can print terms without any knowledge of the infix status of 
the operators.  For prettier printing with context-sensitive 
terms and atoms, use the Class.Print class.
-} 

instance Print Term where
  pPrint t = case t of
    Var x -> pPrint x
    Param c -> pPrint c
    Fn f [] -> pPrint f <> PP.text "¹"
    Fn f ts -> 
      pPrint f <> PP.parens (PP.fcat $ 
          List.intersperse (PP.text ", ") (map pPrint ts))

-- @ Modal Worlds

-- | A (non-parsable) default world.
defaultWorld :: Term
defaultWorld = Fn (Func.make "$w0") []

-- | A (non-parsable) default principal.
defaultPrincipal :: Term
defaultPrincipal = Fn (Func.make "$p0") []

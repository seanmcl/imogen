
-- @ Signature

module Imogen.Test.Uninterp
  ( formulas
  , tests
  )
where

-- @ Imports

import Imogen.Util.Prelude 
import qualified Control.Monad.State as State
import qualified Imogen.Ctx as Ctx
import qualified Imogen.Misc as Misc
import Imogen.Parse ()
import qualified Imogen.Print as Print
import qualified Imogen.Prover as Prover
import qualified Imogen.Sig as Sig
import Imogen.Sig (Σ)
import qualified Imogen.Util.Print as PP
import qualified Imogen.Util.Parse as P
import qualified Imogen.Util.Test as Test
import Imogen.Util.Test (Test(..), (~:), (@?=))
import Imogen.Util.Three (Three(..))
import qualified System.IO.UTF8 as S

-- @ Tests

sig :: Σ 
sig = P.parse 
  "  { p, P, q, Q, a, b : Prop              \
  \  , p1, P1, q1, Q1   : U -> Prop         \
  \  , p2, P2, q2, Q2   : U -> U -> Prop    \
  \  , c, a1, a2, a3    : U                 \
  \  }                                      "

-- | Uninterpreted test formulas.
formulas:: [(String, String, Three)]
formulas = 
  [ ("t1", "⊤", Yes)
  , ("t2", "⊥", No)
  , ("t3", "p", No)
  , ("t4", "P", No)
  , ("t4'", "p ⊃ p", Yes)
  , ("t5", "p ⊃ q ⊃ p", Yes)
  , ("t6", "∀ X. p1(X) ⊃ p1(X)", Yes)
  , ("t7", "a ∨ b ⊃ b ∨ a", Yes)
  , ("t8", "¬ ¬ ((a ∧ b) ∨ (¬ a ∨ ¬ b))", Yes)
  , ("t9", "(∀ X1. ∃ Y1. p2(X1, Y1)) ⊃ (∃ Y2. ∀ X2. p2(X2, Y2))", No)
  , ("t10", "(∃ Y2. ∀ X2. p2(X2, Y2)) ⊃ (∀ X1. ∃ Y1. p2(X1, Y1))", Yes)
    -- @ Positive Atoms
  , ("p0", "⊥ ⊃ ⊤", Yes)
  , ("p1", "p ⊃ p", Yes)
  , ("p2", "p1(c) ⊃ p1(c)", Yes)
  , ("p3", "p ⊃ p ⊃ p", Yes)
  , ("p4", "p ⊃ q ⊃ p", Yes)
  , ("p5", "p ∧ q ⊃ p", Yes)
  , ("p6", "p ∧ q ⊃ q ∧ p", Yes)
  , ("p7", "p ⊃ ¬ ¬ p", Yes)
  , ("p8", "¬ ¬ p ⊃ p", No)
  , ("p9", "¬ a ∨ ¬ b ⊃ ¬ b ∨ ¬ a", Yes)
  , ("p10", "¬ ¬ ((a ∧ b ∧ c) ∨ (¬ a ∨ ¬ b ∨ ¬ c))", Yes)
  , ("p11", "p ⊃ ¬ p", No)
  , ("p12", "p ∧ q ⊃ r", No)
  , ("p13", "p ∨ q ⊃ p", No)
  , ("p14", "p1(c) ⊃ p1(c)", Yes) 
  -- @* Implication
  , ("p15", "(a ⊃ b ⊃ c) ⊃ (a ⊃ b) ⊃ (a ⊃ c)", Yes)
  , ("p47", "a ⊃ b ⊃ a", Yes)
  , ("p48", "a ⊃ a", Yes)
  -- @** Structural rules
  , ("p63", "(a ⊃ b ⊃ c) ⊃ (b ⊃ a ⊃ c)", Yes)
  , ("p64", "(a ⊃ a ⊃ b) ⊃ (a ⊃ b)", Yes)
  , ("p65", "b ⊃ (a ⊃ b)", Yes)
  -- @* Conjunction 
  , ("p66", "a ∧ b ⇔ b ∧ a", Yes)
  , ("p67", "a ∧ (b ∧ c) ⇔ (a ∧ b) ∧ c", Yes)
  , ("p68", "a ∧ a ⇔ a", Yes)
  -- @** ND 
  , ("p69", "a ⊃ b ⊃ a ∧ b", Yes)
  , ("p70", "a ∧ b ⊃ a", Yes)
  , ("p71", "a ∧ b ⊃ b", Yes)
  -- @** Seq 
  , ("p72", "(d ⊃ a) ⊃ (d ⊃ b) ⊃ (d ⊃ a ∧ b)", Yes)
  , ("p73", "(a ⊃ c) ⊃ (a ∧ b ⊃ c)", Yes)
  , ("p74", "(b ⊃ c) ⊃ (a ∧ b ⊃ c)", Yes)
  , ("p75", "(a ⊃ b ⊃ c) ⊃ (a ∧ b ⊃ c)", Yes)
  -- @** Other 
  , ("p76", "a ⊃ a ∧ b", No)
  , ("p77", "b ⊃ a ∧ b", No)
  -- @* Conjunction / Implication 
  , ("p78", "a ∧ b ⊃ c ⇔ a ⊃ b ⊃ c", Yes)
  , ("p79", "a ⊃ b ∧ c ⇔ (a ⊃ b) ∧ (a ⊃ c)", Yes)
  -- @* Disjunction 
  , ("p80", "a ∨ b ⇔ b ∨ a", Yes)
  , ("p81", "a ∨ (b ∨ c) ⇔ (a ∨ b) ∨ c", Yes)
  , ("p82", "a ∨ a ⇔ a", Yes)
  , ("p83", "a ∨ a ⊃ a", Yes)
  -- @** ND 
  , ("p84", "a ⊃ a ∨ b", Yes)
  , ("p85", "b ⊃ a ∨ b", Yes)
  , ("p86", "(a ⊃ c) ⊃ (b ⊃ c) ⊃ (a ∨ b ⊃ c)", Yes)
  -- @** Seq 
  , ("p87", "(d ⊃ a) ⊃ (d ⊃ a ∨ b)", Yes)
  , ("p88", "(d ⊃ b) ⊃ (d ⊃ a ∨ b)", Yes)
  , ("p89", "(a ⊃ c) ⊃ (b ⊃ c) ⊃ (a ∨ b ⊃ c)", Yes)
  -- @** Other 
  , ("p90", "a ∨ b ⊃ a", No)
  , ("p91", "a ∨ b ⊃ b", No)
  -- @* Disjunction / Implication 
  , ("p92", "a ∨ b ⊃ c ⇔ (a ⊃ c) ∧ (b ⊃ c)", Yes)
  , ("p93", "(a ⊃ b ∨ c) ⊃ (a ⊃ b) ∨ (a ⊃ c)", No)
  , ("p94", "(a ⊃ b ∨ c) ⊂ (a ⊃ b) ∨ (a ⊃ c)", Yes)
  -- @* Disjunction / Conjunction 
  , ("p95", "a ∨ (b ∧ c) ⇔ (a ∨ b) ∧ (a ∨ c)", Yes)
  , ("p96", "a ∧ (b ∨ c) ⇔ (a ∧ b) ∨ (a ∧ c)", Yes)
  -- @* Truth 
  -- @** ND 
  , ("p97", "⊤", Yes)
  -- @** Seq 
  , ("p98", "d ⊃ ⊤", Yes)
  , ("p99", "(⊤ ⊃ c) ⊃ c", Yes)
  -- @** Other 
  , ("p100", "⊤ ⊃ c ⇔ c", Yes)
  , ("p101", "a ⊃ ⊤ ⇔ ⊤", Yes)
  , ("p102", "a ∧ ⊤ ⇔ a", Yes)
  , ("p103", "a ∨ ⊤ ⇔ ⊤", Yes)
  -- @* Nohood 
  -- @** ND 
  , ("p104", "⊥ ⊃ c", Yes)
  -- @** Seq 
  , ("p105", "⊥ ⊃ c", Yes)
  -- @** Other 
  , ("p106", "⊥ ⊃ c ⇔ ⊤", Yes)
  , ("p107", "⊥ ⊃ (a ⊃ ⊥)", Yes)
  , ("p108", "a ∧ ⊥ ⇔ ⊥", Yes)
  , ("p109", "a ∨ ⊥ ⇔ a", Yes)
  , ("p110", "(a ⊃ ⊥) ⊃ ⊥", No)
  , ("p111", "⊥", No)
  -- @* Implication / Implication 
  , ("p112", "((a ⊃ b) ⊃ c) ⊃ (a ∨ c) ∧ (b ⊃ c)", No)
  , ("p113", "((a ⊃ b) ⊃ c) ⊂ (a ∨ c) ∧ (b ⊃ c)", Yes)
  -- @* Classical 
  , ("p114", "((a ⊃ ⊥) ⊃ ⊥) ⊃ a", No)
  , ("p115", "((a ⊃ b) ⊃ a) ⊃ a", No)
  , ("p116", "a ∨ (a ⊃ b)", No)
  , ("p171", "((a ⊃ ⊥) ⊃ ⊥) ⊃ a", No)
  , ("p172", "((a ⊃ b) ⊃ a) ⊃ a", No)
  , ("p173", "a ∨ (a ⊃ b)", No)
    -- @ Positive Atoms
  , ("n0", "⊥ ⊃ ⊤", Yes)
  , ("n1", "P ⊃ P", Yes)
  , ("n2", "P1(c) ⊃ P1(c)", Yes)
  , ("n3", "P ⊃ P ⊃ P", Yes)
  , ("n4", "P ⊃ Q ⊃ P", Yes)
  , ("n5", "P ∧ Q ⊃ P", Yes)
  , ("n6", "P ∧ Q ⊃ Q ∧ P", Yes)
  , ("n7", "P ⊃ ¬ ¬ P", Yes)
  , ("n8", "¬ ¬ P ⊃ P", No)
  , ("n9", "¬ A ∨ ¬ b ⊃ ¬ b ∨ ¬ A", Yes)
  , ("n10", "¬ ¬ ((A ∧ b ∧ c) ∨ (¬ A ∨ ¬ b ∨ ¬ c))", Yes)
  , ("n11", "P ⊃ ¬ P", No)
  , ("n12", "P ∧ Q ⊃ r", No)
  , ("n13", "P ∨ Q ⊃ P", No)
  , ("n14", "P1(c) ⊃ P1(c)", Yes) 
  -- @* Implication
  , ("n15", "(A ⊃ B ⊃ C) ⊃ (A ⊃ B) ⊃ (A ⊃ C)", Yes)
  , ("n47", "A ⊃ B ⊃ A", Yes)
  , ("n48", "A ⊃ A", Yes)
  -- @** Structural Rules
  , ("n63", "(A ⊃ B ⊃ C) ⊃ (B ⊃ A ⊃ C)", Yes)
  , ("n64", "(A ⊃ A ⊃ B) ⊃ (A ⊃ B)", Yes)
  , ("n65", "B ⊃ (A ⊃ B)", Yes)
  -- @* Conjunction 
  , ("n66", "A ∧ B ⇔ B ∧ A", Yes)
  , ("n67", "A ∧ (B ∧ C) ⇔ (A ∧ B) ∧ C", Yes)
  , ("n68", "A ∧ A ⇔ A", Yes)
  -- @** ND 
  , ("n69", "A ⊃ B ⊃ A ∧ B", Yes)
  , ("n70", "A ∧ B ⊃ A", Yes)
  , ("n71", "A ∧ B ⊃ B", Yes)
  -- @** Seq 
  , ("n72", "(d ⊃ A) ⊃ (d ⊃ B) ⊃ (d ⊃ A ∧ B)", Yes)
  , ("n73", "(A ⊃ C) ⊃ (A ∧ B ⊃ C)", Yes)
  , ("n74", "(B ⊃ C) ⊃ (A ∧ B ⊃ C)", Yes)
  , ("n75", "(A ⊃ B ⊃ C) ⊃ (A ∧ B ⊃ C)", Yes)
  -- @** Other 
  , ("n76", "A ⊃ A ∧ B", No)
  , ("n77", "B ⊃ A ∧ B", No)
  -- @* ConjunCtion / ImpliCation 
  , ("n78", "A ∧ B ⊃ C ⇔ A ⊃ B ⊃ C", Yes)
  , ("n79", "A ⊃ B ∧ C ⇔ (A ⊃ B) ∧ (A ⊃ C)", Yes)
  -- @* DisjunCtion 
  , ("n80", "A ∨ B ⇔ B ∨ A", Yes)
  , ("n81", "A ∨ (B ∨ C) ⇔ (A ∨ B) ∨ C", Yes)
  , ("n82", "A ∨ A ⇔ A", Yes)
  , ("n83", "A ∨ A ⊃ A", Yes)
  -- @** ND 
  , ("n84", "A ⊃ A ∨ B", Yes)
  , ("n85", "B ⊃ A ∨ B", Yes)
  , ("n86", "(A ⊃ C) ⊃ (B ⊃ C) ⊃ (A ∨ B ⊃ C)", Yes)
  -- @** Seq 
  , ("n87", "(d ⊃ A) ⊃ (d ⊃ A ∨ B)", Yes)
  , ("n88", "(d ⊃ B) ⊃ (d ⊃ A ∨ B)", Yes)
  , ("n89", "(A ⊃ C) ⊃ (B ⊃ C) ⊃ (A ∨ B ⊃ C)", Yes)
  -- @** Other 
  , ("n90", "A ∨ B ⊃ A", No)
  , ("n91", "A ∨ B ⊃ B", No)
  -- @* DisjunCtion / ImpliCation 
  , ("n92", "A ∨ B ⊃ C ⇔ (A ⊃ C) ∧ (B ⊃ C)", Yes)
  , ("n93", "(A ⊃ B ∨ C) ⊃ (A ⊃ B) ∨ (A ⊃ C)", No)
  , ("n94", "(A ⊃ B ∨ C) ⊂ (A ⊃ B) ∨ (A ⊃ C)", Yes)
  -- @* DisjunCtion / ConjunCtion 
  , ("n95", "A ∨ (B ∧ C) ⇔ (A ∨ B) ∧ (A ∨ C)", Yes)
  , ("n96", "A ∧ (B ∨ C) ⇔ (A ∧ B) ∨ (A ∧ C)", Yes)
  -- @* Truth 
  -- @** ND 
  , ("n97", "⊤", Yes)
  -- @** Seq 
  , ("n98", "d ⊃ ⊤", Yes)
  , ("n99", "(⊤ ⊃ C) ⊃ C", Yes)
  -- @** Other 
  , ("n100", "⊤ ⊃ C ⇔ C", Yes)
  , ("n101", "A ⊃ ⊤ ⇔ ⊤", Yes)
  , ("n102", "A ∧ ⊤ ⇔ A", Yes)
  , ("n103", "A ∨ ⊤ ⇔ ⊤", Yes)
  -- @* Nohood 
  -- @** ND 
  , ("n104", "⊥ ⊃ C", Yes)
  -- @** Seq 
  , ("n105", "⊥ ⊃ C", Yes)
  -- @** Other 
  , ("n106", "⊥ ⊃ C ⇔ ⊤", Yes)
  , ("n107", "⊥ ⊃ (A ⊃ ⊥)", Yes)
  , ("n108", "A ∧ ⊥ ⇔ ⊥", Yes)
  , ("n109", "A ∨ ⊥ ⇔ A", Yes)
  , ("n110", "(A ⊃ ⊥) ⊃ ⊥", No)
  , ("n111", "⊥", No)
  -- @* Implication / Implication 
  , ("n112", "((A ⊃ B) ⊃ C) ⊃ (A ∨ C) ∧ (B ⊃ C)", No)
  , ("n113", "((A ⊃ B) ⊃ C) ⊂ (A ∨ C) ∧ (B ⊃ C)", Yes)
  -- @* Classical 
  , ("n114", "((A ⊃ ⊥) ⊃ ⊥) ⊃ A", No)
  , ("n115", "((A ⊃ B) ⊃ A) ⊃ A", No)
  , ("n116", "A ∨ (A ⊃ B)", No)
  , ("n171", "((A ⊃ ⊥) ⊃ ⊥) ⊃ A", No)
  , ("n172", "((A ⊃ B) ⊃ A) ⊃ A", No)
  , ("n173", "A ∨ (A ⊃ B)", No)
    -- Misc
  , ("c10", "s ⊃ (¬ (t ⊃ r) ⊃ p) ⊃ ¬ (((p ⊃ q) ∧ (t ⊃ r)) ⊃ ( ¬ ¬ p ∧ s ∧ s ))", No)
  , ("c11", "s ⊃ (¬ (t ⊃ r) ⊃ p) ⊃ ¬ ((p ⊃ q) ∧ (t ⊃ r)) ⊃ ( ¬ ¬ p ∧ s ∧ s )", Yes)
  , ("c12", "p ∧ q ⊃ r ⇔ p ⊃ q ⊃ r", Yes)
  , ("c13", "(¬ ¬ (p ∨ ¬p) ⊃ p ∨ ¬p) ⊃ (p ∨ ¬p)", Yes)
  , ("c14", "((¬ ¬ (p ∨ ¬p)) ⊃ (p ∨ ¬p)) ⊃ (p ∨ ¬p)", Yes)
  , ("c15", "(¬ ¬ (p ∨ ¬p) ⊃ p ∨ ¬p) ⊃ (p ∨ ¬p)", Yes)
  , ("c17", "( ( a ⇔ b ) ⇔ ( b ⇔ a ) )", Yes)
  , ("c22", "(¬ ¬p ⊃ p) ⊃ (p ∨ ¬p)", No)

  , ("c24", " ( ( p1 ⇔ p2)  ⊃ p) ⊃ \
            \ ( ( p2 ⇔ p3)  ⊃ p) ⊃ \
            \ ( ( p3 ⇔ p1)  ⊃ p) ⊃ p", No)

  , ("c26", "∀ X. p(X) ⊃ p(X)", Yes)
  , ("c27", "(∀ Z. p(Z)) ⊃ p(c)", Yes)
  , ("c28", "(∀ X. p(X)) ⊃ (∀ X. p(X))", Yes)
  , ("c29", "(∃ Y. ∀ X. p2(X, Y)) ⊃ (∀ X. ∃ Y. p2(X, Y))", Yes)
  , ("c30", "(∃ X, Y. p2(X, Y)) ⇔ (∃ Y, X. p2(Y, X))", Yes)
  , ("c32", "(∀ X. ∃ Y. p2(X, Y)) ⊃ (∃ Y. ∀ X. p2(X, Y))", No)
  , ("c33", "∀ X1, X2. \
            \    ∃ Y. \
            \    ∀ Z. \
            \      ( p2(X1,Y) \
            \     ⊃ ( p2(Z,X1) \
            \       ⊃ ( p2(Z,Y) \
            \         ⊃ ( p2(X2,Y) \
            \            ∨ p2(X2,Z) ) ) ) ) ", No)
  , ("c34", "(∃ X. p1(X) ⊃ q1(X)) ⊃ (∃ X, Y. p1(X) ⊃ q1(Y))", Yes)
  , ("c35", "(∃ X, Y. p1(X) ⊃ q1(Y)) ⊃ (∃ X. p1(X) ⊃ q1(X))", No)
  , ("c36", "(∃ X, Y. p1(X) ⊃ q1(Y)) ⇔ (∃ X. p1(X) ⊃ q1(X))", No)

-- Markov's principle

  , ("markov", "(∀ X. P(X) ∨ ¬ P(X)) ∧ (¬ ∀ X. ¬ P(X)) ⊃ (∃ X. P(X))", No)

-- >   , ("c35", "(∀ (X,Y : U). ( p(X) ⊃ p(Y) )) ∨ ⊥", No)
-- >   , ("c36", " (∀ (X : U). ( a(X) ∨ b(X) )) ⊃ \
-- >              ( ∃ (Y : U). a(Y) ⊃ ∃ (Z : U). ¬ a(Z) ) ⊃ \
-- >              ( ∀ (X : U). ¬ b(X) ) ⊃ \
-- >              dummy", Yes)
-- >   , ("c37", "( ∃ (X1 : U). ∀ (X2 : U). ∃ (X3 : U). ∀ (X4 : U). ( p1(X1,X2) ∧ p2(X3,X4) ) ⊃ \
-- >              ∀ (X4 : U). ∃ (X3 : U). ∀ (X2 : U). ∃ (X1 : U). ( p1(X1,X2) ∧ p2(X3,X4) ) )", Yes)
-- >   , ("c38", "∃ (X : U). ∀ (Y : U). ∃ (Z1,Z2 : U). ( f(X,Y,Z1,Z2,Z1) ⊃ f(Z1,X,Y,Z1,Z2) )", No)
-- >   , ("c39", "( (p(c) ) ⇔ ( ∀ (Y : U). p(Y) ) ) ⊃ ( ∀ (Y : U). ( p(c) ⇔ p(Y) ) )", No)
-- >   , ("c40", "( ( ∃ (X : U). p(X) ) ⇔ ( ∀ (Y : U). p(Y) ) ) ⊃ ( ∃ (X : U). ∀ (Y : U). ( p(X) ⇔ p(Y) ) )", Yes)
-- >   , ("c41", "∀ (X : U). ∃ (Y : U). ∀ (Z : U). \
-- >               ( f(Y,X) \
-- >                  ⊃ ( ( f(X,Z) \
-- >                      ⊃ f(X,Y) ) \
-- >                     ∧ ( f(X,Y) \
-- >                      ⊃ ( ¬ f(X,Z) \
-- >                        ⊃ ( f(Y,X) \
-- >                           ∧ f(Z,Y) ) ) ) ) )", No)
-- >   , ("c42", "( ( ∃ (X : U). \
-- >                     ∀ (Y : U). \
-- >                       ( big_p(X) \
-- >                     ⇔ big_p(Y) ) \
-- >                 ⇔ ( ∃ (U : U). big_q(U) \
-- >                   ⇔ ∀ (W : U). big_q(W) ) ) \
-- >               ⇔ ( ∃ (X1 : U). \
-- >                     ∀ (Y1 : U). \
-- >                       ( big_q(X1) \
-- >                     ⇔ big_q(Y1) ) \
-- >                 ⇔ ( ∃ (U1 : U). big_p(U1) \
-- >                   ⇔ ∀ (W1 : U). big_p(W1) ) ) )", Yes)
-- >   , ("c43", "( ( ( ( a0 \
-- >                      ⊃ f ) \
-- >                     ∧ ( ( b2 \
-- >                        ⊃ b0 ) \
-- >                      ⊃ a2 ) \
-- >                     ∧ ( ( b0 \
-- >                        ⊃ a1 ) \
-- >                      ⊃ a0 ) \
-- >                     ∧ ( ( b1 \
-- >                        ⊃ a2 ) \
-- >                      ⊃ a1 ) ) \
-- >                  ⊃ f ) \
-- >                 ∧ ( ( ( ( b1 \
-- >                        ⊃ a2 ) \
-- >                      ⊃ a1 ) \
-- >                     ∧ ( ( b0 \
-- >                        ⊃ a1 ) \
-- >                      ⊃ a0 ) \
-- >                     ∧ ( ( b2 \
-- >                        ⊃ b0 ) \
-- >                      ⊃ a2 ) \
-- >                     ∧ ( a0 \
-- >                      ⊃ f ) ) \
-- >                  ⊃ f ) )", Yes)
-- >   , ("c44", "( ∀ (U,V,W : U). \
-- >                     ( big_p(U,V) \
-- >                     ∨ big_p(V,W) ) \
-- >                ⊃ ∃ (X : U). \
-- >                   ∀ (Y : U). big_p(X,Y) )", No)
-- >   , ("c45", "q(a1,a2,a3,a1,a2,a3) ⊃ \
-- >               (∃ (X1,X2,X3,Y1,Y2,Y3 : U). \
-- >                   ( ( ( p(X1) \
-- >                       ∧ p(X2) \
-- >                       ∧ p(X3) ) \
-- >                   ⊃ ( \
-- >                        p(Y2) \
-- >                       ∧ p(Y3) ) ) \
-- >              ∧ q(X1,X2,X3,Y1,Y2,Y3) ) )", Yes)
-- >   , ( "p174"
-- >     , "q(a1, a2, a3, a1, a2, a3) ⊃ \
-- >        ∃ (X1, X2, X3, Y1, Y2, Y3 : U). \
-- >           ( q(X1, X2, X3, Y1, Y2, Y3) ∧  ( ( p(X1) ∧ p(X2) ∧ p(X3)  ) ⇔ \
-- >               ( p(Y1) ∧ p(Y2) ∧ p(Y3) ) )   )"
-- >     , Yes)
-- >   , ("sm00", " ( p1 ∨ p2 ) \
-- >               ⊃  ( ¬ p1 ∨ p2 ) \
-- >               ⊃ ( p1 ∨ ¬ p2 )  \
-- >               ⊃ ¬ ( ¬ p1 ∨ ¬ p2 )", Yes)
-- >    , ("sm01", "( ( ∃ (X : U). p(X) ) ⇔ ( ∀ (Y : U). p(Y) ) ) ⊃ ( ∃ (X : U). ∀ (Y : U). ( p(X) ⇔ p(Y) ) )", Yes)

  ]

-- @ Problems 

-- >   , ("c16", " ( ( p1 ⇔ p2)  ⊃ ( p1 ∧ ( p2 ∧ p3 ) )) ⊃ \
-- >               ( ( p2 ⇔ p3)  ⊃ ( p1 ∧ ( p2 ∧ p3 ) )) ⊃ \
-- >               ( ( p3 ⇔ p1)  ⊃ ( p1 ∧ ( p2 ∧ p3 ) )) ⊃ \
-- >               ( p1 ∧ ( p2 ∧ p3 ))", Yes)

-- @ Top

tests :: Test
tests = "Uninterp" ~: map mkTest formulas
  where mkTest (name, f, res) = name ~: do
          res' <- Prover.uninterpProve (Misc.String f)
          Misc.three res' @?= res

_main :: IO ()
_main = do S.putStrLn "Uninterp"
           θs :: [Bool] <- mapM prv formulas
           mapM_ (PP.putStrLn . Print.pPrint sig Ctx.empty) θs
 where 
  prv :: (String, String, Three) -> IO Bool
  prv (_, f, b) = Prover.uninterpProve (Misc.String f) >>= return . (== b) . Misc.three


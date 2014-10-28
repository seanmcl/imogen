
structure TermIndex = ListIndex
structure RuleTermIndex = RuleListIndex

(* structure TermIndex = PathIndex *)
(* structure RuleTermIndex = RulePathIndex *)
(* structure Index = StreeIndex *)

structure Index = IndexFn (TermIndex)
structure RuleIndex = IndexFn (RuleTermIndex)

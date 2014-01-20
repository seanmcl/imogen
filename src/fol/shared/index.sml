
(* structure TermIndex = ListIndex *)
structure TermIndex = PathIndex
structure RuleTermIndex = RulePathIndex
(* structure Index = StreeIndex *)

structure Index = IndexFn (TermIndex)
structure RuleIndex = IndexFn (RuleTermIndex)

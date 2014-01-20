
structure Ordered = struct
   structure Frontend = Formula
   structure World = World
   val entails = Entails.f
   val simp = Entails.simp
   val reduce = Entails.reduce
   val unify = Entails.unify
end

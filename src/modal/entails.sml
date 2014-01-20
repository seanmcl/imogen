
structure Entails = struct
   structure K = EntailsFn (val logic = Logic.K)
   structure T = EntailsFn (val logic = Logic.T)
   structure B = EntailsFn (val logic = Logic.B)
   structure K4 = EntailsFn (val logic = Logic.K4)
   structure S4 = EntailsFn (val logic = Logic.S4)
   structure S5 = EntailsFn (val logic = Logic.S5)
end

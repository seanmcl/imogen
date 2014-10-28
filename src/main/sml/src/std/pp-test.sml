
structure PPTest = struct
   open PP.Ops
   val t1 = $"abc"
   val t2 = &[t1, t1]
   val t3 = &[t1, t1, t1]
   val t4 = &[t1, %[\\, t1]]
   val t5 = &[t1, %[\\, &[t1, %[\\, t1]]]]
   val t6 = &(List1.replicate (100, t5))
   val x = [ $"aaaaaaaaaaaaaaaaaa", $"bbbbbbbbbbbbbbbbbb", $"cccccccccccccccc", $"ddddddddddddddddddddddddddddddddd", $"eeeeeeeeeeeeeeeeeeeeeeeeee" ]
   val t7 = PP.sep x
   val t8 = PP.cat x
   val t9 = PP.hsep x
   val t10 = PP.hcat x
   val t11 = PP.vcat x
   val t12 = PP.fsep x
   val t13 = PP.fcat x
   val tests =
      [ t1 , t2 , t3 , t4 , t5 , t6 , t7, t8, t9, t10, t11, t12, t13 ]
   fun run () = List.app PP.ppl tests
end

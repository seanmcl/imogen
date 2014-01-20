
structure Test : Test = struct
   open UnitTest.Ops

   structure RSeq = RSeqTestFn(RSeq)
   val test =
      $("Cascade", &[ $("RSeq", RSeq.test)
                    ])
end

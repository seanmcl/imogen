
module Imogen.Util.Test 
  ( module Imogen.Util.Test.Base
  , module Imogen.Util.Test.Text
  , assertEqualP
  )
where

import Prelude hiding (print, putStr, putStrLn) 
import qualified Codec.Binary.UTF8.String as UString
import qualified Imogen.Util.Print as PP
import Imogen.Util.Test.Base
import Imogen.Util.Test.Text

assertEqualP :: (Eq a, Show a) => PP.Doc -> a -> a -> Assertion
assertEqualP = assertEqual . UString.encodeString . PP.render

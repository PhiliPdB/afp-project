module Unit where

import Test.Tasty
import Test.Tasty.HUnit

unitTests = testGroup "Unit tests"
  [ testCase "List comparison (different length)" $
      [1, 2, 3] `compare` [1,2] @?= GT

  -- the following test does not hold
  , testCase "List comparison (same length)" $
      [1, 2, 3] `compare` [1,2,3] @?= EQ
  ]

-- testConstruction = undefined
--     where 

-- test1 = undefined
--     where s = SpreadSheet $ 1 ("col1", CInt  $ CData [1,2,3,4,5])
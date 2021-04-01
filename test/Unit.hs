module Unit where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Map
import Data.SpreadSheet
import Data.Column
import Data.Maybe

import Data.Formula
import Data.Type

import qualified Data.Map as M

unitTests = testGroup "Unit tests"
  [ testCase "Evaluating spreadsheet without formula's results in the same spreadsheet"  
      test1
  , testCase "Evaluating spreadsheet with formula's"   
      test2
  , testCase "Evaluating spreadsheet with a cross table column"
      test3
  , testCase "validating correct (non cross formula) spreadsheet in empty env"
      testIsValidSpreadSheet
  , testCase "validating incorrect (non cross formula) spreadsheet in empty env"
      testInvalidSpreadSheet
  , testCase "validating cross formula spreadSheet in env with referred columns"
     testIsValidCrossSpreadSheet
  , testCase "validating cross formula spreadsheet in env without referred columns"
     testInvalidCrossSpreadSheet
  , testCase "adding incorrect spreadsheet fails"
     testAddingincorrectSpreadSheet
  , testCase "adding spreadsheet to correct env succeeds"
     testAddingCorrectSpreadSheet
  ]


test1 = evalSpreadSheet s env @?= [("col1", DInt [1..5])]
  where (Just s@(SpreadSheet _ cs)) = spreadSheet [("col1", CInt  $ CData [1..5])]
        env = SpreadSheetEnv $ Data.Map.fromList [("S1", s)]

testSpreadSheet :: SpreadSheet
testSpreadSheet = SpreadSheet 5
    [ ("col1", CInt  $ CData [1,2,3,4,5])
    , ("col2", CInt  $ CData [51,52,53,54,55])
    , ("col3", CInt  $ CForm $ Prod (Var "col1" inferType) (Var "col2" inferType))
    , ("col4", CBool $ CData [True, False, True, False, True])
    ]

test2 = evalSpreadSheet s env @?= expected
  where s@(SpreadSheet _ cs) = testSpreadSheet
        env = SpreadSheetEnv $ Data.Map.fromList [("S1", testSpreadSheet)]
        expected = [("col1", DInt [1..5]), ("col2", DInt [51..55]), ("col3", DInt [51, 104, 159, 216, 275]), ("col4", DBool [True, False, True, False, True])]


testCrossSpreadSheet :: SpreadSheet
testCrossSpreadSheet = SpreadSheet 5
    [ ("col1", CInt $ CForm $ CTVar "main" "col2" inferType)
    ]

testEnv :: SpreadSheetEnv
testEnv = SpreadSheetEnv $ M.fromList [("main", testSpreadSheet), ("secondary", testCrossSpreadSheet)]

test3 = evalSpreadSheet testCrossSpreadSheet testEnv
            @?= [("col1", DInt [51..55])]


testIsValidSpreadSheet = isValidSpreadSheet ("main", testSpreadSheet) (SpreadSheetEnv M.empty) @?= True

testInvalidSpreadSheet = not (isValidSpreadSheet ("main", invalidSpreadSheet) (SpreadSheetEnv M.empty)) @?= True
    where invalidSpreadSheet = 
            SpreadSheet 5
                [ ("col1", CInt  $ CData [1,2,3,4,5])
                , ("col2", CInt  $ CData [51,52,53,54,55])
                , ("col3", CInt  $ CForm $ Prod (Var "col5" inferType) (Var "col2" inferType))
                , ("col4", CBool $ CData [True, False, True, False, True])
                ]

testIsValidCrossSpreadSheet = isValidSpreadSheet ("secondary", testCrossSpreadSheet) env @?= True
    where env = SpreadSheetEnv (M.fromList [("main", testSpreadSheet)])

testInvalidCrossSpreadSheet = isValidSpreadSheet ("secondary", testCrossSpreadSheet) env @?= False
    where env = SpreadSheetEnv (M.fromList [("main", testSpreadSheet)])
          testSpreadSheet 
            = SpreadSheet 5
                [ ("col1", CInt  $ CData [1,2,3,4,5])
                , ("col3", CInt  $ CForm $ Prod (Var "col1" inferType) (Var "col1" inferType))
                , ("col4", CBool $ CData [True, False, True, False, True])
                ]

testAddingincorrectSpreadSheet = isNothing (addSpreadSheet ("secondary", testCrossSpreadSheet) (SpreadSheetEnv M.empty)) @?= True

testAddingCorrectSpreadSheet = isJust (addSpreadSheet ("secondary", testCrossSpreadSheet) env)  @?= True
        where env = SpreadSheetEnv (M.fromList [("main", testSpreadSheet)])


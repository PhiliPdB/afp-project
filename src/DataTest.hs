{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
module DataTest where

import Data.Formula
import Data.Type
import Data.SpreadSheet
import Data.Column
import qualified Data.Map as M
import Data.Hourglass

testFormula :: Formula Int
testFormula = Prod (Var "col1" inferType) (Var "col2" inferType)

testIf :: Formula Int
testIf = IfThenElse (Var "col4" inferType) (Var "col1" (inferType :: Type Int)) (Var "col2" inferType)

plus1 :: Formula Int
plus1 = Add (Var "col1" inferType) (Lit 1)


testSpreadSheet :: SpreadSheet
testSpreadSheet = SpreadSheet 5
    [ ("col1", CInt  $ CData [1,2,3,4,5])
    , ("col2", CInt  $ CData [51,52,53,54,55])
    , ("col3", CInt  $ CForm testFormula)
    , ("col4", CBool $ CData [True, False, True, False, True])
    , ("col5", CTime     $ CData timeList)
    , ("col6", CWeekDay  $ CData [Monday, Tuesday, Thursday, Friday, Sunday]) 
    , ("col7", CMonth    $ CData [March, April, May, June, July])
    , ("col8", CDate     $ CData dateList)
    ]
    where
        timeList = replicate 5 $ TimeOfDay (Hours 11) (Minutes 45) (Seconds 30) (NanoSeconds 0)
        dateList = replicate 5 $ Date 19 March 2021

testCrossSpreadSheet :: SpreadSheet
testCrossSpreadSheet = SpreadSheet 4
    [ ("col1", CInt $ CForm $ CTVar "main" "col2" inferType)
    ]

testEnv :: SpreadSheetEnv
testEnv = M.fromList [("main", testSpreadSheet), ("secondary", testCrossSpreadSheet)]

{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
module DataTest where

import Data.Formula
import Data.Type
import Data.SpreadSheet
import Data.Column
import qualified Data.Map as M
import Data.Hourglass
import Data.Maybe (fromJust)
import ReadCSV
import Export


testFormula :: Formula Int
testFormula = Prod (Var "col1" inferType) (Var "col2" inferType)

testIf :: Formula Int
testIf = IfThenElse (Var "col4" inferType) (Var "col1" (inferType :: Type Int)) (Var "col2" inferType)

plus1 :: Formula Int
plus1 = Add (Var "col1" inferType) (Lit 1)


testSpreadSheet :: SpreadSheet
testSpreadSheet = SpreadSheet 5
    [ ("col1" , CInt  $ CData [1,2,3,4,5])
    , ("col2" , CInt  $ CData [51,52,53,54,55])
    , ("col3" , CInt  $ CForm testFormula)
    , ("col4" , CBool $ CData [True, False, True, False, True])
    , ("col5" , CTime     $ CData timeList)
    , ("col6" , CWeekDay  $ CData [Monday, Tuesday, Thursday, Friday, Sunday])
    , ("col5" , CTime     $ CData timeList) -- fails to export on Windows
    , ("col6" , CWeekDay  $ CData [Monday, Tuesday, Thursday, Friday, Sunday])
    , ("col7" , CMonth    $ CData [March, April, May, June, July])
    , ("col8" , CDate     $ CData dateList) -- fails to export on Windows
    , ("col9" , CDateTime     $ CData dateTimeList) -- fails to export on Windows
    , ("col10", CDuration     $ CData durationList)
    , ("col11", CPeriod       $ CData periodList)
    ]
    where
        timeList     = replicate 5 $ TimeOfDay (Hours 11) (Minutes 45) (Seconds 30) (NanoSeconds 0)
        dateList     = replicate 5 $ Date 19 March 2021
        dateTimeList = replicate 5 $ DateTime (Date 19 March 2021) (TimeOfDay (Hours 11) (Minutes 45) (Seconds 30) (NanoSeconds 0))
        durationList = replicate 5 $ Duration (Hours 11) (Minutes 45) (Seconds 30) (NanoSeconds 0)
        periodList   = replicate 5 $ Period 110 6 27

---------------------------------------------------------------
{-
The "fails to export on Windows" is related to timeParse using timeGetDateTimeOfDay, which uses
dateTimeFromUnixEpochP, which then uses some functions that are most likely unix based.
Futher investigations are needed.
-}

testCrossSpreadSheet :: SpreadSheet
testCrossSpreadSheet = SpreadSheet 4
    [ ("col1", CInt $ CForm $ CTVar "main" "col2" inferType)
    ]

testEnv :: SpreadSheetEnv
testEnv = M.fromList [("main", testSpreadSheet), ("secondary", testCrossSpreadSheet)]


testAggr :: SpreadSheetEnv
testAggr = M.fromList
    [ ("form", SpreadSheet 5
        [ ("item", CInt $ CData [1,2,3,4,5])
        , ("form", CInt $ CForm $ Aggr "data" "count" (inferType :: Type Int) "item" inferType "price" inferType Eq Sum)
        ]
      )
    , ("data", SpreadSheet 10
        [ ("count", CInt $ CData [ 1, 3,  4,   1,  2, 3, 4,  2, 2, 1])
        , ("price", CInt $ CData [10, 5, 12, 234, 43, 1, 5, 90, 2, 5])
        ]
      )
    ]

testForm :: SpreadSheet
testForm = fromJust $ M.lookup "form" testAggr

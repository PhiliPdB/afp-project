{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
module DataTest where

import Data.Formula
import Data.Type
import Data.SpreadSheet
import Data.Column

import Data.Map.Ordered as OM

testFormula :: Formula Int
testFormula = Prod (Var "col1" inferType) (Var "col2" inferType)

testIf :: Formula Int
testIf = IfThenElse (Var "col4" inferType) (Var "col1" (inferType :: Type Int)) (Var "col2" inferType)

plus1 :: Formula Int
plus1 = Add (Var "col1" inferType) (Lit 1)


testSpreadSheet :: SpreadSheet
testSpreadSheet = SpreadSheet 4 (OM.fromList
    [ ("col1", CInt  $ CData [1,2,3,4])
    , ("col2", CInt  $ CData [51,52,53,54])
    , ("col3", CInt  $ CForm testFormula)
    , ("col4", CBool $ CData [True, False, True, False])
    ])

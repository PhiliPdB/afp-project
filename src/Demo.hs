module Demo where

import ReadCSV
import Data.SpreadSheet
import Data.Column
import Export
import Data.Formula
import Data.Type

import Data.Map as M

import Data.Hourglass as HG
import Prelude hiding (LT)

-- Demonstrating types, basic spreadsheet operations & import + exportation

d1 :: IO ()
d1 = do
    s <- importCSV "demo/products.csv" ',' True
    let (Right s1) = s
    let s2 = tryAddColumn s1 1 ("Origin", CString (CData ["Africa", "India", "Brazil"]))
    let s3 = tryAddRow s2 [FDate (HG.Date 2021 HG.April 8), FString "Netherlands", FString "Strawberries", FInt 5, FFloat 1.2, FInt 4]
    let env = spreadSheetEnv $ M.fromList [("s", s3)]
    writeCSVFile "demo/export1.csv" env s3
    return ()


-- Demonstration of Formula's

calculateRevenue :: Formula Double
calculateRevenue = Prod (ToFloat (Var "Quantity" inferType)) (Var "Price" inferType)

needsInvestigation :: Formula Bool
needsInvestigation = IfThenElse (LT (Var "Rating" (inferType :: Type Int)) (Lit (3 :: Int))) (Lit True) (Lit False)

d2 :: IO ()
d2 = do
    s <- importCSV "demo/export1.csv" ',' True
    let (Right s1) = s
    let s2 = tryAddColumn s1 4 ("Revenue", CFloat (CForm calculateRevenue))
    let s3 = tryAddColumn s2 7 ("NeedsInvestigation", CBool (CForm needsInvestigation))
    let env = spreadSheetEnv $ M.fromList [("s", s3)]
    writeCSVFile "demo/export2.csv" env s3
    return ()

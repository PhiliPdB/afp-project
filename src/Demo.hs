module Demo where

import ReadCSV
import Data.SpreadSheet
import Data.Column
import Export

import Data.Map as M

import Data.Hourglass as HG

addColumn :: IO ()
addColumn = do
    s <- importCSV "demo/products.csv" ',' True
    let (Right s1) = s
    let s2 = tryAddColumn s1 1 ("Origin", CString (CData ["Africa", "India", "Brazil"]))
    let s3 = tryAddRow s2 [FDate (HG.Date 2021 HG.April 8), FString "Netherlands", FString "Strawberries", FInt 5, FFloat 1.2]
    let env = spreadSheetEnv $ M.fromList [("s", s3)]
    writeCSVFile "demo/export1.csv" env s3
    return ()

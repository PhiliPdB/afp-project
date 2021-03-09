module ReadCSV(
 importCSV,
 inferDataType
) where

import Data.SpreadSheet
import Data.Column
import Data.List.Split (split, oneOf, dropDelims)
import Data.List       (transpose)
import Text.Read       (readMaybe)
import Data.Maybe      (isJust, fromJust)


-- | Takes a rawColumn in string form, checks what type they should be and
--   replies with a SpreadSheetCols containing the correct type and the list
--   with correct type.
inferDataType :: [String] -> SpreadSheetCols
inferDataType tColumn | isJust intCol  = CInt    $ CData $ fromJust intCol
                      | isJust boolCol = CBool   $ CData $ fromJust boolCol
                      | otherwise      = CString $ CData            tColumn
        where
            intCol  = sequenceA (map readMaybe tColumn :: [Maybe Int])
            boolCol = sequenceA (map readMaybe tColumn :: [Maybe Bool])


-- | Creates the spreadsheet based on the size of the rawData with default column 
--   names and data in them based on the function inferDataType.
--   Additionally it filters out the empty columns in the csv.
createSpSh :: [[String]] -> Bool -> SpreadSheet
createSpSh rows True  = SpreadSheet sheetSize finalData
        where 
            sheetSize = length $ tail rows
            names     = head rows
            transRows = filter (not . all null) $ transpose $ tail rows
            finalData = zip names $ map inferDataType transRows
createSpSh rows False = SpreadSheet sheetSize finalData
        where 
            sheetSize = length rows
            transRows = filter (not . all null) $ transpose rows
            names     = ["col" ++ show x | x <- [1..(length transRows)]]
            finalData = zip names $ map inferDataType transRows


-- | Takes the path to the file, the separator that is should look for, boolean information
--   if it should take the first row as column names and replies with the spreadsheet wraped
--   in the IO monad.
importCSV :: FilePath -> Char -> Bool -> IO SpreadSheet
importCSV fPath sep takeNames= do
                        contents <- readFile fPath
                        let rawData = split (dropDelims $ oneOf [sep]) <$> lines contents
                        return $ createSpSh rawData takeNames


-- TODO: There is one glitch connected to having the , sign as value in a column.
--       In this case it is writen as "," in the csv, but importCSV will read it
--       as two " signs separetad with , . Maybe playing around with oneOf can fix the problem.

-- temporary testing tools
getColVal []                         = []
getColVal ((_,CInt    (CData x)):xs) = show x :getColVal xs
getColVal ((_,CBool   (CData x)):xs) = show x :getColVal xs
getColVal ((_,CString (CData x)):xs) = show x :getColVal xs

getColNames []         = []
getColNames ((x,_):xs) = x:getColNames xs

getInnerList (SpreadSheet _ l) = l

getSpShSize (SpreadSheet n _) = n 
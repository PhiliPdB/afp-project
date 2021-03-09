module ReadCSV where

import Data.SpreadSheet
import Data.Column
import Data.List.Split (split, oneOf)
import Data.List       (transpose)
import Text.Read       (readMaybe)
import Data.Maybe      (isJust)

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
createSpSh :: [[String]] -> SpreadSheet -- TODO: add posibility to take column names from csv
createSpSh rows = SpreadSheet sheetSize finalData
        where
            sheetSize = length rows
            names     = ["col" ++ show x | x <- [1..sheetSize]]
            transRows = transpose rows
            finalData = [(colName, colData) | colName <- names, colData <- map inferDataType transRows]


-- | Takes the path to the file, the separator that is should look for, and replies with the
--   spreadsheet wrapen in the IO monad. 
importCSV :: FilePath -> Char -> IO SpreadSheet --TODO: Add a boolean to inform if the column names should be taken
                                                --from the csv or to be default col<n>
importCSV fPath sep = do
                        contents <- readFile fPath
                        let rawData = split (oneOf [sep]) <$> lines contents
                        return $ createSpSh rawData


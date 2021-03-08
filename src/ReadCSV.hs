module ReadCSV where

import Data.SpreadSheet
import Data.Column
import Data.List.Split  -- for split and oneOf
import Data.List        -- for transpose
import Text.Read        -- for readMaybe
import Data.Maybe       -- for fromJust

-- | Takes a rawColumn in string form, checks what type they should be and
--   replies with a SpreadSheetCols containing the correct type and the list
--   with correct type.
inferDataType :: [String] -> SpreadSheetCols
inferDataType tColumn | intCol    /= Nothing = CInt    $ CData $ fromJust intCol 
                      | boolCol   /= Nothing = CBool   $ CData $ fromJust boolCol
                      | otherwise            = CString $ CData            tColumn
        where
            intCol  = sequenceA (map readMaybe tColumn :: [Maybe Int])
            boolCol = sequenceA (map readMaybe tColumn :: [Maybe Bool])

-- | Creates the spreadsheet based on the size of the rawData with default column 
--   names and data in them based on the function inferDataType.
createSpSh :: [[String]] -> SpreadSheet
createSpSh rows = SpreadSheet sheetSize finalData
        where
            sheetSize = length rows
            names     = ["col" ++ show x | x <- [1..sheetSize]]
            transRows = transpose rows
            finalData = [(colName, colData) | colName <- names, colData <- map inferDataType transRows]


-- | Takes the path to the file, the separator that is should look for, and replies with the
--   spreadsheet wrapen in the IO monad. 
importCSV :: FilePath -> Char -> IO SpreadSheet
importCSV fPath sep = do
                        contents <- readFile fPath
                        let rawData = split (oneOf [sep]) <$> lines contents
                        return $ createSpSh rawData


module ReadCSV(
 importCSV,
 inferDataType
) where

import Data.SpreadSheet
import Data.Column
import Data.Type
import Data.List.Split (split, oneOf, dropDelims)
import Data.List       (transpose)
import Text.Read       (readMaybe)
import Data.Maybe      (isJust, fromJust)

-- | Checks if all lists are of the same length
correctLn :: [[a]] -> Bool
correctLn []     = True
correctLn (x:xs) = all (\y -> length y == length x) xs

-- | Cheks if the raw data from the csv contains values.
--   Takes into account if the first row is are the column names
--   via the second argument. 
emptyCSV :: [[a]] -> Bool -> Bool
emptyCSV []  _    = True
emptyCSV [x] True = True
emptyCSV _   _    = False

-- | Takes the raw data, the column naming information and replies
--   with the constructed spreadsheet if the data is correct or with
--   an error message if some needed property does not hold. 
safetyFilter :: [[String]] -> Bool -> Either ErrMsg SpreadSheet
safetyFilter rawData takeNames
  | emptyCSV  rawData takeNames = Left $ ErrMsg "csv file does not contain any data"
  | correctLn rawData           = createSpSh rawData takeNames
  | otherwise                   = Left $ ErrMsg "csv file has inconsistent row lengths"

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
createSpSh :: [[String]] -> Bool -> Either ErrMsg SpreadSheet
createSpSh (n:rs) True 
    | length transRows == length names = Right $ SpreadSheet sheetSize finalData
    | otherwise                        = Left $ ErrMsg "Amount of column names does not match amount of non empty columns"
        where
            sheetSize = length rs - 1
            transRows = filter (not . all null) $ transpose rs
            names     = filter (not . null) n
            finalData = zip names $ map inferDataType transRows
createSpSh rows False = Right $ SpreadSheet sheetSize finalData
        where
            sheetSize = length rows - 1
            transRows = filter (not . all null) $ transpose rows
            n         = ["col" ++ show x | x <- [1..(length transRows)]]
            finalData = zip n $ map inferDataType transRows


-- | Takes the path to the file, the separator that is should look for, boolean information
--   if it should take the first row as column names and replies with the spreadsheet wraped
--   in the IO monad.
importCSV :: FilePath -> Char -> Bool -> IO (Either ErrMsg SpreadSheet)
importCSV fPath sep takeNames= do
                        contents <- readFile fPath
                        let rawData = split (dropDelims $ oneOf [sep]) <$> lines contents
                        return $ safetyFilter rawData takeNames


-- TODO: There is one glitch connected to having the , sign as value in a column.
--       In this case it is writen as "," in the csv, but importCSV will read it
--       as two " signs separetad with , . Maybe playing around with oneOf can fix the problem.

-- temporary testing tools
getColVal []                         = []
getColVal ((_,CInt    (CData x)):xs) = show x ++ getColVal xs
getColVal ((_,CBool   (CData x)):xs) = show x ++ getColVal xs
getColVal ((_,CString (CData x)):xs) = show x ++ getColVal xs

getColNames []         = []
getColNames ((x,_):xs) = x:getColNames xs

getInnerList (SpreadSheet _ l) = l

getSpShSize (SpreadSheet n _) = n

checkImportOutcome (Left (ErrMsg msg)) = [msg]
checkImportOutcome (Right spsh)        = getColNames (getInnerList spsh) ++ [getColVal (getInnerList spsh)]
                                          
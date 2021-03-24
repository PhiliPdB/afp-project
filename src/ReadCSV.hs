module ReadCSV(
 importCSV,
 inferDataType
) where

import Data.SpreadSheet
import Data.Column
import Data.Type
import Data.List.Split (split, oneOf, dropDelims)
import Data.List       (transpose,nub)
import Text.Read       (readMaybe)
import Data.Maybe      (isJust, fromJust)
import Data.Hourglass
import Data.TimeHelper

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
inferDataType :: [String] -> SpreadSheetCol
inferDataType tColumn | isJust intCol        = CInt      $ CData $ fromJust intCol
                      | isJust boolCol       = CBool     $ CData $ fromJust boolCol
                      | isJust monthCol      = CMonth    $ CData $ fromJust monthCol
                      | isJust dayCol        = CWeekDay  $ CData $ fromJust dayCol
                      | isJust timeLnCol     = CTime     $ CData $ map dtTime $ fromJust timeLnCol
                      | isJust timeShCol     = CTime     $ CData $ map dtTime $ fromJust timeShCol
                      | isJust dateTimeShCol = CDateTime $ CData $ fromJust dateTimeShCol
                      | isJust dateTimeLnCol = CDateTime $ CData $ fromJust dateTimeShCol
                      | isJust dateTimeSLCol = CDateTime $ CData $ fromJust dateTimeShCol
                      | isJust dateTimeSSCol = CDateTime $ CData $ fromJust dateTimeShCol
                      | isJust dateNbCol     = CDate     $ CData $ map dtDate $ fromJust dateNbCol
                      | isJust dateStCol     = CDate     $ CData $ map dtDate $ fromJust dateStCol
                      | isJust dateNbRCol    = CDate     $ CData $ map dtDate $ fromJust dateNbRCol
                      | isJust dateStRCol    = CDate     $ CData $ map dtDate $ fromJust dateStRCol
                      | isJust periodCol     = CPeriod   $ CData $ fromJust periodCol
                      | isJust durationCol   = CDuration $ CData $ fromJust durationCol
                      | otherwise            = CString   $ CData            tColumn
        where
            -- some parse attempts must come before other because of how timeParse works
            intCol        = sequenceA (map readMaybe tColumn :: [Maybe Int])
            boolCol       = sequenceA (map readMaybe tColumn :: [Maybe Bool])
            monthCol      = sequenceA (map readMaybe tColumn :: [Maybe Month])
            dayCol        = sequenceA (map readMaybe tColumn :: [Maybe WeekDay])
            -- time in long    HH:MM:SS
            timeLnCol     = traverse (timeParse "H:MI:S")             tColumn
            -- time in short   HH:MM
            timeShCol     = traverse (timeParse "H:MI")               tColumn
            -- date-time in long num DD-MM-YYYY HH:MM:SS
            dateTimeShCol = traverse (timeParse "DD-MM-YYYY H:MI:S")  tColumn
            -- date-time in short num DD-MM-YYYY HH:MM
            dateTimeLnCol = traverse (timeParse "DD-MM-YYYY H:MI")    tColumn
            -- date-time with month long DD-Mon-YYYY HH:MM:SS
            dateTimeSLCol = traverse (timeParse "DD-Mon-YYYY H:MI:S") tColumn            
            -- date-time with month short DD-Mon-YYYY HH:MM
            dateTimeSSCol = traverse (timeParse "DD-Mon-YYYY H:MI")   tColumn
            -- date in num     DD-MM-YYYY
            dateNbCol     = traverse (timeParse "DD-MM-YYYY")         tColumn
            -- date with month DD-Mon-YYYY
            dateStCol     = traverse (timeParse "DD-Mon-YYYY")        tColumn
            -- date in num     YYYY-MM-DD
            dateNbRCol    = traverse (timeParse "YYYY-MM-DD")         tColumn
            -- date with month YYYY-Mon-DD
            dateStRCol    = traverse (timeParse "YYYY-Mon-DD")        tColumn
            -- special period parse as "P Y:M:D"
            periodCol     = traverse readPeriod                       tColumn
            -- special duration parse as "D H:M:S:NS"
            durationCol   = traverse readDuration                     tColumn


colDataRelation :: [[String]] -> [String] -> Bool
colDataRelation dataCol names = all (\(a,b) -> a == b) boolPairs
    where
        emptyCol   = map (all null) dataCol
        emptyNames = map null names
        boolPairs  = zip emptyCol emptyNames


-- | Creates the spreadsheet based on the size of the rawData with default column
--   names and data in them based on the function inferDataType.
--   Additionally it filters out the empty columns in the csv.
createSpSh :: [[String]] -> Bool -> Either ErrMsg SpreadSheet
createSpSh (n:rs) True
    | everythingOk     = Right $ SpreadSheet sheetSize finalData
    | not matchUp      = Left $ ErrMsg "Column names and non empty columns do not match up"
    | not noDuplicates = Left $ ErrMsg "Column names are not unique"
        where
            everythingOk = matchUp && noDuplicates
            matchUp      = colDataRelation transRows n
            noDuplicates = length names == length (nub names)
            sheetSize    = length rs
            transRows    = transpose rs
            filtRows     = filter (not . all null) transRows
            names        = filter (not . null) n
            finalData    = zip names $ map inferDataType filtRows
createSpSh rows False = Right $ SpreadSheet sheetSize finalData
        where
            sheetSize = length rows
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


-- There is one glitch connected to having the , sign as value in a column.
-- In this case it is writen as "," in the csv, but importCSV will read it
-- as two " signs separetad with.

-- temporary testing tools
getColVal []                         = []
getColVal ((_,CInt     (CData x)):xs) = show x ++ getColVal xs
getColVal ((_,CBool    (CData x)):xs) = show x ++ getColVal xs
getColVal ((_,CMonth   (CData x)):xs) = show x ++ getColVal xs
getColVal ((_,CWeekDay (CData x)):xs) = show x ++ getColVal xs
getColVal ((_,CTime    (CData x)):xs) = show x ++ getColVal xs
getColVal ((_,CDate    (CData x)):xs) = show x ++ getColVal xs
getColVal ((_,CString  (CData x)):xs) = show x ++ getColVal xs

getColNames []         = []
getColNames ((x,_):xs) = x:getColNames xs

getInnerList (SpreadSheet _ l) = l

getSpShSize (Right (SpreadSheet n _)) = n
getSpShSize (Left _)                  = -1

checkImportOutcome (Left (ErrMsg msg)) = [msg]
checkImportOutcome (Right spsh)        = getColNames (getInnerList spsh) ++ [getColVal (getInnerList spsh)]

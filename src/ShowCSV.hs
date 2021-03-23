module ShowCSV
    ( writeCSVFile, toCSVString
    )
where

import Data.SpreadSheet (SpreadSheet(..), SpreadSheetEnv, evalSpreadSheet)
import Data.Column (SpreadSheetCol(..), Column(CData))
import Data.List (intercalate, transpose)
import Data.Hourglass

-- | Write a `SpreadSheet` to a CSV file
writeCSVFile :: FilePath -> SpreadSheetEnv -> SpreadSheet -> IO ()
writeCSVFile file env s = writeFile file (toCSVString env s)

-- | Convert a spreadsheet into an CSV string
toCSVString :: SpreadSheetEnv -> SpreadSheet -> String
toCSVString env = unlines               -- Intercalate each row with a newline character
                . map (intercalate ",") -- Intercalate the data inside each row of the table with commas
                . toStringTable env     -- Convert the spreadsheet to a string table

-- | Convert a spreadsheet into a simple table containing only strings
toStringTable :: SpreadSheetEnv -> SpreadSheet -> [[String]]
toStringTable env = transpose . map (\(h, c) -> h : showCol c) . flip evalSpreadSheet env
    where -- Function to print out a column that only contains data
          showCol :: SpreadSheetCol -> [String]
          showCol (CInt      (CData xs)) = map show xs
          showCol (CBool     (CData xs)) = map show xs
          showCol (CString   (CData xs)) = map show xs
          showCol (CTime     (CData xs)) = map (timePrint "H:MI:S" . DateTime defaultDate) xs
          showCol (CWeekDay  (CData xs)) = map show xs
          showCol (CMonth    (CData xs)) = map show xs
          showCol (CDate     (CData xs)) = map (timePrint "DD-MM-YYYY") xs
          showCol (CDateTime (CData xs)) = map (timePrint "DD-MM-YYYY H:MI:S") xs
          showCol (CDuration (CData xs)) = undefined
          showCol (CPeriod   (CData xs)) = undefined
          showCol _                      = error "Non-evaluated spreadsheet column"
          defaultDate = Date 1 January 1970
          -- TODO: Add a way to print duration and period to csv      
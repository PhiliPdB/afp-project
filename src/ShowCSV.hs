module ShowCSV
    ( writeCSVFile, toCSVString
    )
where

import Data.SpreadSheet (SpreadSheet(..), SpreadSheetEnv, evalSpreadSheet)
import Data.Column (SpreadSheetCol(..), Column(CData))
import Data.List (intercalate, transpose)


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
          showCol (CInt    (CData xs)) = map show xs
          showCol (CBool   (CData xs)) = map show xs
          showCol (CString (CData xs)) = map show xs
          showCol _                    = error "Non-evaluated spreadsheet column"
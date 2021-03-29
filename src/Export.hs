module Export
    ( writeCSVFile, toCSVString, toLatex
    )
where

import Data.SpreadSheet (SpreadSheet(..), SpreadSheetEnv, evalSpreadSheet, SpreadSheetColumnData(..))
import Data.Column (SpreadSheetCol(..), Column(CData))
import Data.List (intercalate, transpose)
import Data.Hourglass
import Data.TimeHelper

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
        showCol :: SpreadSheetColumnData -> [String]
        showCol (DInt      xs) = map show xs
        showCol (DBool     xs) = map show xs
        showCol (DString   xs) = map show xs
        showCol (DTime     xs) = map (timePrint "H:MI:S" . DateTime defaultDate) xs
        showCol (DWeekDay  xs) = map show xs
        showCol (DMonth    xs) = map show xs
        showCol (DDate     xs) = map (timePrint "DD-MM-YYYY")        xs
        showCol (DDateTime xs) = map (timePrint "DD-MM-YYYY H:MI:S") xs
        showCol (DDuration xs) = map showDuration xs
        showCol (DPeriod   xs) = map showPeriod   xs
        showCol _                      = error "Non-evaluated spreadsheet column"
        defaultDate = Date 1 January 1970
          -- TODO: Add a way to print duration and period to csv

-- | Given the spreadsheet env, convert a spreadsheet into a latex table string
toLatex :: SpreadSheetEnv -> SpreadSheet -> String
toLatex env s = latexTable $ toStringTable env s

endline :: String
endline = "\n"

latexTable :: [[String]] -> String
latexTable []     = error "Cannot convert empty spreadsheet"
latexTable (h:bs) = "\\begin{table}" ++ endline ++ "\\begin{tabular}" ++ colAllignment ++ endline
                    ++ header ++ body
                    ++ "\\end{tabular}" ++ endline ++ "\\end{table}" ++ endline
    where header :: String
          header = intercalate "  &  " (map (\s -> "\\textbf{" ++ s ++ "}") h) ++ "\\\\" ++ endline ++ "\\hline" ++ endline
          body :: String
          body = intercalate ("\\\\" ++ endline) (map (intercalate "  &  ") bs) ++ endline
          colAllignment = "{" ++ intercalate "|" (map (const "c") [0..numCols]) ++ "}"
          numCols = length h

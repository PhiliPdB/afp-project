module ShowCSV
    ( writeCSVFile, toCSVString, latexTable
    )
where

import Data.SpreadSheet (SpreadSheet(..), evalSpreadSheet)
import Data.Column (SpreadSheetCol(..), Column(CData))
import Data.List (intercalate, transpose)


-- | Write a `SpreadSheet` to a CSV file
writeCSVFile :: FilePath -> SpreadSheet -> IO ()
writeCSVFile file s = writeFile file (toCSVString s)

-- | Convert a spreadsheet into an CSV string
toCSVString :: SpreadSheet -> String
toCSVString = unlines               -- Intercalate each row with a newline character
            . map (intercalate ",") -- Intercalate the data inside each row of the table with commas
            . toStringTable         -- Convert the spreadsheet to a string table

-- | Convert a spreadsheet into a simple table containing only strings
toStringTable :: SpreadSheet -> [[String]]
toStringTable = transpose . map (\(h, c) -> h : showCol c) . evalSpreadSheet
    where -- Function to print out a column that only contains data
          showCol :: SpreadSheetCol -> [String]
          showCol (CInt    (CData xs)) = map show xs
          showCol (CBool   (CData xs)) = map show xs
          showCol (CString (CData xs)) = map show xs
          showCol _                    = error "Non-evaluated spreadsheet column"

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


module ShowCSV where

import Data.SpreadSheet (SpreadSheet(..), evalSpreadSheet)
import Data.Column (SpreadSheetCol(..), Column(CData))
import Data.List (intercalate)


-- | Write a `SpreadSheet` to a CSV file
writeCSVFile :: FilePath -> SpreadSheet -> IO ()
writeCSVFile file s = writeFile file (toCSVString s)

-- | Convert a spreadsheet into an CSV string
toCSVString :: SpreadSheet -> String
toCSVString s@(SpreadSheet n _) = unlines $ intercalate "," headers : toCSVString' 0 columns
    where -- | Evaluated spreadsheet
          -- Thus contains no CFormula columns anymore.
          evaluatedSpreadSheet = evalSpreadSheet s
          -- | Headers of the evaluated spreadsheet
          headers = map fst evaluatedSpreadSheet
          -- | Columns (with only the data) of the evaluated spreadsheet
          columns = map snd evaluatedSpreadSheet

          -- | Convert the columns into an array of CSV-row-lines
          --   This is done by keeping track of the current row (with a variable `i`)
          --   and appending rows till `i` reaches the size of the spreadsheet.
          --
          -- TODO: With the current implementation for columns, this takes \(O(n^2)\) time,
          --       while \(O(n)\) should be possible if indexing is \(O(1)\).
          toCSVString' :: Int -> [SpreadSheetCol] -> [String]
          toCSVString' i cs | i < n     = intercalate "," (map (showColItem i) cs) : toCSVString' (i + 1) cs
                            | otherwise = []

          -- | Convert the `i`th item in a column to a string.
          -- NOTE: This is done in a very unsafe way where we assume that
          --       the column has been evaluated, thus doesn't contain
          --       any formula's anymore.
          showColItem :: Int            -- ^ Index of the row to 'print'
                      -> SpreadSheetCol -- ^ Column to 'print'
                      -> String         -- ^ Resulting string value
          showColItem i (CInt    (CData a)) = show (a !! i)
          showColItem i (CBool   (CData a)) = show (a !! i)
          showColItem i (CString (CData a)) = show (a !! i)
          showColItem _ _                   = error "Non-Evaluated spreadsheet column"



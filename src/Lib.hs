module Lib
  ( someFunc
  ) where

import           Data.Vector

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type Number = Float -- will need to print rounded up nicely

data Data = VNumber Vector Number | VString Vector String | VBool Vector Bool


-- infer type of column with formula?
data Formula = F


data Column = ColumnD Data | ColumnF Formula

newtype Spreadsheet = Spreadsheet [Column]


module ReadCSV where

import Data.SpreadSheet
import Data.Column
import Data.List.Split



-- | Takes the path to the file, the separator that is should look for, and replies with the
--   spreadsheet wrapen in the IO monad. 
importCSV :: FilePath -> Char -> IO String -- SpreadSheet
importCSV fPath sep = do
                        contents <- readFile fPath
                        let rawData   = split (oneOf [sep]) <$> lines contents

                            -- the data needs to be type derived and put as a Spreadsheet
                            -- data type in one go, as the main problem is to match
                            -- our type system (which can be tricky to use with lists)
                            -- with deriving and loading the types into the spreadsheet columns

                            -- Figuring out the type can be done with readMaybe

                        -- VVV just that it works for now and spoits out the IO String
                        return $ head headRow


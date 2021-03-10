{-# LANGUAGE GADTs #-}
module Data.Column where

import Data.Formula (Formula)
import Data.Type (Type(..))

-- | Column datatype containing either data or a formula
data Column a where
    -- | Column containing data
    CData :: [a]       -> Column a
    -- | Column containing a formula
    CForm :: Formula a -> Column a

instance (Show a) => Show (Column a) where
    show (CData xs) = "CData " ++ show xs
    -- TODO: add show for CForm

-- | Supported SpreadSheet columns
data SpreadSheetCol = CInt    (Column Int)
                    | CBool   (Column Bool)
                    | CString (Column String)
    deriving Show

-- | Sum type for the possible column fields
data ColField = FInt Int
              | FBool Bool
              | FString String
              | FForm           -- ^ Empty field for formula column
    deriving Show


-- | Get the `Column` out of `SpreadSheetCol` based on the requested type
getCol :: SpreadSheetCol -> Type a -> Column a
getCol (CInt c)    TInt    = c
getCol (CBool c)   TBool   = c
getCol (CString c) TString = c
getCol _           _       = error "Requested wrong type"


{-
Functions for updating rows
-}

-- | Try to add a field to a spreadsheet column
tryAddField :: SpreadSheetCol -> ColField -> SpreadSheetCol
-- Expand data columns
tryAddField (CInt    (CData xs)) (FInt i)    = CInt    $ CData $ xs ++ [i]
tryAddField (CBool   (CData xs)) (FBool b)   = CBool   $ CData $ xs ++ [b]
tryAddField (CString (CData xs)) (FString s) = CString $ CData $ xs ++ [s]
-- Match formula columns so they don't error
tryAddField c@(CInt    (CForm _)) FForm = c
tryAddField c@(CBool   (CForm _)) FForm = c
tryAddField c@(CString (CForm _)) FForm = c
-- TODO: Error in resulting datatype?
tryAddField _                    _           = error "Couldn't match column datatype with field datatype"


removeIth :: [a] -> Int -> [a]
removeIth []     _ = []
removeIth (x:xs) i 
    | i < 0      = xs
    | otherwise  = removeIth xs (i - 1)

removeRow :: SpreadSheetCol -> Int -> SpreadSheetCol
removeRow _ i | i < 0 = error "i >= 0"
removeRow (CInt     (CData xs)) i = CInt    $ CData $ removeIth xs i
removeRow (CBool    (CData xs)) i = CBool   $ CData $ removeIth xs i
removeRow (CString  (CData xs)) i = CString $ CData $ removeIth xs i
-- Formula based columns cannot remove a row.
removeRow s _ = s
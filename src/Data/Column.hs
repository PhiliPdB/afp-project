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

-- | Supported SpreadSheet columns
data SpreadSheetCol = CInt    (Column Int)
                    | CBool   (Column Bool)
                    | CString (Column String)

-- | Sum type for the possible column fields
data ColField = FInt Int
              | FBool Bool
              | FString String
              | FForm           -- ^ Empty field for formula column


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

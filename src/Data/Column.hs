{-# LANGUAGE GADTs #-}
module Data.Column where

import Data.Formula (Formula)
import Data.Type (Type(..))

-- | Column datatype containing either data or a formula
data Column a where
    CData :: [a]       -> Column a
    CForm :: Formula a -> Column a


data SpreadSheetCols = CInt    (Column Int)
                     | CBool   (Column Bool)
                     | CString (Column String)


-- | Get the @Column@ out of @SpreadSheetCol@ based on the requested type
getCol :: SpreadSheetCols -> Type a -> Column a
getCol (CInt c)    TInt    = c
getCol (CBool c)   TBool   = c
getCol (CString c) TString = c
getCol _           _       = error "Requested wrong type"

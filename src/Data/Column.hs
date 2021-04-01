{-# LANGUAGE GADTs #-}
module Data.Column where

import Data.Formula (Formula)
import Data.Type (Type(..))
import Data.Hourglass

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
data SpreadSheetCol = CInt      (Column Int)
                    | CFloat    (Column Double)
                    | CBool     (Column Bool)
                    | CString   (Column String)
                    | CTime     (Column TimeOfDay)
                    | CWeekDay  (Column WeekDay)
                    | CMonth    (Column Month)
                    | CDate     (Column Date)
                    | CDateTime (Column DateTime)
                    | CDuration (Column Duration)
                    | CPeriod   (Column Period)
    deriving Show

-- | Sum type for the possible column fields
data ColField = FInt      Int
              | FFloat    Double
              | FBool     Bool
              | FString   String
              | FTime     TimeOfDay
              | FWeekDay  WeekDay
              | FMonth    Month
              | FDate     Date
              | FDateTime DateTime
              | FDuration Duration
              | FPeriod   Period
              | FForm           -- ^ Empty field for formula column
    deriving Show


-- | Get the `Column` out of `SpreadSheetCol` based on the requested type
getCol :: SpreadSheetCol -> Type a -> Column a
getCol (CInt c)      TInt      = c
getCol (CFloat c)    TFloat    = c
getCol (CBool c)     TBool     = c
getCol (CString c)   TString   = c
getCol (CTime c)     TTime     = c
getCol (CWeekDay c)  TWeekDay  = c
getCol (CMonth c)    TMonth    = c
getCol (CDate c)     TDate     = c
getCol (CDateTime c) TDateTime = c
getCol (CDuration c) TDuration = c
getCol (CPeriod c)   TPeriod   = c
getCol _           _       = error "Requested wrong type"


{-
Functions for updating rows
-}

-- | Try to add a field to a spreadsheet column
tryAddField :: SpreadSheetCol -> ColField -> SpreadSheetCol
-- Expand data columns
tryAddField (CInt      (CData xs)) (FInt i)      = CInt      $ CData $ xs ++ [i]
tryAddField (CFloat    (CData xs)) (FFloat f)    = CFloat    $ CData $ xs ++ [f]
tryAddField (CBool     (CData xs)) (FBool b)     = CBool     $ CData $ xs ++ [b]
tryAddField (CString   (CData xs)) (FString s)   = CString   $ CData $ xs ++ [s]
tryAddField (CTime     (CData xs)) (FTime t)     = CTime     $ CData $ xs ++ [t]
tryAddField (CWeekDay  (CData xs)) (FWeekDay w)  = CWeekDay  $ CData $ xs ++ [w]
tryAddField (CMonth    (CData xs)) (FMonth m)    = CMonth    $ CData $ xs ++ [m]
tryAddField (CDate     (CData xs)) (FDate d)     = CDate     $ CData $ xs ++ [d]
tryAddField (CDateTime (CData xs)) (FDateTime d) = CDateTime $ CData $ xs ++ [d]
tryAddField (CDuration (CData xs)) (FDuration d) = CDuration $ CData $ xs ++ [d]
tryAddField (CPeriod   (CData xs)) (FPeriod p)   = CPeriod   $ CData $ xs ++ [p]
-- Match formula columns so they don't error
tryAddField c@(CInt      (CForm _)) FForm = c
tryAddField c@(CFloat    (CForm _)) FForm = c
tryAddField c@(CBool     (CForm _)) FForm = c
tryAddField c@(CString   (CForm _)) FForm = c
tryAddField c@(CTime     (CForm _)) FForm = c
tryAddField c@(CWeekDay  (CForm _)) FForm = c
tryAddField c@(CMonth    (CForm _)) FForm = c
tryAddField c@(CDate     (CForm _)) FForm = c
tryAddField c@(CDateTime (CForm _)) FForm = c
tryAddField c@(CDuration (CForm _)) FForm = c
tryAddField c@(CPeriod   (CForm _)) FForm = c
-- TODO: Error in resulting datatype?
tryAddField _                    _           = error "Couldn't match column datatype with field datatype"


removeIth :: [a] -> Int -> [a]
removeIth []     _ = []
removeIth (x:xs) i
    | i < 0      = xs
    | otherwise  = x : removeIth xs (i - 1)

removeRow :: SpreadSheetCol -> Int -> SpreadSheetCol
removeRow _ i | i < 0 = error "i >= 0"
removeRow (CInt      (CData xs)) i = CInt      $ CData $ removeIth xs i
removeRow (CFloat    (CData xs)) i = CFloat    $ CData $ removeIth xs i
removeRow (CBool     (CData xs)) i = CBool     $ CData $ removeIth xs i
removeRow (CString   (CData xs)) i = CString   $ CData $ removeIth xs i
removeRow (CTime     (CData xs)) i = CTime     $ CData $ removeIth xs i
removeRow (CWeekDay  (CData xs)) i = CWeekDay  $ CData $ removeIth xs i
removeRow (CMonth    (CData xs)) i = CMonth    $ CData $ removeIth xs i
removeRow (CDate     (CData xs)) i = CDate     $ CData $ removeIth xs i
removeRow (CDateTime (CData xs)) i = CDateTime $ CData $ removeIth xs i
removeRow (CDuration (CData xs)) i = CDuration $ CData $ removeIth xs i
removeRow (CPeriod   (CData xs)) i = CPeriod   $ CData $ removeIth xs i
-- Formula based columns cannot remove a row.
removeRow s _ = s
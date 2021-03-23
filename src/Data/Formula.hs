{-# LANGUAGE GADTs #-}
module Data.Formula where

import Data.Type (Type, CT)
import Data.Hourglass

-- | Formula language used for making calculations
data Formula a where
    Var        :: String -> Type a -> Formula a
    -- | Cross table column
    CTVar      :: String -- ^ Name of the table
               -> String -- ^ Name of the column
               -> Type a -> Formula a
    -- | Literal of one of the supported column types
    Lit        :: CT a => a -> Formula a
    -- | Equality
    Eq         :: Eq a => Formula a -> Formula a -> Formula Bool
    -- TODO: Also a Not Equal?
    -- Arithmetic
    Prod       :: Formula Int -> Formula Int -> Formula Int
    Add        :: Formula Int -> Formula Int -> Formula Int
    Sub        :: Formula Int -> Formula Int -> Formula Int
    Min        :: Formula Int -> Formula Int -> Formula Int
    Max        :: Formula Int -> Formula Int -> Formula Int
    -- TODO: Number comparison
    -- Boolean logic
    And        :: Formula Bool -> Formula Bool -> Formula Bool
    Or         :: Formula Bool -> Formula Bool -> Formula Bool
    Not        :: Formula Bool -> Formula Bool
    -- Condition
    IfThenElse :: Formula Bool -> Formula a -> Formula a -> Formula a
    -- Time functions
    TimeAdd    :: Time t => Formula t -> Formula Duration -> Formula t
    TimeSub    :: Time t => Formula t -> Formula Duration -> Formula t
    AddPeriod  :: Formula Date -> Formula Period -> Formula Date
    SubPeriod  :: Formula Date -> Formula Period -> Formula Date
    IsLeapYear :: Formula Int  -> Formula Bool
    GetWeekDay :: Formula Date -> Formula WeekDay
    GetYearDay :: Formula Date -> Formula Int
    MonthDays  :: Formula Int  -- ^ for information about leap years
                  -> Formula Month 
                  -> Formula Int 
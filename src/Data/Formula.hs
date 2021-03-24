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
    Lit        :: CT a =>  a  -> Formula a
    Lift       :: CT a => [a] -> Formula a
    -- | Equality
    Eq         :: Eq a => Formula a -> Formula a -> Formula Bool
    NEq        :: Eq a => Formula a -> Formula a -> Formula Bool
    -- Arithmetic
    Prod       :: Formula Int -> Formula Int -> Formula Int
    Add        :: Formula Int -> Formula Int -> Formula Int
    Sub        :: Formula Int -> Formula Int -> Formula Int
    Min        :: Formula Int -> Formula Int -> Formula Int
    Max        :: Formula Int -> Formula Int -> Formula Int
    -- Ordering methods
    -- | Less than
    LT         :: Ord a => Formula a -> Formula a -> Formula Bool
    -- | Less than or equal to
    LEq        :: Ord a => Formula a -> Formula a -> Formula Bool
    -- | Greater than
    GT         :: Ord a => Formula a -> Formula a -> Formula Bool
    -- | Greater than or equal to
    GEq        :: Ord a => Formula a -> Formula a -> Formula Bool

    -- Boolean logic
    And        :: Formula Bool -> Formula Bool -> Formula Bool
    Or         :: Formula Bool -> Formula Bool -> Formula Bool
    Not        :: Formula Bool -> Formula Bool
    -- Condition
    IfThenElse :: Formula Bool -> Formula a -> Formula a -> Formula a
    -- Aggregation
    Aggr :: (CT a, CT b, CT c)
         => String                                   -- ^ Table name of the table we want to aggregate
         -> String -> Type a                         -- ^ Column of the data in the other table we want to filter
         -> String -> Type b                         -- ^ Column in the current table to determine how to group the rows in a
         -> String -> Type c                         -- ^ Column in the other table with the values we should put in the grouped items
         -> (Formula a -> Formula b -> Formula Bool) -- ^ Function to decide the grouping of columns a and b
         -> (Formula [c] -> Formula d)               -- ^ Aggregation function to map the values to a single value
         -> Formula d

    Sum :: Formula [Int] -> Formula Int
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

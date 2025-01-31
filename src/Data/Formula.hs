{-# LANGUAGE GADTs #-}
module Data.Formula (Formula(..), colRefs) where
import Prelude hiding (LT, GT)

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
    Prod       :: Num a => Formula a -> Formula a -> Formula a
    Add        :: Num a => Formula a -> Formula a -> Formula a
    Sub        :: Num a => Formula a -> Formula a -> Formula a
    -- Ordering methods
    Min        :: Ord a => Formula a -> Formula a -> Formula a
    Max        :: Ord a => Formula a -> Formula a -> Formula a
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

    -- Aggregator functions
    Filter  :: CT a => (a -> Bool) -> Formula [a] -> Formula [a]
    Sum     :: Num a => Formula [a] -> Formula a
    Average :: Formula [Double] -> Formula Double
    Length  :: CT a => Formula [a] -> Formula Int

    -- Time functions
    TimeAdd    :: Time t => Formula t -> Formula Duration -> Formula t
    TimeSub    :: Time t => Formula t -> Formula Duration -> Formula t
    AddPeriod  :: Formula Date -> Formula Period -> Formula Date
    SubPeriod  :: Formula Date -> Formula Period -> Formula Date
    IsLeapYear :: Formula Int  -> Formula Bool
    GetWeekDay :: Formula Date -> Formula WeekDay
    GetYearDay :: Formula Date -> Formula Int
    MonthDays  :: Formula Int    -- ^ for information about leap years
               -> Formula Month
               -> Formula Int

    ToInt   :: Formula Double -> Formula Int
    ToFloat :: Formula Int    -> Formula Double


-- | Given the name of the current column,
-- Returns all references to other (table, column) within the formula
colRefs :: String -> Formula a -> [(String, String)]
colRefs c (Var s _)                           = [(c, s)]
colRefs _ (CTVar fc s _)                      = [(fc, s)]
colRefs _ (Lit _)                             = []
colRefs _ (Lift _)                            = []
colRefs c (Eq x y)                            = colRefs c x ++ colRefs c y
colRefs c (NEq x y)                           = colRefs c x ++ colRefs c y
colRefs c (Prod x y)                          = colRefs c x ++ colRefs c y
colRefs c (Add x y)                           = colRefs c x ++ colRefs c y
colRefs c (Sub x y)                           = colRefs c x ++ colRefs c y
colRefs c (Min x y)                           = colRefs c x ++ colRefs c y
colRefs c (Max x y)                           = colRefs c x ++ colRefs c y
colRefs c (LT x y)                            = colRefs c x ++ colRefs c y
colRefs c (GT x y)                            = colRefs c x ++ colRefs c y
colRefs c (GEq x y)                           = colRefs c x ++ colRefs c y
colRefs c (LEq x y)                           = colRefs c x ++ colRefs c y
colRefs c (And x y)                           = colRefs c x ++ colRefs c y
colRefs c (Or x y)                            = colRefs c x ++ colRefs c y
colRefs c (Not x)                             = colRefs c x
colRefs c (IfThenElse x y z)                  = colRefs c x ++ colRefs c y ++ colRefs c z
colRefs c (Aggr t c1 _ c2 _ c3 _ _cond _aggr) = [(t, c1), (c, c2), (t, c3)]
colRefs c (Filter _ x)                        = colRefs c x
colRefs c (Sum x)                             = colRefs c x
colRefs c (Average x)                         = colRefs c x
colRefs c (Length x)                          = colRefs c x
colRefs c (TimeAdd x y)                       = colRefs c x ++ colRefs c y
colRefs c (TimeSub x y)                       = colRefs c x ++ colRefs c y
colRefs c (AddPeriod x y)                     = colRefs c x ++ colRefs c y
colRefs c (SubPeriod x y)                     = colRefs c x ++ colRefs c y
colRefs c (IsLeapYear x )                     = colRefs c x
colRefs c (GetWeekDay x)                      = colRefs c x
colRefs c (GetYearDay x )                     = colRefs c x
colRefs c (MonthDays x y)                     = colRefs c x ++ colRefs c y
colRefs c (ToInt d)                           = colRefs c d
colRefs c (ToFloat i)                         = colRefs c i

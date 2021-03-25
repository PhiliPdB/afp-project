{-# LANGUAGE GADTs #-}
module Data.Formula where

import Data.Type (Type, CT)

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
    -- TODO: Also a Not Equal?
    -- Arithmetic
    Prod       :: Formula Int -> Formula Int -> Formula Int
    Add        :: Formula Int -> Formula Int -> Formula Int
    Sub        :: Formula Int -> Formula Int -> Formula Int
    Min        :: Formula Int -> Formula Int -> Formula Int
    Max        :: Formula Int -> Formula Int -> Formula Int
    -- TODO: Number comparison

    -- LEq        :: Ord a => Formula a -> Formula a -> Formula Bool

    -- Boolean logic
    And        :: Formula Bool -> Formula Bool -> Formula Bool
    Or         :: Formula Bool -> Formula Bool -> Formula Bool
    Not        :: Formula Bool -> Formula Bool
    -- Condition
    IfThenElse :: Formula Bool -> Formula a -> Formula a -> Formula a

    -- Nullable
    Fill       :: Formula (Maybe Int) -> Formula Int -> Formula Int
    

    Aggr :: (CT a, CT b, CT c)
         => String                                   -- ^ Table name of the table we want to aggregate
         -> String -> Type a                         -- ^ Column of the data in the other table we want to filter
         -> String -> Type b                         -- ^ Column in the current table to determine how to group the rows in a
         -> String -> Type c                         -- ^ Column in the other table with the values we should put in the grouped items
         -> (Formula a -> Formula b -> Formula Bool) -- ^ Function to decide the grouping of columns a and b
         -> (Formula [c] -> Formula d)               -- ^ Aggregation function to map the values to a single value
         -> Formula d

    Sum :: Formula [Int] -> Formula Int

{-
Year  |
2021  | ([a] -> c)
2020  |
2019  |
...


-}



{-# LANGUAGE GADTs #-}
module Data.SpreadSheet where

import Data.Column (SpreadSheetCols(..), Column (..), getCol)
import Data.Formula (Formula(..))

-- | SpreadSheet is defined as list of columns 'indexed' on a column name
newtype SpreadSheet = SpreadSheet [(String, SpreadSheetCols)]

-- | Evaluate a given formula on a given spreadsheet
evalF :: Formula a -> SpreadSheet -> [a]
evalF (Var x t) s@(SpreadSheet cs) = case lookup x cs of
    Just c -> case getCol c t of
        CData v -> v
        CForm f -> evalF f s
    Nothing -> error $ "No column with name '" ++ x ++ "'"
evalF (Lit a)    _ = repeat a
-- Equality
evalF (Eq a b)   s = zipWith (==) (evalF a s) (evalF b s)
-- Arithmetic
evalF (Prod a b) s = zipWith (*)  (evalF a s) (evalF b s)
evalF (Add  a b) s = zipWith (+)  (evalF a s) (evalF b s)
evalF (Sub  a b) s = zipWith (-)  (evalF a s) (evalF b s)
evalF (Min  a b) s = zipWith min  (evalF a s) (evalF b s)
evalF (Max  a b) s = zipWith max  (evalF a s) (evalF b s)
-- Boolean logic
evalF (And a b)  s = zipWith (&&) (evalF a s) (evalF b s)
evalF (Or  a b)  s = zipWith (||) (evalF a s) (evalF b s)
evalF (Not a)    s = map     not  (evalF a s)
-- If then else
evalF (IfThenElse c a b) s = map f (zip3 c' a' b')
    where c' = evalF c s
          a' = evalF a s
          b' = evalF b s

          f (True,  x, _) = x
          f (False, _, y) = y

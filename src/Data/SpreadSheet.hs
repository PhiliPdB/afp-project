{-# LANGUAGE GADTs #-}
module Data.SpreadSheet where

import Data.Column (SpreadSheetCol(..), Column (..), getCol)
import Data.Formula (Formula(..))

-- | SpreadSheet is defined as list of columns /indexed/ on a column name
data SpreadSheet = SpreadSheet Int [(String, SpreadSheetCol)]

-- | Evaluate a given formula on a given spreadsheet
evalF :: Formula a -> SpreadSheet -> [a]
evalF (Var x t) s@(SpreadSheet _ cs) = case lookup x cs of
    Just c -> case getCol c t of
        CData v -> v
        CForm f -> evalF f s
    Nothing -> error $ "No column with name '" ++ x ++ "'"
evalF (Lit a)    (SpreadSheet n _) = replicate n a
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

-- | Try to evaluate a `SpreadSheetCol` if it contains a formula.
--   Otherwise, just the data is returned. This function thus always
--   returns a `CData` column.
-- TODO: Remove code duplication if possible
tryEvalSpreadSheetCol :: SpreadSheetCol -> SpreadSheet -> SpreadSheetCol
tryEvalSpreadSheetCol (CInt c)    s = case c of
    CData d -> CInt    $ CData d
    CForm f -> CInt    $ CData $ evalF f s
tryEvalSpreadSheetCol (CBool c)   s = case c of
    CData d -> CBool   $ CData d
    CForm f -> CBool   $ CData $ evalF f s
tryEvalSpreadSheetCol (CString c) s = case c of
    CData d -> CString $ CData d
    CForm f -> CString $ CData $ evalF f s

-- | Evaluate a whole spreadsheet and get the columns with only data
evalSpreadSheet :: SpreadSheet -> [(String, SpreadSheetCol)]
evalSpreadSheet s@(SpreadSheet _ cs) = map eval cs
    where -- TODO: This could possibly lead to evaluating formulas twice
          eval (n, col) = (n, tryEvalSpreadSheetCol col s)

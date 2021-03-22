{-# LANGUAGE GADTs #-}
module Data.SpreadSheet where

import Data.Column (SpreadSheetCol(..), Column (..), getCol, ColField, tryAddField, removeRow)
import Data.Formula (Formula(..))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.List (nub)


-- | SpreadSheet is defined as list of columns /indexed/ on a column name
-- TODO: Don't export the constructor
data SpreadSheet = SpreadSheet Int [(String, SpreadSheetCol)]
    deriving Show

uniformLength :: [SpreadSheetCol] -> Bool
uniformLength cs = 
    case lengthOfCols of
        [] -> True
        (c:cs) -> all (==c) cs 
  where 
        lengthOfCols :: [Int]
        lengthOfCols = foldl f [] cs
        f :: [Int] -> SpreadSheetCol -> [Int] 
        f is (CInt (CData d)) = length d : is
        f is (CString (CData d)) = length d : is
        f is (CBool (CData d)) = length d : is
        f is  _ = is

spreadSheet :: [(String, SpreadSheetCol)] -> Maybe SpreadSheet
spreadSheet [] = Just $ SpreadSheet 0 []
spreadSheet cs@(c:_) 
    | hasUniqueColumnNames && uniformLength cols = Just $ SpreadSheet (length c) cs
    | otherwise                                          = Nothing
  where hasUniqueColumnNames = length (nub names) == length names
        cols = map snd cs
        names = map fst cs 

-- | Collection of multiple spreadsheets
--   TODO: Might want to use newtype, to control how spreadsheets are added and deleted from the map
type SpreadSheetEnv = Map String SpreadSheet


-- | Evaluate a given formula on a given spreadsheet
evalF :: Formula a -> SpreadSheet -> SpreadSheetEnv -> [a]
evalF (Var x t) s@(SpreadSheet _ cs) env = case lookup x cs of
    Just c -> case getCol c t of
        CData v -> v
        CForm f -> evalF f s env
    Nothing -> error $ "No column with name '" ++ x ++ "'"
evalF (CTVar tn cn t) (SpreadSheet n _) env = fromMaybe (error "Table or column not found") $ do
    table@(SpreadSheet m cs) <- M.lookup tn env
    if m < n then -- Check if the column in the other table is big enough
        error $ "Table '" ++ tn ++ "' doesn't have enough rows"
    else do
        col <- lookup cn cs
        case getCol col t of
            CData v -> return $ take n v -- Make sure that we take the first `n` items of the data
            CForm f -> return $ take n $ evalF f table env
evalF (Lit a) (SpreadSheet n _) _= replicate n a
-- Equality
evalF (Eq a b)   s env = zipWith (==) (evalF a s env) (evalF b s env)
-- Arithmetic
evalF (Prod a b) s env = zipWith (*)  (evalF a s env) (evalF b s env)
evalF (Add  a b) s env = zipWith (+)  (evalF a s env) (evalF b s env)
evalF (Sub  a b) s env = zipWith (-)  (evalF a s env) (evalF b s env)
evalF (Min  a b) s env = zipWith min  (evalF a s env) (evalF b s env)
evalF (Max  a b) s env = zipWith max  (evalF a s env) (evalF b s env)
-- Boolean logic
evalF (And a b)  s env = zipWith (&&) (evalF a s env) (evalF b s env)
evalF (Or  a b)  s env = zipWith (||) (evalF a s env) (evalF b s env)
evalF (Not a)    s env = map     not  (evalF a s env)
-- If then else
evalF (IfThenElse c a b) s env = map f (zip3 c' a' b')
    where c' = evalF c s env
          a' = evalF a s env
          b' = evalF b s env

          f (True,  x, _) = x
          f (False, _, y) = y

-- | Try to evaluate a `SpreadSheetCol` if it contains a formula.
--   Otherwise, just the data is returned. This function thus always
--   returns a `CData` column.
-- TODO: Remove code duplication if possible
tryEvalSpreadSheetCol :: SpreadSheetCol -> SpreadSheet -> SpreadSheetEnv -> SpreadSheetCol
tryEvalSpreadSheetCol (CInt c)    s env = case c of
    CData d -> CInt    $ CData d
    CForm f -> CInt    $ CData $ evalF f s env
tryEvalSpreadSheetCol (CBool c)   s env = case c of
    CData d -> CBool   $ CData d
    CForm f -> CBool   $ CData $ evalF f s env
tryEvalSpreadSheetCol (CString c) s env = case c of
    CData d -> CString $ CData d
    CForm f -> CString $ CData $ evalF f s env

-- | Evaluate a whole spreadsheet and get the columns with only data
evalSpreadSheet :: SpreadSheet -> SpreadSheetEnv -> [(String, SpreadSheetCol)]
evalSpreadSheet s@(SpreadSheet _ cs) env = map eval cs
    where -- TODO: This could possibly lead to evaluating formulas twice
          eval (n, col) = (n, tryEvalSpreadSheetCol col s env)


{-
Functions for spreadsheet updating
-}

-- | Try to add a row of data to the spreadsheet
tryAddRow :: SpreadSheet -> [ColField] -> SpreadSheet
tryAddRow (SpreadSheet n cs) row | length cs == length row = SpreadSheet (n + 1) $ zipWith tryAddItem cs row
                                 | otherwise               = error "Unmatched length" -- TODO: Error in returning datatype?
    where tryAddItem :: (String, SpreadSheetCol) -> ColField -> (String, SpreadSheetCol)
          tryAddItem (s, c) f = (s, tryAddField c f)

-- | Remove a row of data from the spreadsheet
removeRow :: SpreadSheet -> Int -> SpreadSheet
removeRow (SpreadSheet n cs) i
        | i < 0     = error "i >= 0"
        | otherwise = SpreadSheet (n - 1) (fmap (`removeRowFromColumn` i) cs)
    where removeRowFromColumn :: (String, SpreadSheetCol) -> Int -> (String, SpreadSheetCol)
          removeRowFromColumn (s, c) i = (s, Data.Column.removeRow c i)

delFromAL :: Eq key => [(key, a)] -> key -> [(key, a)]
delFromAL l key = filter (\a -> fst a /= key) l

-- | Remove a row of data from the spreadsheet
removeColumn :: SpreadSheet -> String -> SpreadSheet
removeColumn (SpreadSheet n cs) k = SpreadSheet n (delFromAL cs k)

insertAt :: [a] -> a -> Int -> [a]
insertAt xs y 0 = y:xs
insertAt [] y i = [y]
insertAt (x:xs) y i
    | i >= 0    = x : insertAt xs y (i - 1)
    | otherwise = error "i >= 0"

tryAddColumn :: SpreadSheet -> Int -> (String, SpreadSheetCol) -> SpreadSheet
tryAddColumn s@(SpreadSheet n cs) i c@(k, _)
    | i >= 0    = if k `elem` map fst cs then s else SpreadSheet n (insertAt cs c i)
    | otherwise = error "i >= 0"


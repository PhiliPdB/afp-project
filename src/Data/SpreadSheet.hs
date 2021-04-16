{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
module Data.SpreadSheet (SpreadSheetColumnData(..), SpreadSheetEnv(..), SpreadSheet(..), evalSpreadSheet,
                        spreadSheet, spreadSheetEnv, isValidSpreadSheet,
                        addSpreadSheet, evalF, tryAddRow, Data.SpreadSheet.removeRow, tryAddColumn, removeColumn) where

import Prelude hiding (LT, GT)

import Data.Column as C (SpreadSheetCol(..), Column (..), getCol, ColField, tryAddField, removeRow)
import qualified Data.Column as C
import Data.Formula (Formula(..), colRefs)
import Data.Map (Map)
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe (isJust, fromMaybe)
import Data.Hourglass
import Data.List (findIndices, nub)
import Data.Type (Type)
import Data.TimeHelper

-- | SpreadSheet is defined as list of columns /indexed/ on a column name
data SpreadSheet = SpreadSheet Int [(String, SpreadSheetCol)]
    deriving Show

uniformLength :: [SpreadSheetCol] -> Bool
uniformLength cols =
    case lengthOfCols of
        [] -> True
        (c:cs) -> all (==c) cs
  where
        lengthOfCols :: [Int]
        lengthOfCols = foldl f [] cols
        f :: [Int] -> SpreadSheetCol -> [Int]
        f is (CInt (CData d))    = length d : is
        f is (CString (CData d)) = length d : is
        f is (CBool (CData d))   = length d : is
        f is  _ = is

-- | Construct a SpreadSheet
-- The function expects an associated list of column names paired with SpreadSheetCol
-- The associated list must have unique keys, each SpreadSheetCol containing CData must have uniform length.
-- Otherwise a Nothing is returned.
spreadSheet :: [(String, SpreadSheetCol)] -> Maybe SpreadSheet
spreadSheet [] = Just $ SpreadSheet 0 []
spreadSheet cs@(c:_)
    | hasUniqueColumnNames && uniformLength cols = Just $ SpreadSheet (length c) cs
    | otherwise                                          = Nothing
  where hasUniqueColumnNames = length (nub names) == length names
        cols = map snd cs
        names = map fst cs

-- | Collection of multiple spreadsheets
--   TODO: Might want to add a function to remove spreadsheets.
--         Check whether any removal invalidates the spreadsheet env
--         for instance don't remove spreadsheets that are referred to.
newtype SpreadSheetEnv = SpreadSheetEnv (Map String SpreadSheet)

-- | Constructs a SpreadSheet environment from a Map of spreadsheet names and spreadsheets
spreadSheetEnv :: Map String SpreadSheet -> SpreadSheetEnv
spreadSheetEnv = SpreadSheetEnv

-- | Ensures the spreadsheet is valid and does not contain formula's referring to non-existent columns
isValidSpreadSheet :: (String, SpreadSheet) -> SpreadSheetEnv -> Bool
isValidSpreadSheet (name, spreadsheet) (SpreadSheetEnv env) = referredColumns spreadsheet `S.isSubsetOf` existingColumns
    where referredColumns :: SpreadSheet -> S.Set (String, String)
          referredColumns (SpreadSheet _ cs) = S.fromList $ concatMap (g name . snd) cs
          -- finds referred columns in formula's (and pairs them with the spreadsheet name)
          g :: String -> SpreadSheetCol -> [(String, String)]
          g s (CInt      (CForm f)) = colRefs s f
          g s (CFloat    (CForm f)) = colRefs s f
          g s (CBool     (CForm f)) = colRefs s f
          g s (CString   (CForm f)) = colRefs s f
          g s (CTime     (CForm f)) = colRefs s f
          g s (CWeekDay  (CForm f)) = colRefs s f
          g s (CMonth    (CForm f)) = colRefs s f
          g s (CDate     (CForm f)) = colRefs s f
          g s (CDateTime (CForm f)) = colRefs s f
          g s (CDuration (CForm f)) = colRefs s f
          g s (CPeriod   (CForm f)) = colRefs s f
          g _ _                     = []

          newEnv = M.insert name spreadsheet env

          existingColumns :: S.Set (String, String)
          existingColumns = S.fromList $ concatMap (\(n, SpreadSheet _ cs) -> map ((n,) . fst) cs) (M.assocs newEnv)

-- | Add a (name, SpreadSheet) pair to the spreadsheet environment.
-- If the spreadsheet name already exists, or the spreadsheet refers to non-existing spreadsheets in
-- its formula's, a Nothing is returned.
addSpreadSheet :: (String, SpreadSheet) -> SpreadSheetEnv -> Maybe SpreadSheetEnv
addSpreadSheet (n, s) env@(SpreadSheetEnv envMap)
                | isJust (M.lookup n envMap) = Nothing
                | isValidSpreadSheet (n, s) env = Just (SpreadSheetEnv $ M.insert n s envMap)
                | otherwise = Nothing

-- | Evaluate a given formula on a given spreadsheet
evalF :: Formula a -> SpreadSheet -> SpreadSheetEnv -> [a]
evalF (Var x t) s@(SpreadSheet _ cs) env = case lookup x cs of
    Just c -> case getCol c t of
        CData v -> v
        CForm f -> evalF f s env
    Nothing -> error $ "No column with name '" ++ x ++ "'"
evalF (CTVar tn cn t) (SpreadSheet n _) env@(SpreadSheetEnv envMap) = fromMaybe (error "Table or column not found") $ do
    table@(SpreadSheet m cs) <- M.lookup tn envMap
    if m < n then -- Check if the column in the other table is big enough
        error $ "Table '" ++ tn ++ "' doesn't have enough rows"
    else do
        col <- lookup cn cs
        case getCol col t of
            CData v -> return $ take n v -- Make sure that we take the first `n` items of the data
            CForm f -> return $ take n $ evalF f table env
evalF (Lit  a) (SpreadSheet n _) _ = replicate n a
evalF (Lift a) (SpreadSheet n _) _ | length a == n = a
                                   | otherwise     = error "Length doesn't match"
-- Equality
evalF (Eq a b)   s env = zipWith (==) (evalF a s env) (evalF b s env)
evalF (NEq a b)  s env = zipWith (/=) (evalF a s env) (evalF b s env)
-- Arithmetic
evalF (Prod a b) s env = zipWith (*)  (evalF a s env) (evalF b s env)
evalF (Add  a b) s env = zipWith (+)  (evalF a s env) (evalF b s env)
evalF (Sub  a b) s env = zipWith (-)  (evalF a s env) (evalF b s env)
evalF (Min  a b) s env = zipWith min  (evalF a s env) (evalF b s env)
evalF (Max  a b) s env = zipWith max  (evalF a s env) (evalF b s env)
-- Ordering
evalF (LT   a b) s env = zipWith (<)  (evalF a s env) (evalF b s env)
evalF (LEq  a b) s env = zipWith (<=) (evalF a s env) (evalF b s env)
evalF (GT   a b) s env = zipWith (>)  (evalF a s env) (evalF b s env)
evalF (GEq  a b) s env = zipWith (>=) (evalF a s env) (evalF b s env)
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
evalF (Aggr t c1 t1 c2 t2 c3 t3 cond aggr) table env@(SpreadSheetEnv envMap) = fromMaybe (error "Something went wrong") $ do
        -- First we lookup the second table in the environment
        cTable <- M.lookup t envMap
        -- Get the data from all the columns
        c1data <- getData c1 t1 cTable
        c2data <- getData c2 t2  table
        c3data <- getData c3 t3 cTable

        -- First we will group all the items from column 1 based on the given condition function.
        -- Here, we look for each item b in c2, which items in c1 fulfill this condition. And then group the indeces
        -- of the items in c1 together.

        -- Helper function to find the indeces in c1 where the condition compared to b holds.
        let findIndecesInC1 b = findIndices (\a -> head (evalF (cond (Lit a) (Lit b)) (SpreadSheet 1 []) (SpreadSheetEnv M.empty))) c1data
        -- Find the indeces for each item in c2data
        let c2aggr = map findIndecesInC1 c2data
        -- With these indeces collected, we can map them to the corresponding data in c3.
        let c3aggr = map (map (c3data !!)) c2aggr
        -- Then we run the aggregator function over these items, such that we have a single value per row.
        return $ evalF (aggr (Lift c3aggr)) table env
    where -- | Function to extract data from a column of certain type that's inside the given spreadsheet
          getData :: String -> Type a -> SpreadSheet -> Maybe [a]
          getData colName colType tab@(SpreadSheet _ cols) =
              do col <- lookup colName cols
                 case getCol col colType of
                     CData v -> return v
                     CForm f -> return $ evalF f tab env

evalF (Filter f x) s env = map (filter f) (evalF x s env)
evalF (Sum x)      s env = map sum (evalF x s env)
evalF (Average x)  s env = map average (evalF x s env)
    where average xs = sum xs / fromIntegral (length xs)
evalF (Length x)   s env = map length (evalF x s env)

-- Time functions
evalF (TimeAdd t d)   s env = zipWith timeAdd         (evalF t s env) (evalF d s env)
evalF (TimeSub t d)   s env = zipWith timeAdd         (evalF t s env) (map invDur (evalF d s env))
evalF (AddPeriod d p) s env = zipWith dateAddPeriod   (evalF d s env) (evalF p s env)
evalF (SubPeriod d p) s env = zipWith dateAddPeriod   (evalF d s env) (map invPer (evalF p s env))
evalF (IsLeapYear d)  s env = map     isLeapYear      (evalF d s env)
evalF (GetWeekDay d)  s env = map     getWeekDay      (evalF d s env)
evalF (GetYearDay d)  s env = map     getDayOfTheYear (evalF d s env)
evalF (MonthDays y m) s env = zipWith daysInMonth     (evalF y s env) (evalF m s env)

evalF (ToInt f)   s env = map round        (evalF f s env)
evalF (ToFloat i) s env = map fromIntegral (evalF i s env)


data SpreadSheetColumnData = DInt      [Int]
                           | DFloat    [Double]
                           | DBool     [Bool]
                           | DString   [String]
                           | DTime     [TimeOfDay]
                           | DWeekDay  [WeekDay]
                           | DMonth    [Month]
                           | DDate     [Date]
                           | DDateTime [DateTime]
                           | DDuration [Duration]
                           | DPeriod   [Period]
    deriving (Show, Eq)

-- | Try to evaluate a `SpreadSheetCol` if it contains a formula.
--   Otherwise, just the data is returned.
-- TODO: Remove code duplication if possible
tryEvalSpreadSheetCol :: SpreadSheetCol -> SpreadSheet -> SpreadSheetEnv -> SpreadSheetColumnData
tryEvalSpreadSheetCol (CInt c)    s env = case c of
    CData d -> DInt      d
    CForm f -> DInt      $ evalF f s env
tryEvalSpreadSheetCol (CFloat c)  s env = case c of
    CData d -> DFloat    d
    CForm f -> DFloat    $ evalF f s env
tryEvalSpreadSheetCol (CBool c)   s env = case c of
    CData d -> DBool     d
    CForm f -> DBool     $ evalF f s env
tryEvalSpreadSheetCol (CString c) s env = case c of
    CData d -> DString   d
    CForm f -> DString   $ evalF f s env
tryEvalSpreadSheetCol (CTime c)     s env = case c of
    CData d -> DTime     d
    CForm f -> DTime     $ evalF f s env
tryEvalSpreadSheetCol (CWeekDay c)  s env = case c of
    CData d -> DWeekDay  d
    CForm f -> DWeekDay  $ evalF f s env
tryEvalSpreadSheetCol (CMonth c)    s env = case c of
    CData d -> DMonth    d
    CForm f -> DMonth    $ evalF f s env
tryEvalSpreadSheetCol (CDate c)     s env = case c of
    CData d -> DDate     d
    CForm f -> DDate     $ evalF f s env
tryEvalSpreadSheetCol (CDateTime c) s env = case c of
    CData d -> DDateTime d
    CForm f -> DDateTime $ evalF f s env
tryEvalSpreadSheetCol (CDuration c) s env = case c of
    CData d -> DDuration d
    CForm f -> DDuration $ evalF f s env
tryEvalSpreadSheetCol (CPeriod c)   s env = case c of
    CData d -> DPeriod   d
    CForm f -> DPeriod   $ evalF f s env

-- | Evaluate a whole spreadsheet and get the columns with only data
evalSpreadSheet :: SpreadSheet -> SpreadSheetEnv -> [(String, SpreadSheetColumnData)]
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
          removeRowFromColumn (s, c) i = (s, C.removeRow c i)

delFromAL :: Eq key => [(key, a)] -> key -> [(key, a)]
delFromAL l key = filter (\a -> fst a /= key) l

-- | Remove a row of data from the spreadsheet
removeColumn :: SpreadSheet -> String -> SpreadSheet
removeColumn (SpreadSheet n cs) k = SpreadSheet n (delFromAL cs k)

insertAt :: [a] -> a -> Int -> [a]
insertAt xs y 0 = y:xs
insertAt [] y _ = [y]
insertAt (x:xs) y i
    | i >= 0    = x : insertAt xs y (i - 1)
    | otherwise = error "i >= 0"

tryAddColumn :: SpreadSheet -> Int -> (String, SpreadSheetCol) -> SpreadSheet
tryAddColumn s@(SpreadSheet n cs) i c@(k, col)
    | n /= C.length col n = error "Column doesn't match size of the spreadsheet"
    | i >= 0              = if k `elem` map fst cs then s else SpreadSheet n (insertAt cs c i)
    | otherwise           = error "i >= 0"


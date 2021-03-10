{-# LANGUAGE GADTs #-}
module Data.SpreadSheet where

import Data.Column (SpreadSheetCol(..), Column (..), getCol)
import Data.Formula (Formula(..))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

-- | SpreadSheet is defined as list of columns /indexed/ on a column name
data SpreadSheet = SpreadSheet Int [(String, SpreadSheetCol)]

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
-- TODO: What about the size of the other spreadsheet?
evalF (CTVar tn cn t) _ env = fromMaybe (error "Table or column not found") $ do
    table@(SpreadSheet _ cs) <- M.lookup tn env
    col <- lookup cn cs
    case getCol col t of
        CData v -> return v
        CForm f -> return $ evalF f table env
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

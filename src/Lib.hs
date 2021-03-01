{-# LANGUAGE GADTs #-}
module Lib
  ( someFunc
  )
where

import           Prelude                 hiding ( zipWith )
import           Data.Vector                    ( Vector(..)
                                                , zipWith
                                                )
import           Data.Maybe                     ( fromJust )

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type Number = Float -- will need to print rounded up nicely

data Data = VNumber (Vector Number) | VString (Vector String) | VBool (Vector Bool)
  deriving Eq



-- infer type of column with formula?
-- data Formula = F

-- data Formula a where
--   Var  :: String         -> Type a         -> Formula a
--   Prod :: Formula Number -> Formula Number -> Formula Number
--   Add  :: Formula Number -> Formula Number -> Formula Number
--   Sub  :: Formula Number -> Formula Number -> Formula Number
--   Eq   :: Formula a      -> Formula a      -> Formula Bool
--   Min  :: Formula Number -> Formula Number -> Formula Number
--   Max  :: Formula Number -> Formula Number -> Formula Number
--   And  :: Formula Bool   -> Formula Bool   -> Formula Bool
--   Or   :: Formula Bool   -> Formula Bool   -> Formula Bool

data Error = WrongTypeBro | ColumnDoesNotExistBro

data Formula = Var String
             | Add Formula Formula
             | Prod Formula Formula
             | Sub Formula Formula
             | Min Formula Formula
             | Max Formula Formula
             | Eq Formula Formula

data Column = ColumnD Data | ColumnF Formula


newtype Spreadsheet = Spreadsheet [(String, Column)]


evaluateF :: Formula -> Spreadsheet -> Either Error Data
evaluateF (Var x) s@(Spreadsheet cs) = case col of
  ColumnD v -> Right v
  ColumnF f -> evaluateF f s
 where
  col :: Column
  col = fromJust $ lookup x cs
evaluateF (Add  x y) s = arithMatic (+) x y s
evaluateF (Prod x y) s = arithMatic (*) x y s
evaluateF (Sub  x y) s = arithMatic (-) x y s
evaluateF (Min  x y) s = arithMatic min x y s
evaluateF (Max  x y) s = arithMatic max x y s
-- evaluateF (Eq x y) ss  = arithMatic (==) (evaluateF x) (evaluateF y)


 -- case (evaluateF x ss, evaluateF y ss) of
 -- (VNumber s1, VNumber s2) -> Left $ VNumber $ zipWith (+) s1 s2
 -- _                        -> Right WrongTypeBro

type F = (Int -> Int -> Int, Bool -> Bool -> Bool)

arithMatic
  :: (Number -> Number -> Number)
  -> Formula
  -> Formula
  -> Spreadsheet
  -> Either Error Data
arithMatic f x y ss = do
  x' <- evaluateF x ss
  y' <- evaluateF y ss
  case (x', y') of
    (VNumber s1, VNumber s2) -> return $ VNumber $ zipWith f s1 s2
    _                        -> Left WrongTypeBro

boolLogic
  :: (Bool -> Bool -> Bool)
  -> Formula
  -> Formula
  -> Spreadsheet
  -> Either Error Data
boolLogic f x y ss = do
  x' <- evaluateF x ss
  y' <- evaluateF y ss
  case (x', y') of
    (VBool s1, VBool s2) -> return $ VBool $ zipWith f s1 s2
    _                    -> Left WrongTypeBro

test x y ss = do
  x' <- evaluateF x ss
  y' <- evaluateF y ss
  return (x', y')

data Type a where
  TNumber ::Type Number

-- $A + $B
-- product $<COLUMN_ID>


-- Var "column1" inferType

class SSType a where
  inferType :: Type a

-- data Formula a = Var String (Type a)



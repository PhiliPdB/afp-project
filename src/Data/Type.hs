{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.Type where

import Data.Hourglass

-- | Error message for problematic situations.
newtype ErrMsg = ErrMsg String

-- | Possible variable types
data Type a where
    TInt     :: Type Int
    TBool    :: Type Bool
    TString  :: Type String
    TTime    :: Type TimeOfDay
    TWeekDay :: Type WeekDay
    TMonth   :: Type Month
    TDate    :: Type Date

-- | Common column-type functions
class CT a where
    inferType :: Type a

instance CT Int where
    inferType = TInt

instance CT Bool where
    inferType = TBool

instance CT String where
    inferType = TString

instance CT TimeOfDay where
    inferType = TTime

instance CT WeekDay where
    inferType = TWeekDay

instance CT Month where
    inferType = TMonth

instance CT Date where
    inferType = TDate
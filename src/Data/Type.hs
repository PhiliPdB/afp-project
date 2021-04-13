{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.Type where

import Data.Hourglass

-- | Error message for problematic situations.
newtype ErrMsg = ErrMsg String
    deriving(Show)

-- | Possible variable types
data Type a where
    TInt      :: Type Int
    TFloat    :: Type Double
    TBool     :: Type Bool
    TString   :: Type String
    TArray    :: Type a -> Type [a]
    TTime     :: Type TimeOfDay
    TWeekDay  :: Type WeekDay
    TMonth    :: Type Month
    TDate     :: Type Date
    TDateTime :: Type DateTime
    TDuration :: Type Duration
    TPeriod   :: Type Period

-- | Common column-type functions
class CT a where
    inferType :: Type a

instance CT Int where
    inferType = TInt

instance CT Double where
    inferType = TFloat

instance CT Bool where
    inferType = TBool

instance {-# OVERLAPPING #-} CT String where
    inferType = TString

instance {-# OVERLAPPABLE #-} CT a => CT [a] where
    inferType = TArray inferType

instance CT TimeOfDay where
    inferType = TTime

instance CT WeekDay where
    inferType = TWeekDay

instance CT Month where
    inferType = TMonth

instance CT Date where
    inferType = TDate

instance CT DateTime where
    inferType = TDateTime

instance CT Duration where
    inferType = TDuration

instance CT Period where
    inferType = TPeriod

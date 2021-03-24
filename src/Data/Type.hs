{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.Type where

-- | Error message for problematic situations.
newtype ErrMsg = ErrMsg String

-- | Possible variable types
data Type a where
    TInt    :: Type Int
    TBool   :: Type Bool
    TString :: Type String
    TArray  :: Type a -> Type [a]

-- | Common column-type functions
class CT a where
    inferType :: Type a

instance CT Int where
    inferType = TInt

instance CT Bool where
    inferType = TBool

instance CT String where
    inferType = TString

instance CT a => CT [a] where
    inferType = TArray inferType

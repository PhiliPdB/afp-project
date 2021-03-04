{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.Type where

-- | Possible variable types
data Type a where
    TInt    :: Type Int
    TBool   :: Type Bool
    TString :: Type String


class CT a where
    inferType :: Type a

instance CT Int where
    inferType = TInt

instance CT Bool where
    inferType = TBool

instance CT String where
    inferType = TString

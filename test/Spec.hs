import Data.SpreadSheet
import Data.Column
import Test.Tasty.QuickCheck as QC
import Test.Tasty
-- https://hackage.haskell.org/package/QuickCheck-2.14.2/docs/Test-QuickCheck-Gen.html#t:Gen

import Data.Map.Ordered as OM


column :: Int -> Gen SpreadSheetCol
column nRows = oneof [
        CInt    . CData <$> vectorOf nRows (arbitrary :: Gen Int),
        CBool   . CData <$> vectorOf nRows (arbitrary :: Gen Bool),
        CString . CData <$> vectorOf nRows (arbitrary :: Gen String)
    ]

instance Arbitrary SpreadSheet where
    arbitrary = do
        nRows <- abs <$> arbitrary :: (Gen Int)
        nCols <- abs <$> arbitrary :: (Gen Int) 
        columnNames <- vectorOf nCols (arbitrary :: (Gen String))
        columnData  <- vectorOf nCols (column nRows)
        -- entries with duplicate columnNames will be resolved
        let columns = OM.fromList (zip columnNames columnData) 
        return $ SpreadSheet nRows columns


newtype Row = Row [ColField]
    deriving Show

row :: Int -> Gen Row
row n = Row <$> vectorOf n (arbitrary :: Gen ColField)

-- currently not considering formula's
instance Arbitrary ColField where
    arbitrary = do
        oneof [FInt <$> (arbitrary :: Gen Int),
               FBool <$> (arbitrary :: Gen Bool),
               FString <$> (arbitrary :: Gen String)]

-- adding a row increases row length variable
p1 :: SpreadSheet -> Property
p1 s@(SpreadSheet n cs) = (nCols > 0) ==> forAll (row nCols) p
    where p :: Row -> Bool
          p (Row r) = let (SpreadSheet n' _) = tryAddRow s r in n' == n + 1
          nCols = length cs

-- removing a row decreases row length variable
p2 :: SpreadSheet -> Int -> Property
p2 s@(SpreadSheet n cs) i = (nCols > 0 && n > 0 && i > 0 && i < n) ==> p i
    where p :: Int -> Bool
          p i = let (SpreadSheet n' _) = Data.SpreadSheet.removeRow s i in n' == n - 1
          nCols = length cs

main :: IO ()
main = defaultMain properties

properties :: TestTree
properties = testGroup "Properties" qcProps

qcProps =
    [
        QC.testProperty "adding a row increases row length variable"
                Main.p1,
        QC.testProperty "removing a row decreases row length variable"
                Main.p2
    ]

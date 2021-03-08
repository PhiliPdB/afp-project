import Data.SpreadSheet
import Data.Column
import Test.Tasty.QuickCheck 
-- https://hackage.haskell.org/package/QuickCheck-2.14.2/docs/Test-QuickCheck-Gen.html#t:Gen


column :: Int -> Gen SpreadSheetCol
column nRows = oneof [
        CInt    . CData <$> vectorOf nRows (arbitrary :: Gen Int),
        CBool   . CData <$> vectorOf nRows (arbitrary :: Gen Bool),
        CString . CData <$> vectorOf nRows (arbitrary :: Gen String)
    ]

instance Arbitrary SpreadSheet where
    arbitrary = do
        nRows <- arbitrary :: (Gen Int)
        nCols <- arbitrary :: (Gen Int) 
        columnNames <- vectorOf nCols (arbitrary :: (Gen String))
        columnData  <- vectorOf nCols (column nRows)
        return $ SpreadSheet nRows (zip columnNames columnData)
        

main :: IO ()
main = putStrLn "Test suite not yet implemented"

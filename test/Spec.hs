import Data.SpreadSheet
import Data.Column
import Test.Tasty.QuickCheck as QC
import Test.Tasty
-- https://hackage.haskell.org/package/QuickCheck-2.14.2/docs/Test-QuickCheck-Gen.html#t:Gen
import Data.Maybe

import Data.List as L
import Unit


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
        -- entries with duplicate columnNames must be resolved
        let uniqueColumnNames = nub columnNames
        let columns = zip uniqueColumnNames columnData
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
rowAddedLengthProperty :: SpreadSheet -> Property
rowAddedLengthProperty s@(SpreadSheet n cs) = (nCols > 0) ==> forAll (row nCols) p
    where p :: Row -> Bool
          p (Row r) = let (SpreadSheet n' _) = tryAddRow s r in n' == n + 1
          nCols = L.length cs

-- removing a row decreases row length variable
rowRemovedLengthProperty :: SpreadSheet -> Int -> Property
rowRemovedLengthProperty s@(SpreadSheet n cs) i = (nCols > 0 && n > 0 && i >= 0 && i < n) ==> p i
    where p :: Int -> Bool
          p i = let (SpreadSheet n' _) = Data.SpreadSheet.removeRow s i in n' == n - 1
          nCols = L.length cs

columnInsertion :: Int -> Int -> Gen (SpreadSheetCol, Int)
columnInsertion nRows nCols = do
    c <- column nRows
    i <- chooseInt (0, nCols)
    return (c, i)

-- adding a new column preserves uniqueness property
uniqueColumnProperty :: SpreadSheet -> String -> Property
uniqueColumnProperty s@(SpreadSheet n cs) k = forAll (columnInsertion n (L.length cs)) p
    where p :: (SpreadSheetCol, Int) -> Bool
          p (c, i) = let (SpreadSheet _ cs') = tryAddColumn s i (k, c) in keys cs' == nub (keys cs')
          numKeys :: String -> [(String, SpreadSheetCol)] -> Int
          numKeys k' cs' = L.length $ filter (==k') (keys cs')
          keys :: [(String, SpreadSheetCol)] -> [String]
          keys = map fst

instance Arbitrary SpreadSheetCol where
    arbitrary = do
        nRows <- abs <$> arbitrary :: (Gen Int)
        column nRows

-- UniformInput is required otherwise quickcheck discards too many cases where the input is not uniform in length
newtype UniformInput = UI [(String, SpreadSheetCol)]
    deriving Show

instance Arbitrary UniformInput where
    arbitrary = do
        columnNames <- listOf (arbitrary :: (Gen String))
        nRows <- abs <$> arbitrary :: (Gen Int)
        columnData  <- vectorOf (L.length columnNames) (column nRows)
        return $ UI (zip columnNames columnData)

-- A spreadsheet constructed has a row length variable matching its first column length
spreadSheetRowLengthProperty :: UniformInput -> Property
spreadSheetRowLengthProperty (UI cs) =
    case spreadSheet cs of
        Just (SpreadSheet _n [])    -> property True
        Just (SpreadSheet  n (c:_)) -> property $ L.length c == n
        Nothing                     -> property Discard -- discard if incorrect input

-- A spreadsheet constructed has only unique columnNames
spreadSheetUniqueColumnNamesProperty :: UniformInput -> Property
spreadSheetUniqueColumnNamesProperty (UI cs) =
    case spreadSheet cs of
        Just (SpreadSheet  0 [])  -> property True
        Just (SpreadSheet _n cs') -> property $ nub (names cs') == names cs'
        Nothing                   -> property Discard -- discard if incorrect input
    where names = map fst

main :: IO ()
main = defaultMain (testGroup "Tests" [properties, unitTests])

properties :: TestTree
properties = testGroup "Properties" qcProps

qcProps :: [TestTree]
qcProps =
    [
        QC.testProperty "adding a row increases row length variable"
                Main.rowAddedLengthProperty,
        QC.testProperty "removing a row decreases row length variable"
                Main.rowRemovedLengthProperty,
        QC.testProperty "adding a new column preserves uniqueness property"
                Main.uniqueColumnProperty,
        QC.testProperty "A spreadsheet constructed has a row length variable matching its first column length"
                Main.spreadSheetRowLengthProperty,
        QC.testProperty "A spreadsheet constructed has only unique columnNames"
                Main.spreadSheetUniqueColumnNamesProperty
    ]

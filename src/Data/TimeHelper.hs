module Data.TimeHelper (showDuration, showPeriod, readPeriod, readDuration, invDur, invPer) where
    
import Data.Hourglass
import Data.Char  (digitToInt, isDigit)
import Data.Maybe (isJust, fromJust)
import Data.Int

-- Inverts the duration for use in TimeSub
invDur :: Duration -> Duration
invDur (Duration h m s ns) = Duration (-h) (-m) (-s) (-ns)

-- Inverts the period for use in SubPeriod
invPer :: Period -> Period
invPer (Period y m d) = Period (-y) (-m) (-d)

-- Special duration parser for import use, string needs to start with a "D "
readDuration :: String -> Maybe Duration
readDuration ('D':' ':dur) = case parseDur64 dur of
    [h,m,s] -> Just $ Duration (Hours h) (Minutes m) (Seconds s) (NanoSeconds 0)
    _       -> Nothing
    where
        parseDur64 :: String -> [Int64]
        parseDur64 d | isJust (correctForm d) = map fromIntegral $ fromJust $ correctForm d
                     | otherwise              = []
        correctForm d = sequenceA (parseDurPer (Just 0) d)

readDuration _              = Nothing

-- Special period parser for import use, string needs to start with a "P ".
-- Separators are set to : via parseDurPer (instead of - or .)
readPeriod :: String -> Maybe Period
readPeriod ('P':' ':per) = case parseDurPer (Just 0) per of
    [Just y, Just m, Just d] -> Just $ Period y m d
    _                        -> Nothing
readPeriod _             = Nothing

-- Helper for Period and Duration parsing
parseDurPer :: Maybe Int -> String -> [Maybe Int]
parseDurPer Nothing _  = [Nothing]
parseDurPer a []       = [a]
parseDurPer a (':':xs) = a : parseDurPer (Just 0) xs
parseDurPer a ( x :xs) | isDigit x = parseDurPer ((+ digitToInt x) . (* 10) <$> a) xs
                       | otherwise = parseDurPer Nothing                           xs 

-- Special Duration show for export use
showDuration :: Duration -> String
showDuration dur = "D " ++ h ++ ":" ++ m ++ ":" ++ s
    where
        h  = init $ show $ durationHours   dur
        m  = init $ show $ durationMinutes dur
        s  = init $ show $ durationSeconds dur

-- Special Period show for export use
showPeriod :: Period -> String
showPeriod per = "P " ++ y ++ ":" ++ m ++ ":" ++ d
    where
        y  = show $ periodYears  per
        m  = show $ periodMonths per
        d  = show $ periodDays   per 
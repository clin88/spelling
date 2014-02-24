import Control.Applicative ((<$>))
import Control.Monad (ap, liftM)
import Data.Maybe (fromJust, fromMaybe)
import Data.Map as Map (foldrWithKey, filterWithKey, Map)
import Data.Set (member)
import Edits
import Rehelper

maxKey :: WordCount -> Maybe String
maxKey dict = case foldrWithKey f ("", 0) dict of
            (_, 0) -> Nothing
            (s, _) -> Just s
    where
        f :: String -> Int -> (String, Int) -> (String, Int)
        f k v acc@(maxk, maxv)
            | v > maxv = (k, v)
            | otherwise = acc

correct :: String -> String
correct s = fromMaybe s $ takeFirstJust matches
    where
        takeFirstJust (Just x:xs) = Just x
        takeFirstJust (Nothing:xs) = takeFirstJust xs
        takeFirstJust [] = Nothing
        matches = correct' s (fromJust $ countWords "abc def abc hello") <$> [0, 1, 2]

correct' :: String -> WordCount -> Int -> Maybe String
correct' input dict n = maxKey knownDict
    where
        knownDict = filterWithKey (\k _ -> k `member` es) dict
        es = edits input n

loadWords :: IO String

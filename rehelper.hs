module Rehelper
(countWords
, WordCount) where

import Data.Map as M (Map, foldrWithKey, filterWithKey, empty, insertWith)
import Text.Regex
import Data.Char (toLower)

type ReResults = Maybe (String, String, String, [String])
reListMatches :: Regex -> String -> Maybe [String]
reListMatches re text = reListMatches' [] $ matchRegexAll re text
    where
        reListMatches' :: [String] -> ReResults -> Maybe [String]
        reListMatches' [] Nothing = Nothing
        reListMatches' acc Nothing = Just acc
        reListMatches' acc (Just (_, match, more, _)) = reListMatches' (match:acc) $ matchRegexAll re more

type WordCount = M.Map String Int
--countWords :: String -> Maybe WordCount
countWords text = return text >>= getWords >>= countWords'
    where
        getWords :: String -> Maybe [String]
        getWords text = reListMatches regex lowertext
            where
                lowertext = map toLower text
                regex = mkRegex "[a-z]+"

        countWords' :: [String] -> Maybe WordCount
        countWords' words = Just $ foldr f M.empty words
            where
                f word hmap = M.insertWith (+) word 1 hmap

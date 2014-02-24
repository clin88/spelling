module Edits (edits) where

import Control.Applicative ((<*>))
import qualified Data.Set as Set
import Data.List (unfoldr)
import Data.Sequence ((><), (|>), (<|))
import qualified Data.Sequence as Seq
import Data.Foldable (toList)

type SeqString = Seq.Seq Char

--testedit = edit $ Set.fromList [Seq.fromList "abc", Seq.fromList "def"]

edits :: String -> Int -> Set.Set String
edits s 0 = Set.singleton s
edits s n = Set.map toList $ edits' s' !! (n-1)
    where
        s' = Seq.fromList s

        edits' :: SeqString -> [Set.Set SeqString]
        edits' s = unfoldr f $ [s]

        edit :: [SeqString] -> [SeqString]
        edit xs =
            let ops = [deleteAt, transposeAt, insertAt, replaceAt] in
                concat $ ops <*> xs

        f :: [SeqString] -> Maybe (Set.Set SeqString, [SeqString])
        f xs = let val = edit xs in Just (Set.fromList $ val, val)

deleteAt :: SeqString -> [SeqString]
deleteAt s = do
    n <- [0..Seq.length s - 1]
    return $ Seq.take n s >< Seq.drop (n+1) s

transposeAt :: SeqString -> [SeqString]
transposeAt s = do
    n <- [0..Seq.length s - 2]
    let a = Seq.take n s |> Seq.index s (n+1)
        b = Seq.index s n <| Seq.drop (n+2) s
    return $ a >< b

replaceAt :: SeqString -> [SeqString]
replaceAt s = do
    n <- [0..Seq.length s - 1]
    c <- ['a'..'z']
    return $ Seq.update n c s

insertAt :: SeqString -> [SeqString]
insertAt s = do
    n <- [0..Seq.length s]
    c <- ['a'..'z']
    let (a, b) = Seq.splitAt n s
    return $ a >< c <| b



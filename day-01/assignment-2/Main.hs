{- You notice that the device repeats the same frequency change list over and
 - over. To calibrate the device, you need to find the first frequency it
 - reaches twice.
 -
 - For example, using the same list of changes above, the device would loop as
 - follows:
 -
 - Current frequency  0, change of +1; resulting frequency  1.
 - Current frequency  1, change of -2; resulting frequency -1.
 - Current frequency -1, change of +3; resulting frequency  2.
 - Current frequency  2, change of +1; resulting frequency  3.
 - (At this point, the device continues from the start of the list.)
 - Current frequency  3, change of +1; resulting frequency  4.
 - Current frequency  4, change of -2; resulting frequency  2, which has already
 - been seen.
 -
 - In this example, the first frequency reached twice is 2. Note that your
 - device might need to repeat its list of frequency changes many times before a
 - duplicate frequency is found, and that duplicates might be found while in the
 - middle of processing the list.
 -
 - Here are other examples:
 -
 - +1, -1 first reaches 0 twice.
 - +3, +3, +4, -2, -4 first reaches 10 twice.
 - -6, +3, +8, +5, -6 first reaches 5 twice.
 - +7, +7, -2, -7, -4 first reaches 14 twice.
 -
 - What is the first frequency your device reaches twice?
 -}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import AdventOfCode
import ClassyPrelude
import qualified Data.List as L
import qualified Data.Set as Set
import qualified System.Exit as Sys
import qualified Text.Parsec as P
import qualified Text.Parsec.Number as P

main :: IO ()
main = defaultMain parseInput handleInput

handleInput :: (Num a, Ord a, Show a, MonadIO m) => [a] -> m ()
handleInput numbers = case findFirstDuplicate frequencies of
    Nothing -> liftIO $ Sys.die "Could not find any duplicates"
    Just duplicate -> print duplicate
  where
    frequencies = L.scanl (+) 0 (L.cycle numbers)

findFirstDuplicate :: Ord a => [a] -> Maybe a
findFirstDuplicate = findFirstDuplicateWith Set.empty
  where
    findFirstDuplicateWith seen (x:xs) = if x `Set.member` seen
        then Just x
        else findFirstDuplicateWith (Set.insert x seen) xs
    findFirstDuplicateWith _ _ = Nothing

parseInput :: LText -> Either P.ParseError [Int]
parseInput = P.parse (parseLines <* P.eof) ""

parseLines :: P.Parsec LText () [Int]
parseLines = P.many parseLine

parseLine :: P.Parsec LText () Int
parseLine = P.int <* P.char '\n'

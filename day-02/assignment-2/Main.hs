{- Confident that your list of box IDs is complete, you're ready to find the
 - boxes full of prototype fabric.
 -
 - The boxes will have IDs which differ by exactly one character at the same
 - position in both strings. For example, given the following box IDs:
 -
 - abcde
 - fghij
 - klmno
 - pqrst
 - fguij
 - axcye
 - wvxyz
 -
 - The IDs abcde and axcye are close, but they differ by two characters (the
 - second and fourth). However, the IDs fghij and fguij differ by exactly one
 - character, the third (h and u). Those must be the correct boxes.
 -
 - What letters are common between the two correct box IDs? (In the example
 - above, this is found by removing the differing character from either ID,
 - producing fgij.)
 -}
{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import AdventOfCode
import ClassyPrelude

main :: IO ()
main = defaultMain parseInput handleInput

parseInput :: LText -> Either LText [LText]
parseInput = Right <$> lines

handleInput :: MonadIO m => [LText] -> m ()
handleInput lines = mapM_ showSame distance1
  where
    distance1 = findDistance1 (map unpack lines)

    showSame :: (Show a, Eq a, MonadIO m) => ([a], [a]) -> m ()
    showSame (x, y) = print $ map fst $ filter (uncurry (==)) $ zip x y

findDistance1 :: Eq a => [[a]] -> [([a], [a])]
findDistance1 [] = []
findDistance1 (x:xs) = distance1 ++ findDistance1 xs
  where
    distances = map (countNotCommon x) xs
    distanceMap = zip xs distances
    distance1
        = map ((\y -> (x, y)) . fst)
        . filter ((== 1) . snd)
        $ distanceMap

    countNotCommon :: Eq a => [a] -> [a] -> Int
    countNotCommon x1 x2 = length $ filter not $ zipWith (==) x1 x2

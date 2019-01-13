{- --- Part Two ---
 -
 - As it turns out, you got the Elves' plan backwards. They actually want to
 - know how many recipes appear on the scoreboard to the left of the first
 - recipes whose scores are the digits from your puzzle input.
 -
 - 51589 first appears after 9 recipes.
 - 01245 first appears after 5 recipes.
 - 92510 first appears after 18 recipes.
 - 59414 first appears after 2018 recipes.
 -
 - How many recipes appear on the scoreboard to the left of the score sequence
 - in your puzzle input?
 -}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns      #-}
module Main where

import AdventOfCode
import qualified Data.Sequence as S
import qualified Text.Parsec as P
import qualified Text.Parsec.Number as P
import qualified Data.List as L
import qualified Data.Char as C
import qualified Data.Text.Lazy as T

main :: IO ()
main = defaultMain parseInput handleInput

handleInput :: Int -> IO ()
handleInput n = print . subListIndex (digits n) $ recipes

subListIndex :: Eq a => [a] -> [a] -> Int
subListIndex prefix
    = length 
    . takeWhile (not . (prefix `L.isPrefixOf`))
    . L.tails

recipes :: [Int]
recipes = 3:7:computeNext 0 1 (S.fromList [3, 7])

computeNext :: Int -> Int -> S.Seq Int -> [Int]
computeNext !pos1 !pos2 !current = newValues ++ computeNext pos1' pos2' current'
  where
    val1 = current `S.index` pos1
    val2 = current `S.index` pos2
    newValues = digits (val1 + val2)
    current' = L.foldl (S.|>) current newValues
    pos1' = (pos1 + val1 + 1) `mod` length current'
    pos2' = (pos2 + val2 + 1) `mod` length current'

digits :: Int -> [Int]
digits = map C.digitToInt . show

parseInput :: T.Text -> Either P.ParseError Int
parseInput = P.parse (P.int <* P.char '\n' <* P.eof) ""

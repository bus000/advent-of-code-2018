{- --- Part Two ---
 -
 - Time to improve the polymer.
 -
 - One of the unit types is causing problems; it's preventing the polymer from
 - collapsing as much as it should. Your goal is to figure out which unit type
 - is causing the most problems, remove all instances of it (regardless of
 - polarity), fully react the remaining polymer, and measure its length.
 -
 - For example, again using the polymer dabAcCaCBAcCcaDA from above:
 -
 - * Removing all A/a units produces dbcCCBcCcD. Fully reacting this polymer
 -   produces dbCBcD, which has length 6.
 - * Removing all B/b units produces daAcCaCAcCcaDA. Fully reacting this polymer
 -   produces daCAcaDA, which has length 8.
 - * Removing all C/c units produces dabAaBAaDA. Fully reacting this polymer
 -   produces daDA, which has length 4.
 - * Removing all D/d units produces abAcCaCBAcCcaA. Fully reacting this polymer
 -   produces abCBAc, which has length 6.
 -
 - In this example, removing all C/c units was best, producing the answer 4.
 -
 - What is the length of the shortest polymer you can produce by removing all
 - units of exactly one type and fully reacting the result?
 -}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import AdventOfCode
import ClassyPrelude
import qualified Data.Char as Char
import qualified Data.Text.Lazy as T
import qualified Data.List as L

main :: IO ()
main = defaultMain parseInput handleInput

handleInput :: MonadIO m => String -> m ()
handleInput polimer = print minimumPolimerLength
  where
    minimumPolimerLength
        = L.minimum
        . map length
        . map (react shouldReact)
        . map (\x -> filter (not . (\y -> x == y) . Char.toLower) polimer)
        $ ['a'..'z']

shouldReact :: Char -> Char -> Bool
shouldReact c1 c2 = c1 /= c2 && Char.toLower c1 == Char.toLower c2

react :: (a -> a -> Bool) -> [a] -> [a]
react p = foldr removeP []
  where
    removeP x1 (x2:xs) = if p x1 x2 then xs else x1:x2:xs
    removeP x1 [] = [x1]

parseInput :: LText -> Either LText String
parseInput = Right . unpack . T.strip

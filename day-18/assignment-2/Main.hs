{- This important natural resource will need to last for at least thousands of
 - years. Are the Elves collecting this lumber sustainably?
 -
 - What will the total resource value of the lumber collection area be after
 - 1000000000 minutes?
 -}
{-# language BangPatterns #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Main (main) where

import AdventOfCode
import Control.DeepSeq (($!!), NFData)
import qualified Data.Array as A
import qualified Data.Map as Map
import qualified Data.Text.Lazy as T
import GHC.Generics (Generic)
import qualified Text.Parsec as P

main :: IO ()
main = defaultMain parseInput handleInput

data AcreType = Forest | Lumberyard | Open deriving (A.Ix, Eq, Ord, Show, Generic, NFData)

data Land = Land !(A.Array (Int, Int) AcreType) deriving (Show, Ord, Eq)

handleInput :: Land -> IO ()
handleInput = print . evaluateLand . simulate 1000000000

simulate :: Int -> Land -> Land
simulate = simulate' Map.empty

simulate' :: Map.Map Land Int -> Int -> Land -> Land
simulate' _ 0 !land = land
simulate' !previous !steps !land = case previous Map.!? land' of
    Just prevStep ->
        let period = prevStep - steps
            left = steps `mod` period
        in simulate' Map.empty (left - 1) land'
    Nothing -> simulate' previous' (steps - 1) land'
  where
    land' = step land
    previous' = Map.insert land' steps previous

evaluateLand :: Land -> Int
evaluateLand (Land xs) = trees * lumberyards
  where
    elements = A.elems xs
    trees = length . filter (== Forest) $ elements
    lumberyards = length . filter (== Lumberyard) $ elements

step :: Land -> Land
step (Land !array) = Land $!! mapNeighbourhood array mapTile

mapTile :: AcreType -> [AcreType] -> AcreType
mapTile acreType surrounding
    | acreType == Open && trees >= 3 = Forest
    | acreType == Open = Open
    | acreType == Forest && lumberyards >= 3 = Lumberyard
    | acreType == Forest = Forest
    | acreType == Lumberyard && lumberyards >= 1 && trees >= 1 = Lumberyard
    | acreType == Lumberyard = Open
    | otherwise = error "This should not be possible."
  where
    trees = length . filter (== Forest) $ surrounding
    lumberyards = length . filter (== Lumberyard) $ surrounding

mapNeighbourhood :: A.Array (Int, Int) a -> (a -> [a] -> b) -> A.Array (Int, Int) b
mapNeighbourhood xs f = A.listArray bounds (map f' indices)
  where
    f' index = f (xs A.! index) (neighbours xs index)
    bounds = A.bounds xs
    indices = A.range bounds

neighbours :: A.Array (Int, Int) a -> (Int, Int) -> [a]
neighbours xs (x, y) = map (xs A.!) validNeighbours
  where
    bounds = A.bounds xs
    validNeighbours = filter (A.inRange bounds)
        [ (x - 1, y - 1), (x, y - 1), (x + 1, y - 1)
        , (x - 1, y),                 (x + 1, y)
        , (x - 1, y + 1), (x, y + 1), (x + 1, y + 1)
        ]

parseInput :: T.Text -> Either P.ParseError Land
parseInput = P.parse (parseLand <* P.eof) ""

parseLand :: P.Parsec T.Text () Land
parseLand = do
    rows <- parseRow `P.endBy` P.char '\n'

    let rowN = length rows
        colN = maybe 0 length . headMay $ rows
        bounds = ((0, 0), (colN - 1, rowN - 1))
        elements = concat rows
        array = A.listArray bounds elements

    return $! Land array

parseRow :: P.Parsec T.Text () [AcreType]
parseRow = P.many parseTile

parseTile :: P.Parsec T.Text () AcreType
parseTile = do
    c <- P.satisfy $ \x -> x `elem` validChar

    case c of
        '.' -> return Open
        '#' -> return Lumberyard
        '|' -> return Forest
        _ -> error "Should not be possible."
  where
    validChar = ".#|"

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (x:_) = Just x

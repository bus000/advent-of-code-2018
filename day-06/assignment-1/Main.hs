{- --- Day 6: Chronal Coordinates ---
 -
 - The device on your wrist beeps several times, and once again you feel like
 - you're falling.
 -
 - "Situation critical," the device announces. "Destination indeterminate.
 - Chronal interference detected. Please specify new target coordinates."
 -
 - The device then produces a list of coordinates (your puzzle input). Are they
 - places it thinks are safe or dangerous? It recommends you check manual page
 - 729. The Elves did not give you a manual.
 -
 - If they're dangerous, maybe you can minimize the danger by finding the
 - coordinate that gives the largest distance from the other points.
 -
 - Using only the Manhattan distance, determine the area around each coordinate
 - by counting the number of integer X,Y locations that are closest to that
 - coordinate (and aren't tied in distance to any other coordinate).
 -
 - Your goal is to find the size of the largest area that isn't infinite. For
 - example, consider the following list of coordinates:
 -
 - 1, 1
 - 1, 6
 - 8, 3
 - 3, 4
 - 5, 5
 - 8, 9
 -
 - If we name these coordinates A through F, we can draw them on a grid, putting
 - 0,0 at the top left:
 -
 - ..........
 - .A........
 - ..........
 - ........C.
 - ...D......
 - .....E....
 - .B........
 - ..........
 - ..........
 - ........F.
 -
 - This view is partial - the actual grid extends infinitely in all directions.
 - Using the Manhattan distance, each location's closest coordinate can be
 - determined, shown here in lowercase:
 -
 - aaaaa.cccc
 - aAaaa.cccc
 - aaaddecccc
 - aadddeccCc
 - ..dDdeeccc
 - bb.deEeecc
 - bBb.eeee..
 - bbb.eeefff
 - bbb.eeffff
 - bbb.ffffFf
 -
 - Locations shown as . are equally far from two or more coordinates, and so
 - they don't count as being closest to any.
 -
 - In this example, the areas of coordinates A, B, C, and F are infinite - while
 - not shown here, their areas extend forever outside the visible grid. However,
 - the areas of coordinates D and E are finite: D is closest to 9 locations,
 - and E is closest to 17 (both including the coordinate's location itself).
 - Therefore, in this example, the size of the largest area is 17.
 -
 - What is the size of the largest area that isn't infinite?
 -}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Main where

import AdventOfCode
import ClassyPrelude hiding (first)
import qualified Data.Counter as C
import qualified Data.Set as Set
import qualified Text.Parsec as P
import qualified Text.Parsec.Number as P

main :: IO ()
main = defaultMain parseInput handleInput

data CandidatePoint = CandidatePoint
    { _position     :: Point
    , _hasCorner    :: Bool
    , _closestCount :: Int
    }

data Point = Point
    { _x :: Int
    , _y :: Int
    }
  deriving (Eq, Ord, Show)

data MapDimensions = MapDimensions
    { _upperLeft  :: Point
    , _lowerRight :: Point
    }
  deriving (Show, Eq)

handleInput :: MonadIO m => NonNull [Point] -> m ()
handleInput = print . findLargestAreaSize

findLargestAreaSize :: NonNull [Point] -> Int
findLargestAreaSize candidates = snd largestArea
  where
    mapDimensions = minimumMapDimensions candidates
    positions = pointsInMap mapDimensions
    closest = map closestCandidateTo positions
    closestSingles = map head . filter (null . tail) $ closest
    cornerValues
        = Set.fromList
        . concatMap (toNullable . snd)
        . filter (corner mapDimensions . fst)
        . filter (null . tail . snd)
        . zip positions
        $ closest
    withoutCorners = filter (not . (flip Set.member cornerValues)) closestSingles
    largestArea = C.maximum . C.fromList . impureNonNull $ withoutCorners

    closestCandidateTo pos = collectMinimums (manhattanDistance pos) candidates

collectMinimums :: Ord b => (a -> b) -> NonNull [a] -> NonNull [a]
collectMinimums f xs = fst . foldr collect startState $ rest
  where
    first = head xs
    rest = tail xs
    startState = (impureNonNull [first], f first)
    collect candidate (minElements, minValue)
        | f candidate < minValue = (impureNonNull [candidate], f candidate)
        | f candidate == minValue = (candidate <| minElements, minValue)
        | otherwise = (minElements, minValue)

minimumMapDimensions :: NonNull [Point] -> MapDimensions
minimumMapDimensions points = MapDimensions (Point ulx uly) (Point lrx lry)
  where
    ulx = minimum xs
    uly = maximum ys
    lrx = maximum xs
    lry = minimum ys
    xs = mapNonNull _x points
    ys = mapNonNull _y points

pointsInMap :: MapDimensions -> [Point]
pointsInMap dimensions = Point <$> [ulx..lrx] <*> [lry..uly]
  where
    Point ulx uly = _upperLeft dimensions
    Point lrx lry = _lowerRight dimensions

corner :: MapDimensions -> Point -> Bool
corner dimensions (Point x y) = x == ulx || x == lrx || y == uly || y == lry
  where
    Point ulx uly = _upperLeft dimensions
    Point lrx lry = _lowerRight dimensions

manhattanDistance :: Point -> Point -> Int
manhattanDistance (Point x1 y1) (Point x2 y2) = abs (x1 - x2) + abs (y1 - y2)

origin :: Point
origin = Point 0 0

parseInput :: LText -> Either P.ParseError (NonNull [Point])
parseInput = P.parse (parsePoints <* P.eof) ""

parsePoints :: P.Parsec LText () (NonNull [Point])
parsePoints = impureNonNull <$> P.endBy1 parsePoint (P.char '\n')

parsePoint :: P.Parsec LText () Point
parsePoint = Point <$> P.int <* P.string ", " <*> P.int

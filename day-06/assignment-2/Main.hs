{- On the other hand, if the coordinates are safe, maybe the best you can do is
 - try to find a region near as many coordinates as possible.
 -
 - For example, suppose you want the sum of the Manhattan distance to all of the
 - coordinates to be less than 32. For each location, add up the distances to
 - all of the given coordinates; if the total of those distances is less than
 - 32, that location is within the desired region. Using the same coordinates as
 - above, the resulting region looks like this:
 -
 - ..........
 - .A........
 - ..........
 - ...###..C.
 - ..#D###...
 - ..###E#...
 - .B.###....
 - ..........
 - ..........
 - ........F.
 -
 - In particular, consider the highlighted location 4,3 located at the top
 - middle of the region. Its calculation is as follows, where abs() is the
 - absolute value function:
 -
 - * Distance to coordinate A: abs(4-1) + abs(3-1) =  5
 - * Distance to coordinate B: abs(4-1) + abs(3-6) =  6
 - * Distance to coordinate C: abs(4-8) + abs(3-3) =  4
 - * Distance to coordinate D: abs(4-3) + abs(3-4) =  2
 - * Distance to coordinate E: abs(4-5) + abs(3-5) =  3
 - * Distance to coordinate F: abs(4-8) + abs(3-9) = 10
 - * Total distance: 5 + 6 + 4 + 2 + 3 + 10 = 30
 -
 - Because the total distance to all coordinates (30) is less than 32, the
 - location is within the region.
 -
 - This region, which also includes coordinates D and E, has a total size of 16.
 -
 - Your actual region will need to be much larger than this example, though,
 - instead including all locations with a total distance of less than 10000.
 -
 - What is the size of the region containing all locations which have a total
 - distance to all given coordinates of less than 10000?
 -}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import AdventOfCode
import ClassyPrelude
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
handleInput = print . distanceLessThan 10000

distanceLessThan :: Int -> NonNull [Point] -> Int
distanceLessThan dist points = length . filter totalDist $ positions
  where
    mapDimensions = extendMap dist . minimumMapDimensions $ points
    positions = pointsInMap mapDimensions
    totalDist pos = (< dist) . sum . map (manhattanDistance pos) . toNullable $ points

minimumMapDimensions :: NonNull [Point] -> MapDimensions
minimumMapDimensions points = MapDimensions (Point ulx uly) (Point lrx lry)
  where
    ulx = minimum xs
    uly = maximum ys
    lrx = maximum xs
    lry = minimum ys
    xs = mapNonNull _x points
    ys = mapNonNull _y points

extendMap :: Int -> MapDimensions -> MapDimensions
extendMap n (MapDimensions (Point ulx uly) (Point lrx lry)) =
    MapDimensions (Point ulx' uly') (Point lrx' lry')
  where
    ulx' = ulx - n
    uly' = uly + n
    lrx' = lrx + n
    lry' = lry - n

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

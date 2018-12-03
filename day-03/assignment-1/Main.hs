{- --- Day 3: No Matter How You Slice It ---
 -
 - The Elves managed to locate the chimney-squeeze prototype fabric for Santa's
 - suit (thanks to someone who helpfully wrote its box IDs on the wall of the
 - warehouse in the middle of the night). Unfortunately, anomalies are still
 - affecting them - nobody can even agree on how to cut the fabric.
 -
 - The whole piece of fabric they're working on is a very large square - at
 - least 1000 inches on each side.
 -
 - Each Elf has made a claim about which area of fabric would be ideal for
 - Santa's suit. All claims have an ID and consist of a single rectangle with
 - edges parallel to the edges of the fabric. Each claim's rectangle is defined
 - as follows:
 -
 - * The number of inches between the left edge of the fabric and the left edge
 -   of the rectangle.
 - * The number of inches between the top edge of the fabric and the top edge of
 -   the rectangle.
 - * The width of the rectangle in inches.
 - * The height of the rectangle in inches.
 -
 - A claim like #123 @ 3,2: 5x4 means that claim ID 123 specifies a rectangle 3
 - inches from the left edge, 2 inches from the top edge, 5 inches wide, and 4
 - inches tall. Visually, it claims the square inches of fabric represented by
 - # (and ignores the square inches of fabric represented by .) in the diagram
 - below:
 -
 - ...........
 - ...........
 - ...#####...
 - ...#####...
 - ...#####...
 - ...#####...
 - ...........
 - ...........
 - ...........
 -
 - The problem is that many of the claims overlap, causing two or more claims to
 - cover part of the same areas. For example, consider the following claims:
 -
 - #1 @ 1,3: 4x4
 - #2 @ 3,1: 4x4
 - #3 @ 5,5: 2x2
 -
 - Visually, these claim the following areas:
 -
 - ........
 - ...2222.
 - ...2222.
 - .11XX22.
 - .11XX22.
 - .111133.
 - .111133.
 - ........
 -
 - The four square inches marked with X are claimed by both 1 and 2. (Claim 3,
 - while adjacent to the others, does not overlap either of them.)
 -
 - If the Elves all proceed with their own plans, none of them will have enough
 - fabric. How many square inches of fabric are within two or more claims?
 -}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import AdventOfCode
import ClassyPrelude
import qualified Text.Parsec as P
import qualified Text.Parsec.Number as P

main :: IO ()
main = defaultMain parseInput handleInput

handleInput :: MonadIO m => Claims -> m ()
handleInput claims = print . length . filter overlapping $ positions
  where
    areas = map _claim claims
    positions = positionsInArea $ boundingArea areas
    overlapping position = not . null . drop 1 . filter (position `inArea`) $ areas

data Position = Position
    { _x :: Int
    , _y :: Int
    } deriving (Show)

type Positions = [Position]

data Area = Area
    { _topLeft :: Position
    , _width   :: Int
    , _heigth  :: Int
    } deriving (Show)

type Areas = [Area]

data Claim = Claim
    { _claimId :: Text
    , _claim   :: Area
    } deriving (Show)

type Claims = [Claim]

boundingArea :: Areas -> Area
boundingArea areas = Area (Position 0 0) (foldr max 0 xs) (foldr max 0 ys)
  where
    xs = map _x lowerRights
    ys = map _y lowerRights
    lowerRights = map lowerRight areas
    lowerRight (Area (Position x y) width height) =
        Position (x + width) (y + height)

inArea :: Position -> Area -> Bool
inArea (Position x' y') (Area (Position x y) width height) =
    x' >= x && x' < x + width && y' >= y && y' < y + height

positionsInArea :: Area -> [Position]
positionsInArea (Area (Position x y) width height) = Position <$> xs <*> ys
  where
    xs = [x..x+width]
    ys = [y..y+height]

parseInput :: LText -> Either P.ParseError Claims
parseInput = P.parse (parseClaims <* P.eof) ""

parseClaims :: P.Parsec LText () Claims
parseClaims = P.many parseClaim

parseClaim :: P.Parsec LText () Claim
parseClaim = Claim <$> parseClaimId <* P.string "@ "
    <*> parseArea <* P.char '\n'

parseClaimId :: P.Parsec LText () Text
parseClaimId = pack <$> P.manyTill P.anyChar (P.char ' ')

parseArea :: P.Parsec LText () Area
parseArea = Area <$> (parsePosition <* P.string ": ")
    <*> (P.int <* P.char 'x') <*> P.int

parsePosition :: P.Parsec LText () Position
parsePosition = Position <$> P.int <* P.char ',' <*> P.int

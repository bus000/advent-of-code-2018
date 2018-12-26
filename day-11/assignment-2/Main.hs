{- --- Part Two ---
 -
 - You discover a dial on the side of the device; it seems to let you select a
 - square of any size, not just 3x3. Sizes from 1x1 to 300x300 are supported.
 -
 - Realizing this, you now must find the square of any size with the largest
 - total power. Identify this square by including its size as a third parameter
 - after the top-left coordinate: a 9x9 square with a top-left corner of 3,5 is
 - identified as 3,5,9.
 -
 - For example:
 -
 - * For grid serial number 18, the largest total square (with a total power of
 -   113) is 16x16 and has a top-left corner of 90,269, so its identifier is
 -   90,269,16.
 - * For grid serial number 42, the largest total square (with a total power of
 -   119) is 12x12 and has a top-left corner of 232,251, so its identifier is
 -   232,251,12.
 -
 - What is the X,Y,size identifier of the square with the largest total power?
 -}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveFunctor     #-}
module Main where

import AdventOfCode
import ClassyPrelude
import qualified Text.Parsec as P
import qualified Text.Parsec.Number as P
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Semigroup as S

main :: IO ()
main = defaultMain parseInput handleInput

handleInput :: (MonadIO m, Integral a, Show a) => Grid a -> m ()
handleInput grid
    = print
    . (\(Square size (Position (x, y)), _) -> (x, y, size))
    . maximumByEx (comparing snd)
    . map (bestPosition grid)
    $ [1..20]

bestPosition :: (Integral a, Show a) => Grid a -> a -> (Square a, a)
bestPosition grid n
    = maximumByEx (comparing snd)
    . map theSum
    . slide (S.Sum . gridValue grid)
    $ SlidingWindow n
  where
    theSum (square, s) = (square, S.getSum s)

newtype Position a = Position (a, a) deriving (Show, Eq)

data Grid a = Grid
    { _serialNumber :: a
    }
  deriving (Show)

data SlidingWindow a = SlidingWindow
    { _size :: a
    }
  deriving (Show, Functor)

data Square a = Square
    { _squareSize :: a
    , _position   :: Position a
    }
  deriving (Show, Eq)

gridValue :: Integral a => Grid a -> Position a -> a
gridValue (Grid serial) (Position (x, y)) = hundreds - 5
  where
    rackId = x + 10
    powerLevel = ((rackId * y) + serial) * rackId
    hundreds = powerLevel `div` 100 `mod` 10

squarePositions :: Integral a => Square a -> [Position a]
squarePositions (Square size (Position (x, y))) =
    map Position . (,) <$> [x..x+size-1] <*> [y..y+size-1]

squareValue :: (Integral a, Semigroup b) => Square a -> (Position a -> b) -> b
squareValue square f = sconcat . map f $ positions
  where
    positions = p :| ps
    (p:ps) = squarePositions square

slide
    :: (Integral a, Semigroup b)
    => (Position a -> b)
    -> SlidingWindow a
    -> [(Square a, b)]
slide f (SlidingWindow size) = map (\x -> (x, squareValue x f)) squares
  where
    squares
        = map (Square size . Position)
        $ (,) <$> [1..300-size+1] <*> [1..300-size+1]

parseInput :: LText -> Either P.ParseError (Grid Int)
parseInput = P.parse (parseGrid <* P.eof) ""

parseGrid :: P.Parsec LText () (Grid Int)
parseGrid = Grid <$> P.int <* P.char '\n'

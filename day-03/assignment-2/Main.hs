{- Amidst the chaos, you notice that exactly one claim doesn't overlap by even
 - a single square inch of fabric with any other claim. If you can somehow draw
 - attention to it, maybe the Elves will be able to make Santa's suit after all!

 - For example, in the claims above, only claim 3 is intact after all claims are
 - made.

 - What is the ID of the only claim that doesn't overlap?
 -}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import AdventOfCode
import ClassyPrelude
import qualified Text.Parsec as P
import qualified Text.Parsec.Number as P
import qualified Data.Set as Set

main :: IO ()
main = defaultMain parseInput handleInput

handleInput :: MonadIO m => Claims -> m ()
handleInput claims
    = mapM_ (putStrLn . _claimId)
    . filter (all (`Set.member` notOverlapping) . positionsInArea . _claim)
    $ claims
  where
    areas = map _claim claims
    positions = positionsInArea $ boundingArea areas
    notOverlapping = Set.fromList $ filter isNotOverlapping positions
    isNotOverlapping position = null . drop 1 . filter (position `inArea`) $ areas

data Position = Position
    { _x :: Int
    , _y :: Int
    } deriving (Show, Ord, Eq)

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

{- --- Day 10: The Stars Align ---
 -
 - It's no use; your navigation system simply isn't capable of providing walking
 - directions in the arctic circle, and certainly not in 1018.
 -
 - The Elves suggest an alternative. In times like these, North Pole rescue
 - operations will arrange points of light in the sky to guide missing Elves
 - back to base. Unfortunately, the message is easy to miss: the points move
 - slowly enough that it takes hours to align them, but have so much momentum
 - that they only stay aligned for a second. If you blink at the wrong time, it
 - might be hours before another message appears.
 -
 - You can see these points of light floating in the distance, and record their
 - position in the sky and their velocity, the relative change in position
 - per second (your puzzle input). The coordinates are all given from your
 - perspective; given enough time, those positions and velocities will move the
 - points into a cohesive message!
 -
 - Rather than wait, you decide to fast-forward the process and calculate what
 - the points will eventually spell.
 -
 - For example, suppose you note the following points:
 -
 - position=< 9,  1> velocity=< 0,  2>
 - position=< 7,  0> velocity=<-1,  0>
 - position=< 3, -2> velocity=<-1,  1>
 - position=< 6, 10> velocity=<-2, -1>
 - position=< 2, -4> velocity=< 2,  2>
 - position=<-6, 10> velocity=< 2, -2>
 - position=< 1,  8> velocity=< 1, -1>
 - position=< 1,  7> velocity=< 1,  0>
 - position=<-3, 11> velocity=< 1, -2>
 - position=< 7,  6> velocity=<-1, -1>
 - position=<-2,  3> velocity=< 1,  0>
 - position=<-4,  3> velocity=< 2,  0>
 - position=<10, -3> velocity=<-1,  1>
 - position=< 5, 11> velocity=< 1, -2>
 - position=< 4,  7> velocity=< 0, -1>
 - position=< 8, -2> velocity=< 0,  1>
 - position=<15,  0> velocity=<-2,  0>
 - position=< 1,  6> velocity=< 1,  0>
 - position=< 8,  9> velocity=< 0, -1>
 - position=< 3,  3> velocity=<-1,  1>
 - position=< 0,  5> velocity=< 0, -1>
 - position=<-2,  2> velocity=< 2,  0>
 - position=< 5, -2> velocity=< 1,  2>
 - position=< 1,  4> velocity=< 2,  1>
 - position=<-2,  7> velocity=< 2, -2>
 - position=< 3,  6> velocity=<-1, -1>
 - position=< 5,  0> velocity=< 1,  0>
 - position=<-6,  0> velocity=< 2,  0>
 - position=< 5,  9> velocity=< 1, -2>
 - position=<14,  7> velocity=<-2,  0>
 - position=<-3,  6> velocity=< 2, -1>
 -
 - Each line represents one point. Positions are given as <X, Y> pairs: X
 - represents how far left (negative) or right (positive) the point appears,
 - while Y represents how far up (negative) or down (positive) the point
 - appears.
 -
 - At 0 seconds, each point has the position given. Each second, each point's
 - velocity is added to its position. So, a point with velocity <1, -2> is
 - moving to the right, but is moving upward twice as quickly. If this point's
 - initial position were <3, 9>, after 3 seconds, its position would become <6,
 - 3>.
 -
 - Over time, the points listed above would move like this:
 -
 - Initially:
 - ........#.............
 - ................#.....
 - .........#.#..#.......
 - ......................
 - #..........#.#.......#
 - ...............#......
 - ....#.................
 - ..#.#....#............
 - .......#..............
 - ......#...............
 - ...#...#.#...#........
 - ....#..#..#.........#.
 - .......#..............
 - ...........#..#.......
 - #...........#.........
 - ...#.......#..........
 -
 - After 1 second:
 - ......................
 - ......................
 - ..........#....#......
 - ........#.....#.......
 - ..#.........#......#..
 - ......................
 - ......#...............
 - ....##.........#......
 - ......#.#.............
 - .....##.##..#.........
 - ........#.#...........
 - ........#...#.....#...
 - ..#...........#.......
 - ....#.....#.#.........
 - ......................
 - ......................
 -
 - After 2 seconds:
 - ......................
 - ......................
 - ......................
 - ..............#.......
 - ....#..#...####..#....
 - ......................
 - ........#....#........
 - ......#.#.............
 - .......#...#..........
 - .......#..#..#.#......
 - ....#....#.#..........
 - .....#...#...##.#.....
 - ........#.............
 - ......................
 - ......................
 - ......................
 -
 - After 3 seconds:
 - ......................
 - ......................
 - ......................
 - ......................
 - ......#...#..###......
 - ......#...#...#.......
 - ......#...#...#.......
 - ......#####...#.......
 - ......#...#...#.......
 - ......#...#...#.......
 - ......#...#...#.......
 - ......#...#..###......
 - ......................
 - ......................
 - ......................
 - ......................
 -
 - After 4 seconds:
 - ......................
 - ......................
 - ......................
 - ............#.........
 - ........##...#.#......
 - ......#.....#..#......
 - .....#..##.##.#.......
 - .......##.#....#......
 - ...........#....#.....
 - ..............#.......
 - ....#......#...#......
 - .....#.....##.........
 - ...............#......
 - ...............#......
 - ......................
 - ......................
 -
 - After 3 seconds, the message appeared briefly: HI. Of course, your message
 - will be much longer and will take many more seconds to appear.
 -
 - What message will eventually appear in the sky?
 -}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
module Main where

import AdventOfCode
import ClassyPrelude
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import Prelude (iterate)
import qualified Text.Parsec as P
import qualified Text.Parsec.Number as P

main :: IO ()
main = defaultMain parseInput handleInput

handleInput :: MonadIO m => NonNull [Particle Int] -> m ()
handleInput = putStrLn . drawParticles . findMessage

findMessage
  :: (Num a, Ord a, MonoFunctor mono, MonoFoldable mono,
    Element mono ~ Particle a)
  => mono
  -> mono
findMessage particles
    = fst
    . Maybe.fromJust -- List is infinite so either no return or Just.
    . find distanceFalling
    $ candidates
  where
    candidates = zip (p:ps) ps
    (p:ps) = iterate updatePositions particles
    distanceFalling (ps1, ps2) =
        sum (compareDistances ps1) < sum (compareDistances ps2)

data Particle a = Particle
    { _position :: (a, a)
    , _velocity :: (a, a)
    }
  deriving (Show, Eq, Ord)

compareDistance :: Num a => Particle a -> Particle a -> a
compareDistance (Particle (x1, y1) _) (Particle (x2, y2) _) =
    ((x1 - x2)^two) + ((y1 - y2)^two)
  where
    two = 2 :: Int

compareDistances
    :: (Num a, MonoFoldable mono, Element mono ~ Particle a)
    => mono
    -> [a]
compareDistances particles = map (uncurry compareDistance) pairs
  where
    pairs = (,) <$> toList particles <*> toList particles

subtractPosition :: Num a => (a, a) -> Particle a -> Particle a
subtractPosition (dx, dy) (Particle (x, y) velocity) =
    Particle (x - dx, y - dy) velocity

updatePosition :: Num a => Particle a -> Particle a
updatePosition (Particle (x, y) (vx, vy)) = Particle (x + vx, y + vy) (vx, vy)

updatePositions
    :: (Num a, MonoFunctor mono, Element mono ~ Particle a)
    => mono
    -> mono
updatePositions = omap updatePosition

drawParticles
    :: (MonoFunctor mono, MonoFoldable mono, Element mono ~ Particle Int)
    => mono
    -> Text
drawParticles particles
    | null particles = ""
    | otherwise
        = Text.intercalate "\n"
        . Text.chunksOf (upperX+1)
        . pack
        . fmap drawPosition
        $ positions
  where
    hasParticle = Set.fromList . fmap _position . toList $ normalizedParticles
    normalizedParticles = normalize particles
    upperX = getX . maximumByEx (comparing getX) $ normalizedParticles
    upperY = getY . maximumByEx (comparing getY) $ normalizedParticles
    positions = (,) <$> [0..upperY] <*> [0..upperX]
    drawPosition pos = if swap pos `member` hasParticle then '#' else '.'

normalize
    :: (Num a, Ord a, MonoFunctor mono, MonoFoldable mono,
        Element mono ~ Particle a)
    => mono
    -> mono
normalize particles
    | null particles = particles
    | otherwise = omap (subtractPosition (lowerX, lowerY)) particles
  where
    lowerX = getX . minimumByEx (comparing getX) $ particles
    lowerY = getY . minimumByEx (comparing getY) $ particles

getX :: Particle a -> a
getX = fst . _position

getY :: Particle a -> a
getY = snd . _position

parseInput :: LText -> Either P.ParseError (NonNull [Particle Int])
parseInput = P.parse (parseParticles <* P.eof) ""

parseParticles :: P.Parsec LText () (NonNull [Particle Int])
parseParticles = impureNonNull <$> P.many1 (parseParticle <* P.char '\n')

parseParticle :: P.Parsec LText () (Particle Int)
parseParticle = Particle
    <$> (P.string "position=" *> parseTuple)
    <*> (P.string " velocity=" *> parseTuple)
  where
    parseTuple = P.between (P.char '<') (P.char '>') $ (,)
        <$> (P.optional (P.char ' ') *> P.int <* P.string ", ")
        <*> (P.optional (P.char ' ') *> P.int)

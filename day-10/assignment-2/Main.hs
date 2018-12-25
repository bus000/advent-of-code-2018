{- --- Part Two ---
 -
 - Good thing you didn't have to wait, because that would have taken a long time
 - - much longer than the 3 seconds in the example above.
 -
 - Impressed by your sub-hour communication capabilities, the Elves are curious:
 - exactly how many seconds would they have needed to wait for that message to
 - appear?
 -}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
module Main where

import AdventOfCode
import ClassyPrelude
import Prelude (iterate)
import qualified Text.Parsec as P
import qualified Text.Parsec.Number as P

main :: IO ()
main = defaultMain parseInput handleInput

handleInput :: MonadIO m => NonNull [Particle Int] -> m ()
handleInput = print . length . statesUntilMessage

statesUntilMessage
  :: (Num a, Ord a, MonoFunctor mono, MonoFoldable mono,
    Element mono ~ Particle a)
  => mono
  -> [mono]
statesUntilMessage particles
    = map fst
    . takeWhile (not . distanceFalling)
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

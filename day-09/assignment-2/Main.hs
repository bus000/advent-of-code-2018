{- --- Part Two ---
 -
 - Amused by the speed of your answer, the Elves are curious:
 -
 - What would the new winning Elf's score be if the number of the last marble
 - were 100 times larger?
 -}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts  #-}
module Main where

import AdventOfCode
import ClassyPrelude hiding (point)
import qualified Control.Monad.State as M
import qualified Data.CircularList as CL
import qualified Data.Map as Map
import qualified Prelude
import qualified Text.Parsec as P
import qualified Text.Parsec.Number as P

main :: IO ()
main = defaultMain parseInput handleInput

handleInput :: MonadIO m => Configuration -> m ()
handleInput = print . maxPoints . runGame

type Marble = Int

type Player = Int

type Points = (Player, Int)

data GameState = GameState (CL.CircularList Int) [Player] deriving (Show)

runGame :: Configuration -> [Points]
runGame conf
    = catMaybes
    . M.evalState (mapM addMarble [1.._marbleCount conf * 100])
    . createGameState
    $ conf

createGameState :: Configuration -> GameState
createGameState conf = GameState initialBoard players
  where
    players = Prelude.cycle [1.._playerCount conf]
    initialBoard = CL.singleton 0

addMarble :: (M.MonadState GameState m) => Marble -> m (Maybe Points)
addMarble marble
    | marble `mod` 23 == 0 = do
        GameState circle (player:players) <- M.get

        let circle' = CL.moveLeftN circle 7
            points = CL.current circle'
            circle'' = CL.removeRight circle'
            gamestate' = GameState circle'' players

        M.put gamestate'
        return $ points >>= \p -> return (player, p + marble)
    | otherwise = do
        GameState circle (_:players) <- M.get

        let circle' = CL.insertLeft marble . CL.moveRight $ circle
            gamestate' = GameState circle' players

        M.put gamestate'
        return Nothing

maxPoints :: [Points] -> Int
maxPoints points = foldr max 0 . foldr addPoint Map.empty $ points
  where
    addPoint (player, point) = Map.insertWith (+) player point

data Configuration = Configuration
    { _playerCount :: Int
    , _marbleCount :: Int
    }
  deriving (Show)

parseInput :: LText -> Either P.ParseError Configuration
parseInput = P.parse (parseConfiguration <* P.eof) ""

parseConfiguration :: P.Parsec LText () Configuration
parseConfiguration = Configuration
    <$> P.int
    <* P.string " players; last marble is worth "
    <*> P.int
    <* P.string " points\n"

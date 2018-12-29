{- --- Part Two ---
 -
 - You realize that 20 generations aren't enough. After all, these plants will
 - need to last another 1500 years to even reach your timeline, not to mention
 - your future.
 -
 - After fifty billion (50000000000) generations, what is the sum of the numbers
 - of all pots which contain a plant?
 -}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import AdventOfCode
import ClassyPrelude hiding (lefts, rights)
import qualified Data.Tape as Tape
import qualified Prelude
import qualified Text.Parsec as P

main :: IO ()
main = defaultMain parseInput handleInput

handleInput :: MonadIO m => Input -> m ()
handleInput input = print $ (50000000000-1000) * diff + computeSum tape2
  where
    [tape1, tape2] = take 2. drop 999 . runSimulation $ input
    diff = computeSum tape2 - computeSum tape1

data Liveness = Living | Dead
  deriving (Show, Eq, Ord)

data Pot a = Pot
    { _potId    :: a
    , _liveness :: Liveness
    }
  deriving (Show, Eq, Ord)

data Rule = Rule
    { _lefts   :: [Liveness]
    , _current :: Liveness
    , _rights  :: [Liveness]
    , _target  :: Liveness
    }
  deriving (Show, Eq, Ord)

data Input = Input [Liveness] [Rule]
  deriving (Show, Eq, Ord)

runSimulation :: Input -> [Tape.Tape (Pot Int)]
runSimulation (Input liveness rules) =
    Prelude.iterate (applyRules rules) initialTape
  where
    initialTape = Tape.fromLists lefts rights r
    lefts = map (uncurry Pot) $ zip [-1,-2..(-10000)] (repeat Dead)
    r:rights = map (uncurry Pot) $ zip [0..10000] (liveness ++ repeat Dead)

applyRules :: [Rule] -> Tape.Tape (Pot a) -> Tape.Tape (Pot a)
applyRules rules = Tape.mapNeighbourhood getTarget
  where
    getTarget lefts current rights = case findRule rules lefts current rights of
        Just rule -> current { _liveness = _target rule }
        Nothing -> current

findRule :: [Rule] -> [Pot a] -> Pot a -> [Pot a] -> Maybe Rule
findRule rules lefts current rights =
    find (\x -> matchRule x ls c rs) rules
  where
    ls = map _liveness lefts
    rs = map _liveness rights
    c = _liveness current

matchRule :: Rule -> [Liveness] -> Liveness -> [Liveness] -> Bool
matchRule rule lefts current rights = matchLefts && matchCurrent && matchRights
  where
    matchLefts = _lefts rule `isPrefixOf` lefts
    matchCurrent = _current rule == current
    matchRights = _rights rule `isPrefixOf` rights

computeSum :: Num a => Tape.Tape (Pot a) -> a
computeSum tape = sum . map _potId . filter alive $ current:lefts ++ rights
  where
    current = Tape.read tape
    (lefts, rights) = Tape.take 10000 tape

alive :: Pot a -> Bool
alive pot = _liveness pot == Living

parseInput :: LText -> Either P.ParseError Input
parseInput = P.parse (parseInput' <* P.eof) ""

parseInput' :: P.Parsec LText () Input
parseInput' = do
    void $ P.string "initial state: "
    state <- parseState
    void $ P.string "\n\n"
    rules <- parseRules

    return $ Input state rules

parseState :: P.Parsec LText () [Liveness]
parseState = P.many parseLiveness

parseLiveness :: P.Parsec LText () Liveness
parseLiveness = do
    liveness <- P.satisfy $ \x -> x == '#' || x == '.'
    case liveness of
        '#' -> return Living
        '.' -> return Dead
        _ -> error "Should never happen."

parseRules :: P.Parsec LText () [Rule]
parseRules = parseRule `P.endBy` P.char '\n'

parseRule :: P.Parsec LText () Rule
parseRule = do
    [l2, l1, c, r1, r2] <- P.count 5 parseLiveness
    void $ P.string " => "
    target <- parseLiveness

    return $ Rule [l1, l2] c [r1, r2] target

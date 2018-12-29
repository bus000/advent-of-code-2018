{- --- Day 12: Subterranean Sustainability ---
 -
 - The year 518 is significantly more underground than your history books
 - implied. Either that, or you've arrived in a vast cavern network under the
 - North Pole.
 -
 - After exploring a little, you discover a long tunnel that contains a
 - row of small pots as far as you can see to your left and right. A few
 - of them contain plants - someone is trying to grow things in these
 - geothermally-heated caves.
 -
 - The pots are numbered, with 0 in front of you. To the left, the pots are
 - numbered -1, -2, -3, and so on; to the right, 1, 2, 3.... Your puzzle input
 - contains a list of pots from 0 to the right and whether they do (#) or do not
 - (.) currently contain a plant, the initial state. (No other pots currently
 - contain plants.) For example, an initial state of #..##.... indicates that
 - pots 0, 3, and 4 currently contain plants.
 -
 - Your puzzle input also contains some notes you find on a nearby table:
 - someone has been trying to figure out how these plants spread to nearby pots.
 - Based on the notes, for each generation of plants, a given pot has or does
 - not have a plant based on whether that pot (and the two pots on either side
 - of it) had a plant in the last generation. These are written as LLCRR => N,
 - where L are pots to the left, C is the current pot being considered, R are
 - the pots to the right, and N is whether the current pot will have a plant in
 - the next generation. For example:
 -
 - * A note like ..#.. => . means that a pot that contains a plant but with no
 -   plants within two pots of it will not have a plant in it during the next
 -   generation.
 - * A note like ##.## => . means that an empty pot with two plants on each side
 -   of it will remain empty in the next generation.
 - * A note like .##.# => # means that a pot has a plant in a given generation
 -   if, in the previous generation, there were plants in that pot, the one
 -   immediately to the left, and the one two pots to the right, but not in the
 -   ones immediately to the right and two to the left.
 -
 - It's not clear what these plants are for, but you're sure it's important, so
 - you'd like to make sure the current configuration of plants is sustainable by
 - determining what will happen after 20 generations.
 -
 - For example, given the following input:
 -
 - initial state: #..#.#..##......###...###
 -
 - ...## => #
 - ..#.. => #
 - .#... => #
 - .#.#. => #
 - .#.## => #
 - .##.. => #
 - .#### => #
 - #.#.# => #
 - #.### => #
 - ##.#. => #
 - ##.## => #
 - ###.. => #
 - ###.# => #
 - ####. => #
 -
 - For brevity, in this example, only the combinations which do produce a plant
 - are listed. (Your input includes all possible combinations.) Then, the next
 - 20 generations will look like this:
 -
 -                  1         2         3
 -        0         0         0         0
 -  0: ...#..#.#..##......###...###...........
 -  1: ...#...#....#.....#..#..#..#...........
 -  2: ...##..##...##....#..#..#..##..........
 -  3: ..#.#...#..#.#....#..#..#...#..........
 -  4: ...#.#..#...#.#...#..#..##..##.........
 -  5: ....#...##...#.#..#..#...#...#.........
 -  6: ....##.#.#....#...#..##..##..##........
 -  7: ...#..###.#...##..#...#...#...#........
 -  8: ...#....##.#.#.#..##..##..##..##.......
 -  9: ...##..#..#####....#...#...#...#.......
 - 10: ..#.#..#...#.##....##..##..##..##......
 - 11: ...#...##...#.#...#.#...#...#...#......
 - 12: ...##.#.#....#.#...#.#..##..##..##.....
 - 13: ..#..###.#....#.#...#....#...#...#.....
 - 14: ..#....##.#....#.#..##...##..##..##....
 - 15: ..##..#..#.#....#....#..#.#...#...#....
 - 16: .#.#..#...#.#...##...#...#.#..##..##...
 - 17: ..#...##...#.#.#.#...##...#....#...#...
 - 18: ..##.#.#....#####.#.#.#...##...##..##..
 - 19: .#..###.#..#.#.#######.#.#.#..#.#...#..
 - 20: .#....##....#####...#######....#.#..##.
 -
 - The generation is shown along the left, where 0 is the initial state.
 - The pot numbers are shown along the top, where 0 labels the center pot,
 - negative-numbered pots extend to the left, and positive pots extend toward
 - the right. Remember, the initial state begins at pot 0, which is not the
 - leftmost pot used in this example.
 -
 - After one generation, only seven plants remain. The one in pot 0 matched the
 - rule looking for ..#.., the one in pot 4 matched the rule looking for .#.#.,
 - pot 9 matched .##.., and so on.
 -
 - In this example, after 20 generations, the pots shown as # contain plants,
 - the furthest left of which is pot -2, and the furthest right of which is
 - pot 34. Adding up all the numbers of plant-containing pots after the 20th
 - generation produces 325.
 -
 - After 20 generations, what is the sum of the numbers of all pots which
 - contain a plant?
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
handleInput input = case index (runSimulation input) 20 of
        Just tape -> print . computeSum $ tape
        Nothing -> putStrLn "Unexpected limited list."

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
    lefts = map (uncurry Pot) $ zip [-1,-2..] (repeat Dead)
    r:rights = map (uncurry Pot) $ zip [0..] (liveness ++ repeat Dead)

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
    (lefts, rights) = Tape.take 1000 tape

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

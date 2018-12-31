{- --- Part Two ---
 -
 - There isn't much you can do to prevent crashes in this ridiculous system.
 - However, by predicting the crashes, the Elves know where to be in advance and
 - instantly remove the two crashing carts the moment any crash occurs.
 -
 - They can proceed like this for a while, but eventually, they're going to
 - run out of carts. It could be useful to figure out where the last cart that
 - hasn't crashed will end up.
 -
 - For example:
 -
 - />-<\
 - |   |
 - | /<+-\
 - | | | v
 - \>+</ |
 -   |   ^
 -   \<->/
 -
 - /---\
 - |   |
 - | v-+-\
 - | | | |
 - \-+-/ |
 -   |   |
 -   ^---^
 -
 - /---\
 - |   |
 - | /-+-\
 - | v | |
 - \-+-/ |
 -   ^   ^
 -   \---/
 -
 - /---\
 - |   |
 - | /-+-\
 - | | | |
 - \-+-/ ^
 -   |   |
 -   \---/
 -
 - After four very expensive crashes, a tick ends with only one cart remaining;
 - its final location is 6,4.
 -
 - What is the location of the last cart at the end of the first tick where it
 - is the only cart left?
 -}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import AdventOfCode
import ClassyPrelude hiding (lefts, rights)
import qualified Data.Array as A
import qualified Data.Map.Strict as Map
import qualified Prelude
import qualified Text.Parsec as P

main :: IO ()
main = defaultMain parseInput handleInput

handleInput :: MonadIO m => Grid (Tile (Maybe Direction)) -> m ()
handleInput grid
    = mapM_ printPos
    . map fst
    . fromMaybe []
    . fmap Map.toList
    . find ((== 1) . Map.size)
    . Prelude.iterate (tick grid)
    $ carts
  where
    carts = findCarts grid
    printPos (Position (x, y)) = do
        putStr . pack . show $ x
        putStr ","
        putStrLn . pack . show $ y

newtype Position = Position (Int, Int) deriving (Eq, A.Ix, Show)

instance Ord Position where
    compare (Position (x1, y1)) (Position (x2, y2)) = case compare y1 y2 of
        LT -> LT
        GT -> GT
        EQ -> compare x1 x2

newtype Grid a = Grid (A.Array Position a)

data Cart = Cart Direction [Turn]

data Tile a
    = Empty
    | Intersection a
    | Turn (Direction -> Direction) a
    | Strait Orientation a

data Direction
    = North
    | South
    | West
    | East
  deriving (Show)

data Turn = TLeft | TRight | TStrait deriving (Show)

data Orientation
    = Horizontal
    | Vertical

tick :: Grid (Tile a) -> Map Position Cart -> Map Position Cart
tick grid carts = tick' grid (carts, Map.empty)

tick'
    :: Grid (Tile a)
    -> (Map Position Cart, Map Position Cart)
    -> Map Position Cart
tick' grid (notMoved, moved)
    | null notMoved = moved
    | pos' `Map.member` notMoved' || pos' `Map.member` moved =
        tick' grid (Map.delete pos' notMoved', Map.delete pos' moved)
    | otherwise = tick' grid (notMoved', Map.insert pos' cart' moved)
  where
    ((pos, cart), notMoved') = Map.deleteFindMin notMoved
    (cart', pos') = nextPosition grid cart pos

findCarts :: Grid (Tile (Maybe Direction)) -> Map Position Cart
findCarts (Grid array) = Map.fromList . mapMaybe toCart . A.assocs $ array
  where
    toCart (_, Empty) = Nothing
    toCart (i, Intersection (Just direction)) = Just (i, Cart direction turns)
    toCart (_, Intersection Nothing) = Nothing
    toCart (i, Turn _ (Just direction)) = Just (i, Cart direction turns)
    toCart (_, Turn _ Nothing) = Nothing
    toCart (i, Strait _ (Just direction)) = Just (i, Cart direction turns)
    toCart (_, Strait _ Nothing) = Nothing

    turns = Prelude.cycle [TLeft, TStrait, TRight]

nextPosition :: Grid (Tile a) -> Cart -> Position -> (Cart, Position)
nextPosition (Grid array) (Cart direction turns) pos = case array A.! pos' of
    Empty -> error "Cart on empty field."
    Intersection _ -> (Cart (performTurn direction t1) ts, pos')
    Turn turn _ -> (Cart (turn direction) turns, pos')
    Strait _ _ -> (Cart direction turns, pos')
  where
    pos' = addDirection pos direction
    (t1:ts) = turns

addDirection :: Position -> Direction -> Position
addDirection (Position (x, y)) North = Position (x, y - 1)
addDirection (Position (x, y)) South = Position (x, y + 1)
addDirection (Position (x, y)) East = Position (x + 1, y)
addDirection (Position (x, y)) West = Position (x - 1, y)

performTurn :: Direction -> Turn -> Direction
performTurn direction TStrait = direction
performTurn North TLeft = West
performTurn North TRight = East
performTurn South TLeft = East
performTurn South TRight = West
performTurn West TLeft = South
performTurn West TRight = North
performTurn East TLeft = North
performTurn East TRight = South

parseInput :: LText -> Either P.ParseError (Grid (Tile (Maybe Direction)))
parseInput = P.parse (parseGrid <* P.eof) ""

parseGrid :: P.Parsec LText () (Grid (Tile (Maybe Direction)))
parseGrid = do
    rows <- parseRow `P.endBy` P.char '\n'

    let rowN = length rows
        colN = fromMaybe 0 . fmap length . headMay $ rows
        bounds = (Position (0, 0), Position (colN - 1, rowN - 1))
        indices = sort . map Position $ (,) <$> [0..colN - 1] <*> [0..rowN - 1]
        elements = zip indices . concat $ rows
        array = A.array bounds elements

    return $! Grid array

parseRow :: P.Parsec LText () [Tile (Maybe Direction)]
parseRow = P.many parseTile

parseTile :: P.Parsec LText () (Tile (Maybe Direction))
parseTile = do
    c <- P.satisfy $ \x -> x `elem` validChar

    case c of
        '|' -> return $! Strait Vertical Nothing
        '-' -> return $! Strait Horizontal Nothing
        '/' -> return $! Turn turn1 Nothing
        '\\' -> return $! Turn turn2 Nothing
        '^' -> return $! Strait Vertical (Just North)
        'v' -> return $! Strait Vertical (Just South)
        '>' -> return $! Strait Horizontal (Just East)
        '<' -> return $! Strait Horizontal (Just West)
        ' ' -> return $! Empty
        '+' -> return $! Intersection Nothing
        _ -> error "Should not be possible."
  where
    validChar = "|-/\\^v>< +" :: Text

turn1 :: Direction -> Direction
turn1 North = East
turn1 South = West
turn1 West = South
turn1 East = North

turn2 :: Direction -> Direction
turn2 North = West
turn2 South = East
turn2 West = North
turn2 East = South

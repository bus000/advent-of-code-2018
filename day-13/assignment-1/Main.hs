{- --- Day 13: Mine Cart Madness ---
 -
 - A crop of this size requires significant logistics to transport produce,
 - soil, fertilizer, and so on. The Elves are very busy pushing things around in
 - carts on some kind of rudimentary system of tracks they've come up with.
 -
 - Seeing as how cart-and-track systems don't appear in recorded history for
 - another 1000 years, the Elves seem to be making this up as they go along.
 - They haven't even figured out how to avoid collisions yet.
 -
 - You map out the tracks (your puzzle input) and see where you can help.
 -
 - Tracks consist of straight paths (| and -), curves (/ and \), and
 - intersections (+). Curves connect exactly two perpendicular pieces of track;
 - for example, this is a closed loop:
 -
 - /----\
 - |    |
 - |    |
 - \----/
 -
 - Intersections occur when two perpendicular paths cross. At an intersection, a
 - cart is capable of turning left, turning right, or continuing straight. Here
 - are two loops connected by two intersections:
 -
 - /-----\
 - |     |
 - |  /--+--\
 - |  |  |  |
 - \--+--/  |
 -    |     |
 -    \-----/
 -
 - Several carts are also on the tracks. Carts always face either up (^), down
 - (v), left (<), or right (>). (On your initial map, the track under each cart
 - is a straight path matching the direction the cart is facing.)
 -
 - Each time a cart has the option to turn (by arriving at any intersection),
 - it turns left the first time, goes straight the second time, turns right the
 - third time, and then repeats those directions starting again with left the
 - fourth time, straight the fifth time, and so on. This process is independent
 - of the particular intersection at which the cart has arrived - that is, the
 - cart has no per-intersection memory.
 -
 - Carts all move at the same speed; they take turns moving a single step at
 - a time. They do this based on their current location: carts on the top row
 - move first (acting from left to right), then carts on the second row move
 - (again from left to right), then carts on the third row, and so on. Once each
 - cart has moved one step, the process repeats; each of these loops is called a
 - tick.
 -
 - For example, suppose there are two carts on a straight track:
 -
 - |  |  |  |  |
 - v  |  |  |  |
 - |  v  v  |  |
 - |  |  |  v  X
 - |  |  ^  ^  |
 - ^  ^  |  |  |
 - |  |  |  |  |
 -
 - First, the top cart moves. It is facing down (v), so it moves down one
 - square. Second, the bottom cart moves. It is facing up (^), so it moves up
 - one square. Because all carts have moved, the first tick ends. Then, the
 - process repeats, starting with the first cart. The first cart moves down,
 - then the second cart moves up - right into the first cart, colliding with it!
 - (The location of the crash is marked with an X.) This ends the second and
 - last tick.
 -
 - Here is a longer example:
 -
 - /->-\
 - |   |  /----\
 - | /-+--+-\  |
 - | | |  | v  |
 - \-+-/  \-+--/
 -   \------/
 -
 - /-->\
 - |   |  /----\
 - | /-+--+-\  |
 - | | |  | |  |
 - \-+-/  \->--/
 -   \------/
 -
 - /---v
 - |   |  /----\
 - | /-+--+-\  |
 - | | |  | |  |
 - \-+-/  \-+>-/
 -   \------/
 -
 - /---\
 - |   v  /----\
 - | /-+--+-\  |
 - | | |  | |  |
 - \-+-/  \-+->/
 -   \------/
 -
 - /---\
 - |   |  /----\
 - | /->--+-\  |
 - | | |  | |  |
 - \-+-/  \-+--^
 -   \------/
 -
 - /---\
 - |   |  /----\
 - | /-+>-+-\  |
 - | | |  | |  ^
 - \-+-/  \-+--/
 -   \------/
 -
 - /---\
 - |   |  /----\
 - | /-+->+-\  ^
 - | | |  | |  |
 - \-+-/  \-+--/
 -   \------/
 -
 - /---\
 - |   |  /----<
 - | /-+-->-\  |
 - | | |  | |  |
 - \-+-/  \-+--/
 -   \------/
 -
 - /---\
 - |   |  /---<\
 - | /-+--+>\  |
 - | | |  | |  |
 - \-+-/  \-+--/
 -   \------/
 -
 - /---\
 - |   |  /--<-\
 - | /-+--+-v  |
 - | | |  | |  |
 - \-+-/  \-+--/
 -   \------/
 -
 - /---\
 - |   |  /-<--\
 - | /-+--+-\  |
 - | | |  | v  |
 - \-+-/  \-+--/
 -   \------/
 -
 - /---\
 - |   |  /<---\
 - | /-+--+-\  |
 - | | |  | |  |
 - \-+-/  \-<--/
 -   \------/
 -
 - /---\
 - |   |  v----\
 - | /-+--+-\  |
 - | | |  | |  |
 - \-+-/  \<+--/
 -   \------/
 -
 - /---\
 - |   |  /----\
 - | /-+--v-\  |
 - | | |  | |  |
 - \-+-/  ^-+--/
 -   \------/
 -
 - /---\
 - |   |  /----\
 - | /-+--+-\  |
 - | | |  X |  |
 - \-+-/  \-+--/
 -   \------/
 -
 - After following their respective paths for a while, the carts eventually
 - crash. To help prevent crashes, you'd like to know the location of the first
 - crash. Locations are given in X,Y coordinates, where the furthest left column
 - is X=0 and the furthest top row is Y=0:
 -
 -            111
 -  0123456789012
 - 0/---\
 - 1|   |  /----\
 - 2| /-+--+-\  |
 - 3| | |  X |  |
 - 4\-+-/  \-+--/
 - 5  \------/
 -
 - In this example, the location of the first crash is 7,3.
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
handleInput grid = printPos . firstLeft (tick grid) $ carts
  where
    carts = findCarts grid
    printPos (Collision (Position (x, y))) = do
        putStr . pack . show $ x
        putStr ","
        putStrLn . pack . show $ y

firstLeft :: (a -> Either b a) -> a -> b
firstLeft f x = case f x of
    Left err -> err
    Right res -> firstLeft f res

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

newtype Collision = Collision Position deriving (Show)

tick :: Grid (Tile a) -> Map Position Cart -> Either Collision (Map Position Cart)
tick grid = foldr (tick' grid) (Right Map.empty) . Map.toAscList

tick'
    :: Grid (Tile a)
    -> (Position, Cart)
    -> Either Collision (Map Position Cart)
    -> Either Collision (Map Position Cart)
tick' _ _ err@(Left _) = err
tick' grid (pos, cart) (Right carts)
    | pos' `Map.member` carts = Left $ Collision pos'
    | otherwise = Right $ Map.insert pos' cart' carts
  where
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

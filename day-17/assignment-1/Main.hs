{- --- Day 17: Reservoir Research ---
 -
 - You arrive in the year 18. If it weren't for the coat you got in 1018, you
 - would be very cold: the North Pole base hasn't even been constructed.
 -
 - Rather, it hasn't been constructed yet. The Elves are making a little
 - progress, but there's not a lot of liquid water in this climate, so they're
 - getting very dehydrated. Maybe there's more underground?
 -
 - You scan a two-dimensional vertical slice of the ground nearby and discover
 - that it is mostly sand with veins of clay. The scan only provides data with a
 - granularity of square meters, but it should be good enough to determine how
 - much water is trapped there. In the scan, x represents the distance to the
 - right, and y represents the distance down. There is also a spring of water
 - near the surface at x=500, y=0. The scan identifies which square meters are
 - clay (your puzzle input).
 -
 - For example, suppose your scan shows the following veins of clay:
 -
 -    x=495, y=2..7
 -    y=7, x=495..501
 -    x=501, y=3..7
 -    x=498, y=2..4
 -    x=506, y=1..2
 -    x=498, y=10..13
 -    x=504, y=10..13
 -    y=13, x=498..504
 -
 - Rendering clay as #, sand as ., and the water spring as +, and with x
 - increasing to the right and y increasing downward, this becomes:
 -
 -       44444455555555
 -       99999900000000
 -       45678901234567
 -     0 ......+.......
 -     1 ............#.
 -     2 .#..#.......#.
 -     3 .#..#..#......
 -     4 .#..#..#......
 -     5 .#.....#......
 -     6 .#.....#......
 -     7 .#######......
 -     8 ..............
 -     9 ..............
 -    10 ....#.....#...
 -    11 ....#.....#...
 -    12 ....#.....#...
 -    13 ....#######...
 -
 - The spring of water will produce water forever. Water can move through sand,
 - but is blocked by clay. Water always moves down when possible, and spreads to
 - the left and right otherwise, filling space that has clay on both sides and
 - falling out otherwise.
 -
 - For example, if five squares of water are created, they will flow downward
 - until they reach the clay and settle there. Water that has come to rest is
 - shown here as ~, while sand through which water has passed (but which is now
 - dry again) is shown as |:
 -
 -    ......+.......
 -    ......|.....#.
 -    .#..#.|.....#.
 -    .#..#.|#......
 -    .#..#.|#......
 -    .#....|#......
 -    .#~~~~~#......
 -    .#######......
 -    ..............
 -    ..............
 -    ....#.....#...
 -    ....#.....#...
 -    ....#.....#...
 -    ....#######...
 -
 - Two squares of water can't occupy the same location. If another five squares
 - of water are created, they will settle on the first five, filling the clay
 - reservoir a little more:
 -
 -    ......+.......
 -      ......|.....#.
 -      .#..#.|.....#.
 -      .#..#.|#......
 -      .#..#.|#......
 -      .#~~~~~#......
 -      .#~~~~~#......
 -      .#######......
 -      ..............
 -      ..............
 -      ....#.....#...
 -      ....#.....#...
 -      ....#.....#...
 -      ....#######...
 -
 - Water pressure does not apply in this scenario. If another four squares of
 - water are created, they will stay on the right side of the barrier, and no
 - water will reach the left side:
 -
 -    ......+.......
 -    ......|.....#.
 -    .#..#.|.....#.
 -    .#..#~~#......
 -    .#..#~~#......
 -    .#~~~~~#......
 -    .#~~~~~#......
 -    .#######......
 -    ..............
 -    ..............
 -    ....#.....#...
 -    ....#.....#...
 -    ....#.....#...
 -    ....#######...
 -
 - At this point, the top reservoir overflows. While water can reach the tiles
 - above the surface of the water, it cannot settle there, and so the next five
 - squares of water settle like this:
 -
 -    ......+.......
 -    ......|.....#.
 -    .#..#||||...#.
 -    .#..#~~#|.....
 -    .#..#~~#|.....
 -    .#~~~~~#|.....
 -    .#~~~~~#|.....
 -    .#######|.....
 -    ........|.....
 -    ........|.....
 -    ....#...|.#...
 -    ....#...|.#...
 -    ....#~~~~~#...
 -    ....#######...
 -
 - Note especially the leftmost |: the new squares of water can reach this tile,
 - but cannot stop there. Instead, eventually, they all fall to the right and
 - settle in the reservoir below.
 -
 - After 10 more squares of water, the bottom reservoir is also full:
 -
 -    ......+.......
 -    ......|.....#.
 -    .#..#||||...#.
 -    .#..#~~#|.....
 -    .#..#~~#|.....
 -    .#~~~~~#|.....
 -    .#~~~~~#|.....
 -    .#######|.....
 -    ........|.....
 -    ........|.....
 -    ....#~~~~~#...
 -    ....#~~~~~#...
 -    ....#~~~~~#...
 -    ....#######...
 -
 - Finally, while there is nowhere left for the water to settle, it can reach a
 - few more tiles before overflowing beyond the bottom of the scanned data:
 -
 -    ......+.......    (line not counted: above minimum y value)
 -    ......|.....#.
 -    .#..#||||...#.
 -    .#..#~~#|.....
 -    .#..#~~#|.....
 -    .#~~~~~#|.....
 -    .#~~~~~#|.....
 -    .#######|.....
 -    ........|.....
 -    ...|||||||||..
 -    ...|#~~~~~#|..
 -    ...|#~~~~~#|..
 -    ...|#~~~~~#|..
 -    ...|#######|..
 -    ...|.......|..    (line not counted: below maximum y value)
 -    ...|.......|..    (line not counted: below maximum y value)
 -    ...|.......|..    (line not counted: below maximum y value)
 -
 - How many tiles can be reached by the water? To prevent counting forever,
 - ignore tiles with a y coordinate smaller than the smallest y coordinate in
 - your scan data or larger than the largest one. Any x coordinate is valid. In
 - this example, the lowest y coordinate given is 1, and the highest is 13,
 - causing the water spring (in row 0) and the water falling off the bottom of
 - the render (in rows 14 through infinity) to be ignored.
 -
 - So, in the example above, counting both water at rest (~) and other sand
 - tiles the water can hypothetically reach (|), the total number of tiles the
 - water can reach is 57.
 -
 - How many tiles can the water reach within the range of y values in your scan?
 -}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module Main where

import AdventOfCode
import qualified Text.Parsec as P
import qualified Text.Parsec.Number as P
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Control.Monad.State.Strict as M

main :: IO ()
main = preludeMain parseInput handleInput

data Position = Position
    { _x :: Int
    , _y :: Int
    } deriving (Show, Eq, Ord)

data TileType = Clay | Still | Flowing deriving (Eq, Ord, Show)

data GroundSlice = GroundSlice
    { _tiles :: !(Map.Map Position TileType)
    , _maxY  :: !Int
    , _minY  :: !Int
    } deriving (Eq, Ord, Show)

handleInput :: GroundSlice -> IO ()
handleInput
    = print
    . length
    . filter isWater
    . Map.elems
    . simulate (Position 500 0)
  where
    isWater Clay = False
    isWater Still = True
    isWater Flowing = True

simulate :: Position -> GroundSlice -> Map.Map Position TileType
simulate initPos (GroundSlice initOccupied maxY minY) =
    M.execState (go initPos) initOccupied
  where
    go :: Position -> M.State (Map.Map Position TileType) TileType
    go pos
        | _y pos < minY = go (down pos)
        | _y pos > maxY = pure Flowing
        | otherwise = M.get >>= \occ -> case Map.lookup pos occ of
            Just tile -> pure tile
            Nothing -> go (down pos) >>= \case
                Flowing -> set pos Flowing
                _ -> do
                    set pos Still -- Guess still.
                    fl <- go (left pos)
                    fr <- go (right pos)
                    -- Fix guess.
                    case (fl, fr) of
                        (Flowing, Flowing) -> set pos Flowing
                        (Flowing, _) -> flowRight pos >> return Flowing
                        (_, Flowing) -> flowLeft pos >> return Flowing
                        _ -> return Still

    flowRight :: Position -> M.State (Map.Map Position TileType) ()
    flowRight pos = M.get >>= \tiles -> case Map.lookup pos tiles of
        Just Still -> set pos Flowing >> flowRight (right pos)
        _ -> return ()

    flowLeft :: Position -> M.State (Map.Map Position TileType) ()
    flowLeft pos = M.get >>= \tiles -> case Map.lookup pos tiles of
        Just Still -> set pos Flowing >> flowLeft (left pos)
        _ -> return ()

    set :: Position -> TileType -> M.State (Map.Map Position TileType) TileType
    set pos value = M.modify (Map.insert pos value) >> pure value

showTiles :: Map.Map Position TileType -> String
showTiles tiles = concat $ do
    y <- [0..maxY]
    x <- [minX..maxX]
    let c = case Map.lookup (Position x y) tiles of
            Nothing -> '.'
            Just Still -> '~'
            Just Flowing -> '|'
            Just Clay -> '#'
    if x == maxX
        then return [c, '\n']
        else return [c]
  where
    minX = minimum . map _x . Map.keys $ tiles
    maxX = maximum . map _x . Map.keys $ tiles
    maxY = maximum . map _y . Map.keys $ tiles

down :: Position -> Position
down (Position x y) = Position x (y + 1)

left :: Position -> Position
left (Position x y) = Position (x - 1) y

right :: Position -> Position
right (Position x y) = Position (x + 1) y

fromClays :: [Position] -> GroundSlice
fromClays clays = GroundSlice tiles maxY minY
  where
    tiles = Map.fromList . map (, Clay) $ clays
    maxY = maximum . map _y $ clays
    minY = minimum . map _y $ clays

parseInput :: T.Text -> Either P.ParseError GroundSlice
parseInput = P.parse (parseGroundSlice <* P.eof) ""

parseGroundSlice :: P.Parsec T.Text () GroundSlice
parseGroundSlice = fromClays . concat <$> parseClayLine `P.endBy` P.newline

parseClayLine :: P.Parsec T.Text () [Position]
parseClayLine = P.choice [vertical, horizontal]
  where
    horizontal = do
        P.string "x="
        x <- P.int
        P.string ", y="
        yMin <- P.int
        P.string ".."
        yMax <- P.int

        return $ map (Position x) [yMin..yMax]
    vertical = do
        P.string "y="
        y <- P.int
        P.string ", x="
        xMin <- P.int
        P.string ".."
        xMax <- P.int

        return $ map (flip Position y) [xMin..xMax]

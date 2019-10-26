{- --- Day 18: Settlers of The North Pole ---
 -
 - On the outskirts of the North Pole base construction project, many Elves are
 - collecting lumber.
 -
 - The lumber collection area is 50 acres by 50 acres; each acre can be either open
 - ground (.), trees (|), or a lumberyard (#). You take a scan of the area (your
 - puzzle input).
 -
 - Strange magic is at work here: each minute, the landscape looks entirely
 - different. In exactly one minute, an open acre can fill with trees, a wooded
 - acre can be converted to a lumberyard, or a lumberyard can be cleared to open
 - ground (the lumber having been sent to other projects).
 -
 - The change to each acre is based entirely on the contents of that acre as well
 - as the number of open, wooded, or lumberyard acres adjacent to it at the start
 - of each minute. Here, "adjacent" means any of the eight acres surrounding that
 - acre. (Acres on the edges of the lumber collection area might have fewer than
 - eight adjacent acres; the missing acres aren't counted.)
 -
 - In particular:
 -
 - * An open acre will become filled with trees if three or more adjacent acres
 -   contained trees. Otherwise, nothing happens.
 - * An acre filled with trees will become a lumberyard if three or more
 -   adjacent acres were lumberyards. Otherwise, nothing happens.
 - * An acre containing a lumberyard will remain a lumberyard if it was adjacent
 -   to at least one other lumberyard and at least one acre containing trees.
 -   Otherwise, it becomes open.
 -
 - These changes happen across all acres simultaneously, each of them using the
 - state of all acres at the beginning of the minute and changing to their new form
 - by the end of that same minute. Changes that happen during the minute don't
 - affect each other.
 -
 - For example, suppose the lumber collection area is instead only 10 by 10 acres
 - with this initial configuration:
 -
 - Initial state:
 -
 - .#.#...|#.
 - .....#|##|
 - .|..|...#.
 - ..|#.....#
 - #.#|||#|#|
 - ...#.||...
 - .|....|...
 - ||...#|.#|
 - |.||||..|.
 - ...#.|..|.
 -
 - After 1 minute:
 -
 - .......##.
 - ......|###
 - .|..|...#.
 - ..|#||...#
 - ..##||.|#|
 - ...#||||..
 - ||...|||..
 - |||||.||.|
 - ||||||||||
 - ....||..|.
 -
 - After 2 minutes:
 -
 - .......#..
 - ......|#..
 - .|.|||....
 - ..##|||..#
 - ..###|||#|
 - ...#|||||.
 - |||||||||.
 - ||||||||||
 - ||||||||||
 - .|||||||||
 -
 - After 3 minutes:
 -
 - .......#..
 - ....|||#..
 - .|.||||...
 - ..###|||.#
 - ...##|||#|
 - .||##|||||
 - ||||||||||
 - ||||||||||
 - ||||||||||
 - ||||||||||
 -
 - After 4 minutes:
 -
 - .....|.#..
 - ...||||#..
 - .|.#||||..
 - ..###||||#
 - ...###||#|
 - |||##|||||
 - ||||||||||
 - ||||||||||
 - ||||||||||
 - ||||||||||
 -
 - After 5 minutes:
 -
 - ....|||#..
 - ...||||#..
 - .|.##||||.
 - ..####|||#
 - .|.###||#|
 - |||###||||
 - ||||||||||
 - ||||||||||
 - ||||||||||
 - ||||||||||
 -
 - After 6 minutes:
 -
 - ...||||#..
 - ...||||#..
 - .|.###|||.
 - ..#.##|||#
 - |||#.##|#|
 - |||###||||
 - ||||#|||||
 - ||||||||||
 - ||||||||||
 - ||||||||||
 -
 - After 7 minutes:
 -
 - ...||||#..
 - ..||#|##..
 - .|.####||.
 - ||#..##||#
 - ||##.##|#|
 - |||####|||
 - |||###||||
 - ||||||||||
 - ||||||||||
 - ||||||||||
 -
 - After 8 minutes:
 -
 - ..||||##..
 - ..|#####..
 - |||#####|.
 - ||#...##|#
 - ||##..###|
 - ||##.###||
 - |||####|||
 - ||||#|||||
 - ||||||||||
 - ||||||||||
 -
 - After 9 minutes:
 -
 - ..||###...
 - .||#####..
 - ||##...##.
 - ||#....###
 - |##....##|
 - ||##..###|
 - ||######||
 - |||###||||
 - ||||||||||
 - ||||||||||
 -
 - After 10 minutes:
 -
 - .||##.....
 - ||###.....
 - ||##......
 - |##.....##
 - |##.....##
 - |##....##|
 - ||##.####|
 - ||#####|||
 - ||||#|||||
 - ||||||||||
 -
 - After 10 minutes, there are 37 wooded acres and 31 lumberyards. Multiplying the
 - number of wooded acres by the number of lumberyards gives the total resource
 - value after ten minutes: 37 * 31 = 1147.
 -
 - What will the total resource value of the lumber collection area be after 10 minutes?
 -}
module Main (main) where

import AdventOfCode
import qualified Data.Array as A
import qualified Data.List as L
import qualified Data.List.Split as L
import qualified Data.Maybe as M
import qualified Data.Text.Lazy as T
import qualified Text.Parsec as P

main :: IO ()
main = defaultMain parseInput handleInput

data AcreType = Forest | Lumberyard | Open deriving (A.Ix, Eq, Ord, Show)

data Land = Land !(A.Array (Int, Int) AcreType) deriving (Show)

handleInput :: Land -> IO ()
handleInput = print . evaluateLand . head . drop 10 . iterate step

evaluateLand :: Land -> Int
evaluateLand (Land xs) = trees * lumberyards
  where
    elements = A.elems xs
    trees = length . filter (== Forest) $ elements
    lumberyards = length . filter (== Lumberyard) $ elements

step :: Land -> Land
step (Land array) = Land $ mapNeighbourhood array mapTile

mapTile :: AcreType -> [AcreType] -> AcreType
mapTile acreType neighbours
    | acreType == Open && trees >= 3 = Forest
    | acreType == Open = Open
    | acreType == Forest && lumberyards >= 3 = Lumberyard
    | acreType == Forest = Forest
    | acreType == Lumberyard && lumberyards >= 1 && trees >= 1 = Lumberyard
    | acreType == Lumberyard = Open
  where
    trees = length . filter (== Forest) $ neighbours
    lumberyards = length . filter (== Lumberyard) $ neighbours

mapNeighbourhood :: A.Array (Int, Int) a -> (a -> [a] -> b) -> A.Array (Int, Int) b
mapNeighbourhood xs f = A.listArray bounds (map f' indices)
  where
    f' index = f (xs A.! index) (neighbours xs index)
    bounds = A.bounds xs
    indices = A.range bounds

neighbours :: A.Array (Int, Int) a -> (Int, Int) -> [a]
neighbours xs (x, y) = map (xs A.!) validNeighbours
  where
    bounds = A.bounds xs
    validNeighbours = filter (A.inRange bounds)
        [ (x - 1, y - 1), (x, y - 1), (x + 1, y - 1)
        , (x - 1, y),                 (x + 1, y)
        , (x - 1, y + 1), (x, y + 1), (x + 1, y + 1)
        ]

showLand :: Land -> String
showLand (Land array) = L.intercalate "\n" . L.chunksOf (cols + 1) $ characters
  where
    characters = map toChar . A.elems $ array
    (_, (cols, _)) = A.bounds array
    toChar Forest = '|'
    toChar Lumberyard = '#'
    toChar Open = '.'

parseInput :: T.Text -> Either P.ParseError Land
parseInput = P.parse (parseLand <* P.eof) ""

parseLand :: P.Parsec T.Text () Land
parseLand = do
    rows <- parseRow `P.endBy` P.char '\n'

    let rowN = length rows
        colN = M.fromMaybe 0 . fmap length . headMay $ rows
        bounds = ((0, 0), (colN - 1, rowN - 1))
        elements = concat rows
        array = A.listArray bounds elements

    return $! Land array

parseRow :: P.Parsec T.Text () [AcreType]
parseRow = P.many parseTile

parseTile :: P.Parsec T.Text () AcreType
parseTile = do
    c <- P.satisfy $ \x -> x `elem` validChar

    case c of
        '.' -> return Open
        '#' -> return Lumberyard
        '|' -> return Forest
        _ -> error "Should not be possible."
  where
    validChar = ".#|"

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (x:_) = Just x

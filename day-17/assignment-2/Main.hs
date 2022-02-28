{- --- Part Two ---
 -
 - After a very long time, the water spring will run dry. How much water will be
 - retained?
 -
 - In the example above, water that won't eventually drain out is shown as ~, a
 - total of 29 tiles.
 -
 - How many water tiles are left after the water spring stops producing water
 - and all remaining water not at rest has drained?
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
    . filter (== Still)
    . Map.elems
    . simulate (Position 500 0)

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

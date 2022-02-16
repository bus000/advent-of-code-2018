{- According to your calculations, the Elves are going to lose badly. Surely,
 - you won't mess up the timeline too much if you give them just a little
 - advanced technology, right?
 -
 - You need to make sure the Elves not only win, but also suffer no losses: even
 - the death of a single Elf is unacceptable.
 -
 - However, you can't go too far: larger changes will be more likely to
 - permanently alter spacetime.
 -
 - So, you need to find the outcome of the battle in which the Elves have the
 - lowest integer attack power (at least 4) that allows them to win without a
 - single death. The Goblins always have an attack power of 3.
 -
 - In the first summarized example above, the lowest attack power the Elves need
 - to win without losses is 15:
 -
 -    #######       #######
 -    #.G...#       #..E..#   E(158)
 -    #...EG#       #...E.#   E(14)
 -    #.#.#G#  -->  #.#.#.#
 -    #..G#E#       #...#.#
 -    #.....#       #.....#
 -    #######       #######
 -
 - Combat ends after 29 full rounds
 - Elves win with 172 total hit points left
 - Outcome: 29 * 172 = 4988
 -
 - In the second example above, the Elves need only 4 attack power:
 -
 -    #######       #######
 -    #E..EG#       #.E.E.#   E(200), E(23)
 -    #.#G.E#       #.#E..#   E(200)
 -    #E.##E#  -->  #E.##E#   E(125), E(200)
 -    #G..#.#       #.E.#.#   E(200)
 -    #..E#.#       #...#.#
 -    #######       #######
 -
 - Combat ends after 33 full rounds
 - Elves win with 948 total hit points left
 - Outcome: 33 * 948 = 31284
 -
 - In the third example above, the Elves need 15 attack power:
 -
 -    #######       #######
 -    #E.G#.#       #.E.#.#   E(8)
 -    #.#G..#       #.#E..#   E(86)
 -    #G.#.G#  -->  #..#..#
 -    #G..#.#       #...#.#
 -    #...E.#       #.....#
 -    #######       #######
 -
 - Combat ends after 37 full rounds
 - Elves win with 94 total hit points left
 - Outcome: 37 * 94 = 3478
 -
 - In the fourth example above, the Elves need 12 attack power:
 -
 -    #######       #######
 -    #.E...#       #...E.#   E(14)
 -    #.#..G#       #.#..E#   E(152)
 -    #.###.#  -->  #.###.#
 -    #E#G#G#       #.#.#.#
 -    #...#G#       #...#.#
 -    #######       #######
 -
 - Combat ends after 39 full rounds
 - Elves win with 166 total hit points left
 - Outcome: 39 * 166 = 6474
 -
 - In the last example above, the lone Elf needs 34 attack power:
 -
 -    #########       #########
 -    #G......#       #.......#
 -    #.E.#...#       #.E.#...#   E(38)
 -    #..##..G#       #..##...#
 -    #...##..#  -->  #...##..#
 -    #...#...#       #...#...#
 -    #.G...G.#       #.......#
 -    #.....G.#       #.......#
 -    #########       #########
 -
 - Combat ends after 30 full rounds
 - Elves win with 38 total hit points left
 - Outcome: 30 * 38 = 1140
 -
 - After increasing the Elves' attack power until it is just barely enough for
 - them to win without any Elves dying, what is the outcome of the combat
 - described in your puzzle input?
 -}
module Main where

import AdventOfCode
import qualified Text.Parsec as P
import qualified Control.Monad.State.Lazy as M
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as L
import qualified Data.Maybe as Maybe
import qualified Data.PSQueue as PQ
import Data.PSQueue (Binding((:->)))

main :: IO ()
main = preludeMain parseInput handleInput

type Position = (Int, Int)

data NPCType = Elf | Goblin deriving (Show, Eq, Ord)

data NPC = NPC
    { _type     :: !NPCType
    , _hp       :: !Int
    , _position :: !Position
    , _hasMoved :: !Bool
    , _power    :: !Int
    } deriving (Show, Eq, Ord)

data Arena = Arena
    { _npcs  :: !(Map.Map Position NPC)
    , _walls :: !(Set.Set Position)
    } deriving (Show, Eq, Ord)

data Combat = Combat
    { _arena :: !Arena
    , _round :: !Int
    } deriving (Show, Eq, Ord)

handleInput :: Arena -> IO ()
handleInput arena
    = print
    . evaluate
    . head
    . filter (elfsSurvived elfCount)
    . map (\power -> simulate arena power)
    $ [0..]
  where
    elfCount = countNpcs Elf arena

elfsSurvived :: Int -> Combat -> Bool
elfsSurvived elfCount combat = (== elfCount) . countNpcs Elf . _arena $ combat

countNpcs :: NPCType -> Arena -> Int
countNpcs nType arena
    = length
    . filter (\npc -> _type npc == nType)
    . Map.elems
    . _npcs
    $ arena

-- Technically its not always -1 but it almost always is.
evaluate :: Combat -> Int
evaluate (Combat arena c) = (c - 1) * hitpoints
  where
    hitpoints = sum . map _hp . Map.elems . _npcs $ arena

simulate :: Arena -> Int -> Combat
simulate arena power = go $ Combat (arena { _npcs = npcs' }) 0
  where
    npcs' = Map.map (\npc -> if _type npc == Elf
        then npc { _power = power }
        else npc) $ _npcs arena

    go combat
        | isFinished combat = combat
        | otherwise = go $ step combat

    isFinished
        = (== 1)
        . Set.size
        . Set.fromList
        . map _type
        . Map.elems
        . _npcs
        . _arena

step :: Combat -> Combat
step (Combat arena combatRound) = Combat arena' combatRound'
  where
    arena' = stepUnits arena
    combatRound' = combatRound + 1

stepUnits :: Arena -> Arena
stepUnits = resetMoved . go (-1, -1)
  where
    go prevPos arena@(Arena npcs _) = case Map.lookupGT prevPos npcs of
        Just (pos, npc) -> go pos $ stepUnit arena npc
        Nothing -> arena

    resetMoved arena@(Arena npcs _) = arena { _npcs = Map.map notMoved npcs }
    notMoved npc = npc { _hasMoved = False }

stepUnit :: Arena -> NPC -> Arena
stepUnit arena npc
    | _hasMoved npc = arena
    | otherwise = arena''
  where
    (arena', npc') = performMove arena npc
    arena'' = performAttack arena' npc'

performMove :: Arena -> NPC -> (Arena, NPC)
performMove arena npc@(NPC nType hp pos _ power) = case path of
    (firstStep:_) -> moveNpc arena npc firstStep
    [] -> (arena, NPC nType hp pos True power)
  where
    distances = distancesFrom pos arena
    target
        = Maybe.listToMaybe
        . map fst
        . L.sortOn snd
        . filter (isNextToEnemy . fst)
        $ distances
    path = maybe [] (pathTo (Map.fromList distances)) target

    isNextToEnemy
        = any (\candidate -> _type candidate /= nType)
        . lookupNeighbours arena

pathTo :: Map.Map Position Int -> Position -> [Position]
pathTo distances target = case Map.lookup target distances of
    Just 0 -> [] -- Path to self.
    Just _ -> reverse $ go target
    Nothing -> error "Not gonna happen unless map is broken."
  where
    go pos = case minStep pos of
        Just (_, 0) -> [pos]
        Just (s, _) -> pos:go s
        Nothing -> error "Not gonna happen unless map is broken."

    minStep pos
        = Maybe.listToMaybe
        . L.sortOn snd
        . Maybe.mapMaybe
            (\n -> Map.lookup n distances >>= \dist -> return (n, dist))
        $ neighbours pos

distancesFrom :: Position -> Arena -> [(Position, Int)]
distancesFrom from arena = M.evalState go (initialQueue, Set.empty)
  where
    go :: M.State (PQ.PSQ Position Int, Set.Set Position) [(Position, Int)]
    go = M.gets fst >>= \queue -> case PQ.minView queue of
        Nothing -> pure []
        Just (point :-> price, queue') -> do
            M.modify $ \(_, seen) -> (queue', Set.insert point seen)
            updateNeighbours point price
            rest <- go
            return $ (point, price) : rest

    initialQueue :: PQ.PSQ Position Int
    initialQueue = PQ.singleton from 0

    updateNeighbours
        :: Position -> Int -> M.State (PQ.PSQ Position Int, Set.Set Position) ()
    updateNeighbours point price = do
        seen <- M.gets snd
        mapM_ (setMinPrice (succ price)) (reachableNeighbours seen point)

    reachableNeighbours :: Set.Set Position -> Position -> [Position]
    reachableNeighbours seen pos
        = filter (isFree arena)
        . filter (`Set.notMember` seen)
        . neighbours
        $ pos

    setMinPrice
        :: Int -> Position -> M.State (PQ.PSQ Position Int, Set.Set Position) ()
    setMinPrice price point = M.modify $ \(q, seen) ->
        (PQ.alter (Just . maybe price (min price)) point q, seen)

performAttack :: Arena -> NPC -> Arena
performAttack arena npc = case targets of
    (target:_) -> attackTarget target
    [] -> arena
  where
    targets = L.sortOn _hp . filter isEnemy . lookupNeighboursOf arena $ npc
    isEnemy candidate = _type npc /= _type candidate
    attackTarget target
        | _hp target <= _power npc = removeNpc arena target
        | otherwise = updateNpc arena (target { _hp = _hp target - _power npc})

lookupNeighboursOf :: Arena -> NPC -> [NPC]
lookupNeighboursOf arena npc = lookupNeighbours arena (_position npc)

lookupNeighbours :: Arena -> Position -> [NPC]
lookupNeighbours arena pos
    = Maybe.mapMaybe (lookupNpc arena)
    $ neighbours pos

lookupNpc :: Arena -> Position -> Maybe NPC
lookupNpc (Arena npcs _) pos = Map.lookup pos npcs

updateNpc :: Arena -> NPC -> Arena
updateNpc (Arena npcs walls) (NPC nType hp pos moved power) = Arena npcs' walls
  where
    npcs' = Map.insert pos (NPC nType hp pos moved power) npcs

removeNpc :: Arena -> NPC -> Arena
removeNpc (Arena npcs walls) (NPC _ _ pos _ _) = Arena npcs' walls
  where
    npcs' = Map.delete pos npcs

moveNpc :: Arena -> NPC -> Position -> (Arena, NPC)
moveNpc arena npc@(NPC nType hp pos _ power) pos' =
    (arena { _npcs = npcs' }, npc')
  where
    npcs'
        = Map.insert pos' (NPC nType hp pos' True power)
        . Map.delete pos
        . _npcs
        $ arena
    npc' = npc { _position = pos', _hasMoved = True }

isFree :: Arena -> Position -> Bool
isFree (Arena npcs walls) pos =
    Map.notMember pos npcs && Set.notMember pos walls

neighbours :: Position -> [Position]
neighbours (row, col) =
    [                 (row - 1, col)
    , (row, col - 1),                (row, col + 1)
    ,                 (row + 1, col) 
    ]

showArena :: Arena -> String
showArena (Arena npcs walls) = concat $ do
    y <- [minY..maxY]
    x <- [minX..maxX]
    let c = getCharAt y x
    if x == maxX
        then return $ [c] ++ hpRow y ++ "\n"
        else return [c]
  where
    minX = minimum . map snd . Set.toList $ walls
    minY = minimum . map fst . Set.toList $ walls
    maxX = maximum . map snd . Set.toList $ walls
    maxY = maximum . map fst . Set.toList $ walls

    getCharAt x y
        | (x, y) `Set.member` walls = '#'
        | otherwise = case Map.lookup (x, y) npcs of
            Just (NPC Elf _ _ _ _) -> 'E'
            Just (NPC Goblin _ _ _ _) -> 'G'
            Nothing -> '.'

    hpRow row = concat $ do
        x <- [minX..maxX]
        case Map.lookup (row, x) npcs of
            Just (NPC _ hp _ _ _) -> return $ ' ':show hp
            Nothing -> return ""

parseInput :: T.Text -> Either P.ParseError Arena
parseInput = P.parse (parseArena <* P.eof) ""

data GameEntity = Entity NPCType | Wall | Empty

parseArena :: P.Parsec T.Text () Arena
parseArena = arenaFromRows <$> parseRow `P.endBy` P.newline

parseRow :: P.Parsec T.Text () [GameEntity]
parseRow = P.many1 $ P.choice
    [ P.char '#' *> pure Wall
    , P.char '.' *> pure Empty
    , P.char 'G' *> pure (Entity Goblin)
    , P.char 'E' *> pure (Entity Elf)
    ]

arenaFromRows :: [[GameEntity]] -> Arena
arenaFromRows rows = Arena npcs walls
  where
    npcs = Map.fromList $ do
        (rowN, row) <- zip [0..] rows
        (colN, Entity eType) <- zip [0..] row

        let npc = NPC eType 200 (rowN, colN) False 3

        return ((rowN, colN), npc)
    walls = Set.fromList $ do
        (rowN, row) <- zip [0..] rows
        (colN, Wall) <- zip [0..] row

        return (rowN, colN)

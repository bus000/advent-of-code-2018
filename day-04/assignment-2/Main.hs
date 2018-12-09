{- Strategy 2: Of all guards, which guard is most frequently asleep on the same
 - minute?
 -
 - In the example above, Guard #99 spent minute 45 asleep more than any other
 - guard or minute - three times in total. (In all other cases, any guard spent
 - any minute asleep at most twice.)
 -
 - What is the ID of the guard you chose multiplied by the minute you chose? (In
 - the above example, the answer would be 99 * 45 = 4455.)
 -}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import AdventOfCode
import ClassyPrelude hiding (Day, guard)
import qualified Data.List as L
import qualified Data.Map as Map
import qualified Prelude
import qualified Text.Parsec as P
import qualified Text.Parsec.Number as P

main :: IO ()
main = defaultMain parseInput handleInput

data Event = Event
    { _time   :: Time
    , _action :: Action
    }
  deriving (Show)

data Time = Time Year Month Day Hour Minute deriving (Eq, Ord, Show)

data Action
    = BeginShift GuardId
    | FallAsleep
    | Wakeup
  deriving (Show)

type Events = [Event]
type GuardId = Int
type Year = Int
type Month = Int
type Day = Int
type Hour = Int
type Minute = Int

data GuardEvent = Wake GuardId Time | Sleep GuardId Time
  deriving (Eq, Ord, Show)

handleInput :: MonadIO m => Events -> m ()
handleInput events = print $ fst best * (Prelude.head . snd $ best)
  where
    best = L.maximumBy (comparing $ length . snd) groupedSleepMinutes

    groupedSleepMinutes = concatMap (\(guard, minutes) ->
        map (\x -> (guard, x)) . group . sort $ minutes) sleepMinutes

    sleepMinutes
        = Map.toList
        . foldr (uncurry $ Map.insertWith (++)) Map.empty
        . toMinuteLists
        . toGuardEvents
        $ events

    toMinuteLists (Sleep g1 t1 : Wake g2 t2 : rest)
        | g1 == g2 && t2 > t1 = (g1, minuteList t1 t2) : toMinuteLists rest
    toMinuteLists [] = []
    toMinuteLists _ = error "Unexpected ordering"

    minuteList (Time _ _ _ _ m1) (Time _ _ _ _ m2)
        | m1 > m2 = [m2..m1-1]
        | m2 > m1 = [m1..m2-1]
        | otherwise = []

toGuardEvents :: Events -> [GuardEvent]
toGuardEvents
    = catMaybes
    . snd
    . L.mapAccumL accum (-1)
    . L.sortOn _time
  where
    accum guard (Event time action) = case action of
        BeginShift guard' -> (guard', Nothing)
        FallAsleep -> (guard, Just $ Sleep guard time)
        Wakeup -> (guard, Just $ Wake guard time)

parseInput :: LText -> Either P.ParseError Events
parseInput = P.parse (P.many parseEvent <* P.eof) ""

parseEvent :: P.Parsec LText () Event
parseEvent = Event <$> myParseTime <* P.char ' ' <*> parseAction <* P.char '\n'

myParseTime :: P.Parsec LText () Time
myParseTime = P.between (P.char '[') (P.char ']') $ Time
    <$> P.int <* P.char '-'
    <*> P.int <* P.char '-'
    <*> P.int <* P.char ' '
    <*> P.int <* P.char ':'
    <*> P.int

parseAction :: P.Parsec LText () Action
parseAction = parseBeginShift <|> parseFallAsleep <|> parseWakeup

parseBeginShift :: P.Parsec LText () Action
parseBeginShift = BeginShift <$> (P.string "Guard #" *> P.int <* P.string " begins shift")

parseFallAsleep :: P.Parsec LText () Action
parseFallAsleep = P.string "falls asleep" *> pure FallAsleep

parseWakeup :: P.Parsec LText () Action
parseWakeup = P.string "wakes up" *> pure Wakeup

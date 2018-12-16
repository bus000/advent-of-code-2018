{- As you're about to begin construction, four of the Elves offer to help. "The
 - sun will set soon; it'll go faster if we work together." Now, you need to
 - account for multiple people working on steps simultaneously. If multiple
 - steps are available, workers should still begin them in alphabetical order.
 -
 - Each step takes 60 seconds plus an amount corresponding to its letter: A=1,
 - B=2, C=3, and so on. So, step A takes 60+1=61 seconds, while step Z takes
 - 60+26=86 seconds. No time is required between steps.
 -
 - To simplify things for the example, however, suppose you only have help from
 - one Elf (a total of two workers) and that each step takes 60 fewer seconds
 - (so that step A takes 1 second and step Z takes 26 seconds). Then, using the
 - same instructions as above, this is how each second would be spent:

 - Second   Worker 1   Worker 2   Done
 -    0        C          .
 -    1        C          .
 -    2        C          .
 -    3        A          F       C
 -    4        B          F       CA
 -    5        B          F       CA
 -    6        D          F       CAB
 -    7        D          F       CAB
 -    8        D          F       CAB
 -    9        D          .       CABF
 -   10        E          .       CABFD
 -   11        E          .       CABFD
 -   12        E          .       CABFD
 -   13        E          .       CABFD
 -   14        E          .       CABFD
 -   15        .          .       CABFDE
 -
 - Each row represents one second of time. The Second column identifies how many
 - seconds have passed as of the beginning of that second. Each worker column
 - shows the step that worker is currently doing (or . if they are idle). The
 - Done column shows completed steps.
 -
 - Note that the order of the steps has changed; this is because steps now take
 - time to finish and multiple workers can begin multiple steps simultaneously.
 -
 - In this example, it would take 15 seconds for two workers to complete these
 - steps.
 -
 - With 5 workers and the 60+ second step durations described above, how long
 - will it take to complete all of the steps?
 -}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts  #-}
module Main where

import AdventOfCode
import ClassyPrelude
import qualified Control.Monad.State as M
import qualified Control.Monad.Util as M
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Text.Parsec as P

main :: IO ()
main = defaultMain parseInput handleInput

data State a = State
    { _todo     :: [Action a]
    , _finished :: Set.Set a
    , _workers  :: [Worker a]
    }
  deriving (Show, Eq)

data Action a = Action
    { _actionRef    :: a
    , _dependencies :: Set.Set a
    , _cost         :: Int
    }
  deriving (Show, Eq)

data Rule a = Rule
    { _ref       :: a
    , _dependsOn :: a
    }
  deriving (Show, Eq)

data Worker a = Idle | Working a Int deriving (Show, Eq)

type ActionRef = Char

handleInput :: MonadIO m => NonNull [Rule Char] -> m ()
handleInput =
    print . subtract 1 . runSimulation 5 . toNullable . constructActions

runSimulation :: Ord a => Int -> [Action a] -> Int
runSimulation workers actions = M.evalState countSteps initialState
  where
    initialState = State actions Set.empty (replicate workers Idle)

countSteps :: (M.MonadState (State a) m, Ord a) => m Int
countSteps = M.countWhile (not <$> isFinished) step

isFinished :: (M.MonadState (State a) m, Ord a) => m Bool
isFinished = do
    State todo _ workers <- M.get
    return $ null todo && allIdle workers

step :: (M.MonadState (State a) m, Ord a) => m ()
step = advanceWorkers >> startIdle

advanceWorkers :: (M.MonadState (State a) m, Ord a) => m ()
advanceWorkers = do
    State todo finished workers <- M.get

    let timeAdvanced = map advanceTime workers
        finishedWorkers = filter workerIsFinished timeAdvanced
        notFinishedWorkers = filter (not . workerIsFinished) timeAdvanced
        finishedTasks = mapMaybe getTask finishedWorkers

        finished' = foldr Set.insert finished finishedTasks
        workers' = map toIdle finishedWorkers ++ notFinishedWorkers

    M.put $ State todo finished' workers'

startIdle :: (M.MonadState (State a) m, Ord a) => m ()
startIdle = do
    State todo finished workers <- M.get

    let (ready, nonReady) = partition (`isReady` finished) todo
        (idle, working) = partition workerIsFinished workers
        assignments = zip idle ready
        started = map (\(_, Action ref _ cost) -> Working ref cost) assignments
        startedN = length started

        todo' = drop startedN ready ++ nonReady
        workers' = drop startedN idle ++ working ++ started

    M.put $ State todo' finished workers'

isReady :: Ord a => Action a -> Set a -> Bool
isReady (Action _ dependencies _) done = dependencies `Set.isSubsetOf` done

allIdle :: Eq a => [Worker a] -> Bool
allIdle = all (== Idle)

advanceTime :: Worker a -> Worker a
advanceTime (Working ref time) = Working ref (time - 1)
advanceTime Idle = Idle

workerIsFinished :: Worker a -> Bool
workerIsFinished Idle = True
workerIsFinished (Working _ 0) = True
workerIsFinished _ = False

getTask :: Worker a -> Maybe a
getTask (Working task _) = Just task
getTask _ = Nothing

toIdle :: Worker a -> Worker a
toIdle _ = Idle

constructActions :: NonNull [Rule Char] -> NonNull [Action Char]
constructActions rules
    = impureNonNull
    . map (\ref -> Action ref
        (fromMaybe Set.empty $ Map.lookup ref dependencies) (charCost ref))
    . Set.toList
    $ actionRefs
  where
    actionRefs = Set.fromList . concatMap (\(Rule x y) -> [x, y]) $ rules
    dependencies = foldr addDependency Map.empty rules
    addDependency rule =
        Map.insertWith Set.union (_ref rule) (Set.singleton $ _dependsOn rule)

charCost :: Char -> Int
charCost 'A' = 61
charCost x = charCost (pred x) + 1

parseInput :: LText -> Either P.ParseError (NonNull [Rule Char])
parseInput = P.parse (parseRules <* P.eof) ""

parseRules :: P.Parsec LText () (NonNull [Rule Char])
parseRules = impureNonNull <$> P.many1 parseRule

parseRule :: P.Parsec LText () (Rule Char)
parseRule = do
    void $ P.string "Step "
    depend <- P.anyChar
    void $ P.string " must be finished before step "
    ref <- P.anyChar
    void $ P.string " can begin.\n"

    return Rule {_ref = ref, _dependsOn = depend }

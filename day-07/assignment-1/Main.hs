{- --- Day 7: The Sum of Its Parts ---
 -
 - You find yourself standing on a snow-covered coastline; apparently, you
 - landed a little off course. The region is too hilly to see the North Pole
 - from here, but you do spot some Elves that seem to be trying to unpack
 - something that washed ashore. It's quite cold out, so you decide to risk
 - creating a paradox by asking them for directions.
 -
 - "Oh, are you the search party?" Somehow, you can understand whatever Elves
 - from the year 1018 speak; you assume it's Ancient Nordic Elvish. Could the
 - device on your wrist also be a translator? "Those clothes don't look very
 - warm; take this." They hand you a heavy coat.
 -
 - "We do need to find our way back to the North Pole, but we have higher
 - priorities at the moment. You see, believe it or not, this box contains
 - something that will solve all of Santa's transportation problems - at least,
 - that's what it looks like from the pictures in the instructions." It doesn't
 - seem like they can read whatever language it's in, but you can: "Sleigh kit.
 - Some assembly required."
 -
 - "'Sleigh'? What a wonderful name! You must help us assemble this 'sleigh' at
 - once!" They start excitedly pulling more parts out of the box.
 -
 - The instructions specify a series of steps and requirements about which steps
 - must be finished before others can begin (your puzzle input). Each step is
 - designated by a single letter. For example, suppose you have the following
 - instructions:
 -
 - * Step C must be finished before step A can begin.
 - * Step C must be finished before step F can begin.
 - * Step A must be finished before step B can begin.
 - * Step A must be finished before step D can begin.
 - * Step B must be finished before step E can begin.
 - * Step D must be finished before step E can begin.
 - * Step F must be finished before step E can begin.
 - * Visually, these requirements look like this:
 -
 -   -->A--->B--
 -  /    \      \
 - C      -->D----->E
 -  \           /
 -   ---->F-----
 -
 - Your first goal is to determine the order in which the steps should be
 - completed. If more than one step is ready, choose the step which is first
 - alphabetically. In this example, the steps would be completed as follows:
 -
 - * Only C is available, and so it is done first.
 - * Next, both A and F are available. A is first alphabetically, so it is done
 -   next.
 - * Then, even though F was available earlier, steps B and D are now also
 -   available, and B is the first alphabetically of the three.
 - * After that, only D and F are available. E is not available because only
 -   some of its prerequisites are complete. Therefore, D is completed next.
 - * F is the only choice, so it is done next.
 - * Finally, E is completed.
 -
 - So, in this example, the correct order is CABDFE.
 -
 - In what order should the steps in your instructions be completed?
 -}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts  #-}
module Main where

import AdventOfCode
import ClassyPrelude
import qualified Control.Monad.Loops as M
import qualified Control.Monad.State as M
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Text.Parsec as P

main :: IO ()
main = defaultMain parseInput handleInput

data State a = State
    { _notReady :: [Action a]
    , _ready    :: Set.Set a
    }
  deriving (Show, Eq)

data Action a = Action
    { _actionRef              :: a
    , _unfinishedDependencies :: Set.Set a
    }
  deriving (Show, Eq)

data Rule a = Rule
    { _ref       :: a
    , _dependsOn :: a
    }
  deriving (Show, Eq)

handleInput :: MonadIO m => NonNull [Rule Char] -> m ()
handleInput = putStrLn . pack . getActionOrder . toNullable . constructActions

getActionOrder :: (Ord a) => [Action a] -> [a]
getActionOrder actions = M.evalState runActions initialState
  where
    ready = map _actionRef . filter isReady $ actions
    notReady = filter (not . isReady) actions
    initialState = State notReady (Set.fromList ready)

runActions :: (M.MonadState (State a) m, Ord a) => m [a]
runActions = M.whileM hasReady runNextAction

runNextAction :: (M.MonadState (State a) m, Ord a) => m a
runNextAction = do
    next <- popNext
    removeDependencyOn next
    return next

hasReady :: (M.MonadState (State a) m, Ord a) => m Bool
hasReady = M.gets (not . Set.null . _ready)

removeDependencyOn :: (M.MonadState (State a) m, Ord a) => a -> m ()
removeDependencyOn dependency = do
    State notReady ready <- M.get

    let filtered = map (removeActionDependency dependency) notReady
        ready' = foldr (Set.insert . _actionRef) ready . filter isReady $ filtered
        notReady' = filter (not . isReady) filtered
        state' = State notReady' ready'

    M.put state'

removeActionDependency :: Ord a => a -> Action a -> Action a
removeActionDependency toRemove (Action ref ud) = Action ref ud'
  where
    ud' = Set.delete toRemove ud

popNext :: (M.MonadState (State a) m, Ord a) => m a
popNext = do
    State notReady ready <- M.get
    let (next, ready') = Set.deleteFindMin ready
    M.put $ State notReady ready'

    return next

isReady :: Action a -> Bool
isReady (Action _ dependencies) = Set.null dependencies

constructActions :: Ord a => NonNull [Rule a] -> NonNull [Action a]
constructActions rules
    = impureNonNull
    . map (\ref -> Action ref (fromMaybe Set.empty $ Map.lookup ref dependencies))
    . Set.toList
    $ actionRefs
  where
    actionRefs = Set.fromList . concatMap (\(Rule x y) -> [x, y]) $ rules
    dependencies = foldr addDependency Map.empty rules
    addDependency rule =
        Map.insertWith Set.union (_ref rule) (Set.singleton $ _dependsOn rule)

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

    return Rule {_ref = ref, _dependsOn = depend}

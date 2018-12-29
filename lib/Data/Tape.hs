{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveFunctor     #-}
module Data.Tape
    ( Tape

    -- Construction.
    , generate
    , fromLists
    , singleton

    -- Queries.
    , read
    , take
    , takeRight
    , takeLeft
    , takeWhile
    , takeWhileRight
    , takeWhileLeft

    -- Movement.
    , right
    , left

    -- Modification.
    , write
    , modify
    , mapNeighbourhood

    -- Reduction.
    , reduce
    , reduceBy
    , reduceM
    , reduceByM
    ) where

import ClassyPrelude hiding (takeWhile, take, singleton, lefts, rights)
import qualified ClassyPrelude as CP
import Data.List.NonEmpty (NonEmpty(..))

data Tape a = Tape [a] a [a] deriving (Show, Eq, Ord, Functor)

generate :: (a -> Maybe a) -> (a -> Maybe a) -> a -> Tape a
generate itLeft itRight base = Tape lefts base rights
  where
    lefts = generateList itLeft base
    rights = generateList itRight base

mapNeighbourhood :: ([a] -> a -> [a] -> a) -> Tape a -> Tape a
mapNeighbourhood f (Tape lefts x rights) = Tape lefts' x' rights'
  where
    x' = f lefts x rights
    lefts' = mapNeighbourhoodLeft f lefts (x:rights)
    rights' = mapNeighbourhoodRight f (x:lefts) rights

mapNeighbourhoodLeft :: ([a] -> a -> [a] -> a) -> [a] -> [a] -> [a]
mapNeighbourhoodLeft _ [] _ = []
mapNeighbourhoodLeft f (l:lefts) rights =
    f lefts l rights : mapNeighbourhoodLeft f lefts (l:rights)

mapNeighbourhoodRight :: ([a] -> a -> [a] -> a) -> [a] -> [a] -> [a]
mapNeighbourhoodRight _ _ [] = []
mapNeighbourhoodRight f lefts (r:rights) =
    f lefts r rights : mapNeighbourhoodRight f (r:lefts) rights

fromLists :: [a] -> [a] -> a -> Tape a
fromLists lefts rights base = Tape lefts base rights

singleton :: a -> Tape a
singleton base = Tape [] base []

read :: Tape a -> a
read (Tape _ x _) = x

write :: a -> Tape a -> Tape a
write x (Tape lefts _ rights) = Tape lefts x rights

modify :: (a -> a) -> Tape a -> Tape a
modify f (Tape lefts x rights) = Tape lefts (f x) rights

right :: Tape a -> Tape a
right (Tape lefts x (r:rights)) = Tape (x:lefts) r rights
right tape@(Tape _ _ []) = tape

left :: Tape a -> Tape a
left (Tape (l:lefts) x rights) = Tape lefts l (x:rights)
left tape@(Tape [] _ _) = tape

reduce :: Semigroup a => Tape a -> a
reduce (Tape lefts x rights) = sconcat $ x :| lefts ++ rights

reduceBy :: Semigroup b => Tape a -> (a -> b) -> b
reduceBy tape f = reduce . fmap f $ tape

reduceM :: (Monoid a, Semigroup a) => Tape a -> a
reduceM (Tape lefts x rights) = mconcat lefts <> x <> mconcat rights

reduceByM :: (Monoid b, Semigroup b) => Tape a -> (a -> b) -> b
reduceByM tape f = reduceM . fmap f $ tape

take :: Int -> Tape a -> ([a], [a])
take n tape = (takeLeft n tape, takeRight n tape)

takeRight :: Int -> Tape a -> [a]
takeRight n (Tape _ _ rights) = CP.take n rights

takeLeft :: Int -> Tape a -> [a]
takeLeft n (Tape lefts _ _) = CP.take n lefts

takeWhile :: (a -> Bool) -> Tape a -> ([a], [a])
takeWhile f tape = (takeWhileLeft f tape, takeWhileRight f tape)

takeWhileRight :: (a -> Bool) -> Tape a -> [a]
takeWhileRight f (Tape _ _ rights) = CP.takeWhile f rights

takeWhileLeft :: (a -> Bool) -> Tape a -> [a]
takeWhileLeft f (Tape lefts _ _) = CP.takeWhile f lefts

generateList :: (a -> Maybe a) -> a -> [a]
generateList f x = case f x of
    Just x' -> x:generateList f x'
    Nothing -> [x]

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Counter
    ( Counter

    -- Constructors.
    , singleton
    , fromList

    -- Modification.
    , insert

    -- Lookup.
    , lookup
    , maximum
    , maximumKey
    , maximumCount
    , minimum
    , minimumKey
    , minimumCount
    ) where

import ClassyPrelude hiding (singleton, minimum, maximum, fromList, lookup)
import qualified Data.Map as Map

newtype Counter k = Counter (Map k Int) deriving (Show, Eq)

singleton :: Ord k => k -> Counter k
singleton = Counter . flip Map.singleton 1

fromList :: Ord k => NonNull [k] -> Counter k
fromList ks = foldr (flip insert) (singleton . head $ ks) $ tail ks

insert :: Ord k => Counter k -> k -> Counter k
insert (Counter c) k = Counter $ Map.insertWith (+) k 1 c

lookup :: Ord k => Counter k -> k -> Maybe Int
lookup (Counter c) k = Map.lookup k c

maximum :: Ord k => Counter k -> (k, Int)
maximum (Counter c) = foldr chooseMax (head pairs) (tail pairs)
  where
    pairs = impureNonNull . Map.toList $ c
    chooseMax (k1, c1) (k2, c2) = if c1 > c2 then (k1, c1) else (k2, c2)

maximumCount :: Ord k => Counter k -> Int
maximumCount = snd . maximum

maximumKey :: Ord k => Counter k -> k
maximumKey = fst . maximum

minimum :: Ord k => Counter k -> (k, Int)
minimum (Counter c) = foldr chooseMin (head pairs) (tail pairs)
  where
    pairs = impureNonNull . Map.toList $ c
    chooseMin (k1, c1) (k2, c2) = if c1 < c2 then (k1, c1) else (k2, c2)

minimumCount :: Ord k => Counter k -> Int
minimumCount = snd . minimum

minimumKey :: Ord k => Counter k -> k
minimumKey = fst . minimum

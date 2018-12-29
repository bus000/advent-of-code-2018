{-# LANGUAGE NoImplicitPrelude #-}
module Data.CircularList
    ( CircularList

    -- Constructors.
    , empty
    , singleton
    , fromList

    -- Accessors.
    , current
    , takeRightN
    , takeLeftN


    -- Deletion.
    , removeRight
    , removeLeft

    -- Movement.
    , moveRight
    , moveLeft
    , moveRightN
    , moveLeftN

    -- Insertion.
    , insertRight
    , insertLeft
    , insertRightN
    , insertLeftN

    -- DeconStruction.
    , toList
    , toListRight
    , toListLeft
    ) where

import ClassyPrelude hiding (empty, singleton, fromList, rights, lefts, toList)
import qualified Data.Foldable as Fold

data CircularList a
    = Circ [a] a [a]
    | Empty
  deriving (Show, Read, Eq, Ord)

instance Functor CircularList where
    -- fmap :: (a -> b) -> CircularList a -> CircularList b.
    fmap _ Empty = Empty
    fmap f (Circ lefts x rights) = Circ (fmap f lefts) (f x) (fmap f rights)

instance Foldable CircularList where
    -- foldMap :: Monoid m => (a -> m) -> CircularList a -> m.
    foldMap f = foldMap f . toList

    -- foldr :: (a -> b -> b) -> b -> CircularList a -> b.
    foldr f e = foldr f e . toList

instance Semigroup (CircularList a) where
    -- (<>) :: CircularList a -> CircularList a -> CircularList a
    c1 <> c2 = insertRightN c1 (toList c2)

instance Monoid (CircularList a) where
    -- mempty :: CircularList a
    mempty = Empty

empty :: CircularList a
empty = Empty

singleton :: a -> CircularList a
singleton x = Circ [] x []

fromList :: [a] -> CircularList a
fromList [] = Empty
fromList (x:xs) = Circ [] x xs

current :: CircularList a -> Maybe a
current Empty = Nothing
current (Circ _ x _) = Just x

removeRight :: CircularList a -> CircularList a
removeRight Empty = Empty
removeRight (Circ [] _ []) = Empty
removeRight (Circ lefts _ (x:rights)) = Circ lefts x rights
removeRight (Circ lefts _ []) = Circ [] x rights
  where
    (x:rights) = reverse lefts

removeLeft :: CircularList a -> CircularList a
removeLeft Empty = Empty
removeLeft (Circ [] _ []) = Empty
removeLeft (Circ (x:lefts) _ rights) = Circ lefts x rights
removeLeft (Circ [] _ rights) = Circ lefts x []
  where
    (x:lefts) = reverse rights

moveRight :: CircularList a -> CircularList a
moveRight Empty = Empty
moveRight (Circ [] x []) = Circ [] x []
moveRight (Circ lefts x (x':rights)) = Circ (x:lefts) x' rights
moveRight (Circ lefts x []) = Circ [x] x' rights
  where
    (x':rights) = reverse lefts

moveRightN :: CircularList a -> Int -> CircularList a
moveRightN = applyN moveRight

moveLeft :: CircularList a -> CircularList a
moveLeft Empty = Empty
moveLeft (Circ [] x []) = Circ [] x []
moveLeft (Circ (x':lefts) x rights) = Circ lefts x' (x:rights)
moveLeft (Circ [] x rights) = Circ lefts x' [x]
  where
    (x':lefts) = reverse rights

moveLeftN :: CircularList a -> Int -> CircularList a
moveLeftN = applyN moveLeft

insertRight :: a -> CircularList a -> CircularList a
insertRight x Empty = Circ [] x []
insertRight x2 (Circ lefts x rights) = Circ lefts x2 (x:rights)

insertLeft :: a -> CircularList a -> CircularList a
insertLeft x Empty = Circ [] x []
insertLeft x2 (Circ lefts x rights) = Circ (x:lefts) x2 rights

toList :: CircularList a -> [a]
toList = toListRight

toListRight :: CircularList a -> [a]
toListRight Empty = []
toListRight (Circ lefts x rights) = (x:rights) ++ reverse lefts

toListLeft :: CircularList a -> [a]
toListLeft Empty = []
toListLeft (Circ lefts x rights) = (x:lefts) ++ reverse rights

insertRightN :: CircularList a -> [a] -> CircularList a
insertRightN = foldr insertRight

insertLeftN :: CircularList a -> [a] -> CircularList a
insertLeftN = foldr insertLeft

takeRightN :: Int -> CircularList a -> [a]
takeRightN _ Empty = []
takeRightN n (Circ _ _ rights) = take n rights

takeLeftN :: Int -> CircularList a -> [a]
takeLeftN _ Empty = []
takeLeftN n (Circ lefts _ _) = take n lefts

applyN :: (a -> a) -> a -> Int -> a
applyN _ x 0 = x
applyN f x n = applyN f (f x) (n - 1)

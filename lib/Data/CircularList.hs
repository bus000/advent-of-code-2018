{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.CircularList
    ( CircularList

    -- Constructors.
    , empty
    , singleton
    , fromList

    -- Accessors.
    , current


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
    ) where

import ClassyPrelude hiding (empty, singleton, fromList, rights, lefts)

data CircularList a
    = Circ [a] a [a]
    | Empty
  deriving (Show, Eq, Ord)

{-instance Functor a => Functor (CircularList a) where-}
instance Functor CircularList where
    fmap _ Empty = Empty
    fmap f (Circ lefts x rights) = Circ (fmap f lefts) (f x) (fmap f rights)

empty :: CircularList a
empty = Empty

singleton :: a -> CircularList a
singleton x = Circ [] x []

current :: CircularList a -> Maybe a
current Empty = Nothing
current (Circ _ x _) = Just x

fromList :: [a] -> CircularList a
fromList [] = Empty
fromList (x:xs) = Circ [] x xs

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
moveRightN circ n = applyN moveRight circ n

moveLeft :: CircularList a -> CircularList a
moveLeft Empty = Empty
moveLeft (Circ [] x []) = Circ [] x []
moveLeft (Circ (x':lefts) x rights) = Circ lefts x' (x:rights)
moveLeft (Circ [] x rights) = Circ lefts x' [x]
  where
    (x':lefts) = reverse rights

moveLeftN :: CircularList a -> Int -> CircularList a
moveLeftN circ n = applyN moveLeft circ n

insertRight :: a -> CircularList a -> CircularList a
insertRight x Empty = Circ [] x []
insertRight x2 (Circ lefts x rights) = Circ lefts x2 (x:rights)

insertLeft :: a -> CircularList a -> CircularList a
insertLeft x Empty = Circ [] x []
insertLeft x2 (Circ lefts x rights) = Circ (x:lefts) x2 rights

applyN :: (a -> a) -> a -> Int -> a
applyN f x 0 = x
applyN f x n = applyN f (f x) (n - 1)

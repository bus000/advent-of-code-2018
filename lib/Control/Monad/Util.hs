{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Control.Monad.Util
    ( countWhile
    ) where

import ClassyPrelude

countWhile :: Monad m => m Bool -> m a -> m Int
countWhile p a = countWhile' 0
  where
    countWhile' n = do
        test <- p
        if test
            then a >> countWhile' (n + 1)
            else return n

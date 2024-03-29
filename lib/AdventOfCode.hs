{-# LANGUAGE NoImplicitPrelude #-}

module AdventOfCode
    ( defaultMain
    , preludeMain
    , assoc
    , assocs
    ) where

import ClassyPrelude
import qualified System.Exit as Sys
import qualified Prelude
import qualified Data.Text as T
import qualified Data.Text.IO as T

{- | Default main method that can be used to solve the advent of code
 - assignments. The main method will handle reading input and exiting on parser
 - errors. -}
defaultMain
    :: (Show e, MonadIO m)
    => (LText -> Either e a)
    -- ^ The input parser.
    -> (a -> m ())
    -- ^ The action to perform on parsed input.
    -> m ()
defaultMain parseInput handleInput = do
    parsedInput <- parseInput <$> getContents

    case parsedInput of
        Left err -> liftIO $ Sys.die $ show err
        Right input -> handleInput input

preludeMain :: Show e => (T.Text -> Either e a) -> (a -> IO ()) -> IO ()
preludeMain parseInput handleInput = do
    parsedInput <- parseInput <$> T.getContents
    case parsedInput of
        Left err -> Sys.die $ show err
        Right input -> handleInput input

assoc :: (a -> b) -> a -> (a, b)
assoc f x = (x, f x)

assocs :: (a -> b) -> [a] -> [(a, b)]
assocs f = map (assoc f)

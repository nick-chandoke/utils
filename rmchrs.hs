#!/usr/bin/env stack
-- stack exec ghc --package rio --package containers --package bytestring -- -O2

{-# language BangPatterns, ViewPatterns, OverloadedStrings, LambdaCase, NoImplicitPrelude #-}

-- remove characters from filename
module Main where

import System.Environment (getArgs)
import RIO
import Data.Set (member, fromList)
import qualified Data.Text as T
import qualified Data.Text.IO as T

-- TODO: this pattern of accepting an argument either as a cmdline arg or stdin should have a function for creating such programs in a templated manner.
main :: IO ()
main = runSimpleApp $ liftIO getArgs >>= \case
    (chrs:xs) -> case xs of
        [s] -> rmchrs chrs (T.pack s)
        [] -> rmchrs chrs =<< liftIO T.getContents
        _ -> usage
    _ -> usage

rmchrs :: MonadIO m => String -> Text -> m ()
rmchrs (fromList -> !chrs) = liftIO . T.putStr . T.foldl' (\s' c -> if member c chrs then s' else T.snoc s' c) mempty
{-# INLINE rmchrs #-}

usage :: RIO SimpleApp ()
usage = logError "Usage: rmchrs <characters to remove> <string to process>\ne.g. rmchrs '()!' 'there are (problematic parens) here!'"
{-# INLINE usage #-}

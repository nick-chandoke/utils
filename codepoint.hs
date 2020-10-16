#!/usr/bin/env stack
-- stack exec ghc

{-# language TypeApplications, LambdaCase #-}
module Main where

import Numeric (showHex)
import Data.Char (ord, chr, isDigit)
import System.Environment (getArgs)

data Radix = Hex Char | Dec Char | Chr Int deriving Eq

instance Show Radix where
    show (Hex c) = "0x" ++ showHex (ord c) ""
    show (Dec c) = show $ ord c
    show (Chr i) = [chr i]

main :: IO ()
main = getArgs >>= putStrLn . \case
    [[c]] -> show $ (Hex c, Dec c)
    [z@(c:cs)] -> show $ Chr (read @Int z)
    _ -> "Invalid arguments. Please provide exactly one character or a number."

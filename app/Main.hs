#!/usr/bin/env stack
-- stack --install-ghc runghc

module Main where

import Prelude hiding (Left, Right)
import Match

main :: IO ()
main = putStrLn "foo"
module Main where

import           App.Server

main :: IO ()
main = run $ const mempty

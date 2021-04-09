module Main where

import Hangman (hangman)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  hangman

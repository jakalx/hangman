{-# LANGUAGE RecordWildCards #-}

module Hangman
   -- (hangman)
    where

import Data.List (intersperse, insert)
import Data.Maybe (fromMaybe, catMaybes)
import Data.Char (toLower)
import System.IO (hFlush, stdout)
import System.Random (randomRIO)

data GameState = GameState { gsSecret :: String,
  gsGuessed :: String,
  gsSolution :: [Maybe Char]
} deriving Show

newGame :: String -> GameState
newGame w = GameState { gsSecret = w, gsGuessed = [], gsSolution = map (const Nothing) w }

handleGuess :: Char -> Char -> Maybe Char -> Maybe Char
handleGuess guess secret prev
  | toLower guess == toLower secret = Just secret
  | otherwise = prev

move :: GameState -> Char -> GameState
move g@GameState{..} guess = g { gsGuessed = insert guess gsGuessed, gsSolution = newSolution }
  where newSolution = zipWith (handleGuess guess) gsSecret gsSolution

loadWords :: FilePath -> IO [String]
loadWords f = lines <$> readFile f

play :: GameState -> IO ()
play game@GameState{..} =
    if catMaybes gsSolution == gsSecret
      then putStrLn $ render game
      else do
        putStrLn $ render game
        putStr "enter guess: "
        hFlush stdout
        guess <- getChar
        putStrLn ""
        play $ move game guess

chooseRandomWord :: [String] -> IO (Maybe String)
chooseRandomWord [] = return Nothing
chooseRandomWord xs = do
  index <- randomRIO (0, length xs - 1)
  return $ Just (xs !! index)

hangman :: IO ()
hangman = do
    w <- loadWords "data/words.txt" >>= chooseRandomWord
    case w of
       Nothing -> return ()
       Just w -> play $ newGame w

render :: GameState -> String
render GameState{..} = intersperse ' ' w
  where w = map (fromMaybe '_') gsSolution

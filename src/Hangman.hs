{-# LANGUAGE RecordWildCards #-}

module Hangman (hangman) where

data Game = Game { secret :: String }

newGame :: String -> Game
newGame w = Game { secret = w }

move :: Game -> Char -> Game
move g@Game{..} guess = undefined


loadWords :: FilePath -> IO [String]
loadWords f = lines <$> readFile f

hangman :: IO ()
hangman = do
    words <- loadWords "data/words.txt"
    print words

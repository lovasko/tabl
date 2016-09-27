{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Data.List.Split
import Data.Word
import System.Random
import Text.Tabl

import qualified Data.Text.IO as Text

-- | Table containing the play grid of tic-tac-toe.
main :: IO ()
main = do
  fields <- replicateM 9 randomIO :: IO [Word8]
  let table = chunksOf 3 $ map (mark . (`mod` 3)) fields
  Text.putStrLn $ tabl EnvAscii DecorAll DecorAll (repeat AlignCentre) table
  where
    mark 0 = " "
    mark 1 = "X"
    mark 2 = "O"
    mark _ = "?"


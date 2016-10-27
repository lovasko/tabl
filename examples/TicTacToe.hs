{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Data.List.Split
import Data.Word
import Safe
import System.Random
import Text.Tabl

import qualified Data.Text.IO as T

-- | Table containing the play grid of tic-tac-toe.
main :: IO ()
main = do
  xs <- replicateM 9 randomIO :: IO [Word8]
  let cells = chunksOf 3 $ map (mark . (`mod` 3)) xs
  T.putStrLn $ tabl EnvAscii hdecor vdecor aligns cells
  where
    mark x = lookupJust x [(0, " "), (1, "X"), (2, "O")]
    hdecor = DecorAll
    vdecor = DecorAll
    aligns = []


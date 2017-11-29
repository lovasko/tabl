{-# LANGUAGE OverloadedStrings #-}

import Text.Tabl
import qualified Data.Text as T
import qualified Data.Text.IO as T

main :: IO ()
main = T.putStrLn $ tabl EnvAscii DecorOuter DecorAll aligns cells
  where
    aligns = [AlignRight, AlignText "."]
    cells  = zipWith (\x y -> [x, y]) xs ys
    xs     = map (T.pack . show) nums
    ys     = map (T.pack . take 5 . show . over . fromIntegral) nums
    nums   = [1..20] :: [Integer]
    over   = ((100.0 :: Double) /)

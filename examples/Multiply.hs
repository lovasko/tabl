{-# LANGUAGE OverloadedStrings #-}

import System.Environment
import Text.Tabl

import qualified Data.Text as T
import qualified Data.Text.IO as T

-- | Create the multiplication table.
numbers :: Int        -- ^ table side size
        -> [[T.Text]] -- ^ multiplication table
numbers n = header : zipWith (:) digits content
  where
    header  = " " : digits
    digits  = map (T.pack . show) [1..n]
    content = map (map (T.pack . show)) mults
    mults   = map (flip map [1..n] . (*)) [1..n]

-- | Table containing basic integer products.
main :: IO ()
main = do
  [n] <- getArgs
  let cells = numbers (read n)
  T.putStrLn $ tabl EnvAscii hdecor vdecor aligns cells
    where
      hdecor = DecorOnly [1]
      vdecor = DecorOnly [1]
      aligns = repeat AlignRight


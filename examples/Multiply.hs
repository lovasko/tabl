{-# LANGUAGE OverloadedStrings #-}

import System.Environment
import Text.Tabl

import qualified Data.Text as Text
import qualified Data.Text.IO as Text

-- | Create the multiplication table.
numbers :: Int           -- ^ table side size
        -> [[Text.Text]] -- ^ multiplication table 
numbers n = header : zipWith (:) digits content
  where
    header  = " " : digits
    digits  = map (Text.pack . show) [1..n]
    content = map (map (Text.pack . show)) mults
    mults   = map (flip map [1..n] . (*)) [1..n]

-- | Table containing basic integer products.
main :: IO ()
main = do
  [n] <- getArgs
  let hdecor = DecorOnly [1]
  let vdecor = DecorOnly [1]
  let aligns = repeat AlignRight
  Text.putStrLn $ tabl EnvAscii hdecor vdecor aligns (numbers (read n))


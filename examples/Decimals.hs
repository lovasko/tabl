{-# LANGUAGE OverloadedStrings #-}

import System.Environment
import Text.Tabl
import qualified Data.Text as T
import qualified Data.Text.IO as T

-- | Create the division table.
table
  :: Int    -- ^ number of rows
  -> T.Text -- ^ resulting table
table n = tabl EnvAscii hdecor vdecor aligns cells
  where
    hdecor = DecorUnion [DecorIf (\x _ -> mod x 5 == 0), DecorOuter]
    vdecor = DecorAll
    aligns = [AlignRight, AlignText "."]
    cells  = zipWith (\x y -> [x, y]) xs ys
    xs     = map (T.pack . show . round) nums
    ys     = map (T.pack . take 5 . show . (100.0 /)) nums
    nums   = take n $ iterate (+1.0) 1.0

main :: IO ()
main = getArgs >>= (\[n] -> T.putStrLn (table (read n)))

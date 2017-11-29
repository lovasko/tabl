{-# LANGUAGE OverloadedStrings #-}

module Text.Tabl.Ascii.Decoration
( apply
) where

import Safe
import qualified Data.Text as T

import Text.Tabl.Util


-- | Convert decoration presence to actual decorator text.
verticalDecorators
  :: [Bool]   -- ^ presence
  -> [T.Text] -- ^ decorators
verticalDecorators pres = concat [left, mid, right]
  where
    left  = map (bool "| "  "")  [head pres]
    mid   = map (bool " | " " ") (tail $ init pres)
    right = map (bool  " |" "")  [last pres]

-- | Create the decorative horizontal line.
line
  :: [T.Text] -- ^ first row
  -> [T.Text] -- ^ vertical decoration
  -> T.Text   -- ^ horizontal line
line frow vdecor = zipcat isects dashes
  where
    dashes = map (\cell -> T.replicate (T.length cell) "-") frow
    isects = map (T.map conv) vdecor
    conv c = lookupJustDef '?' c [(' ', '-'), ('|', '+')]

-- | Apply both vertical and horizontal decorations to the table cells.
apply
  :: [Bool]     -- ^ horizontal decoration
  -> [Bool]     -- ^ vertical decoration
  -> [[T.Text]] -- ^ table cell data
  -> T.Text     -- ^ decorated table
apply hpres vpres cells = T.intercalate "\n" $ intersperseOn rows hpres hline
  where
    vdecor = verticalDecorators vpres
    hline  = line (head cells) vdecor
    rows   = map (zipcat vdecor) cells

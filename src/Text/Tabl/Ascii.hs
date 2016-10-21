{-# LANGUAGE OverloadedStrings #-}

module Text.Tabl.Ascii
( ascii
) where

import Text.Tabl.Alignment
import Text.Tabl.Util

import qualified Data.Text as T

-- | Compute the greatest cell width of each column.
columnWidths :: [[T.Text]] -- ^ table cell data
             -> [Int]      -- ^ column widths
columnWidths cells = foldr combine zeros cells
  where
    zeros   = replicate (length $ head cells) 0
    combine = zipWith (\txt len -> max len (T.length txt))

-- | Convert decoration presence to actual decorator text.
verticalDecorators :: [Bool]   -- ^ presence
                   -> [T.Text] -- ^ decorators
verticalDecorators pres  = [left $ head pres]
                        ++ map mid (drop 1 $ init pres)
                        ++ [right $ last pres]
  where
    left  = bool "| "  ""
    mid   = bool " | " " "
    right = bool "  |" ""

-- | Create the decorative horizontal line.
horizontalLine :: [T.Text] -- ^ first row
               -> [T.Text] -- ^ vertical decoration
               -> T.Text   -- ^ horizontal line
horizontalLine frow vdecor = zipcat isects dashes
  where
    dashes   = map (\cell -> T.replicate (T.length cell) "-") frow
    isects   = map (T.map conv) vdecor
    conv ' ' = '-'
    conv '|' = '+'
    conv _   = '?'

-- | Apply both vertical and horizontal decorations to the table.
applyDecoration :: [Bool]     -- ^ horizontal decoration
                -> [Bool]     -- ^ vertical decoration
                -> [[T.Text]] -- ^ table cell data
                -> [T.Text]   -- ^ decorated rows
applyDecoration hpres vpres cells = intersperseOn rows hpres hline
  where
    vdecor = verticalDecorators vpres
    hline  = horizontalLine (head cells) vdecor
    rows   = map (zipcat vdecor) cells

-- | Align a cell content based on specified width and style.
alignCell :: Alignment -- ^ alignment style
          -> Int       -- ^ width
          -> T.Text    -- ^ text
          -> T.Text    -- ^ aligned text
alignCell AlignLeft   = flip T.justifyLeft  ' '
alignCell AlignRight  = flip T.justifyRight ' '
alignCell AlignCentre = flip T.center       ' '

-- | Align each cell of the table based on the width and alignment of
-- the column it is in.
alignCells :: [[T.Text]]  -- ^ table cell data
           -> [Int]       -- ^ column widths
           -> [Alignment] -- ^ column alignments
           -> [[T.Text]]  -- ^ aligned table cell data
alignCells cells widths aligns = map (zipWith3 alignCell aligns widths) cells

-- | Create a table layout using elements of ASCII art, thus making the table
-- suitable for the command line environment.
ascii :: [Bool]      -- ^ horizontal decoration
      -> [Bool]      -- ^ vertical decoration
      -> [Alignment] -- ^ column alignments
      -> [[T.Text]]  -- ^ table cell data
      -> T.Text      -- ^ table
ascii hpres vpres aligns cells = T.intercalate "\n" drows
  where
    drows  = applyDecoration hpres vpres acells
    acells = alignCells cells (columnWidths cells) aligns


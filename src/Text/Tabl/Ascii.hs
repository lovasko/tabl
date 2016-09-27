{-# LANGUAGE OverloadedStrings #-}

module Text.Tabl.Ascii
( ascii
) where

import Text.Tabl.Alignment
import Text.Tabl.Util

import qualified Data.Text as Text

-- | Compute the greatest cell width of each column.
columnWidths :: [[Text.Text]] -- ^ table cell data
             -> [Int]      -- ^ column widths
columnWidths cells = foldr combine zeros cells
  where
    zeros   = replicate (length $ head cells) 0
    combine = zipWith (\txt len -> max len (Text.length txt))

-- | Convert decoration presence to actual decorator text.
verticalDecorators :: [Bool]      -- ^ presence
                   -> [Text.Text] -- ^ decorators
verticalDecorators pres  = [left $ head pres]
                        ++ map mid (drop 1 $ init pres)
                        ++ [right $ last pres]
  where
    left  = bool "| "  ""
    mid   = bool " | " " "
    right = bool "  |" ""

-- | Create the decorative horizontal line.
horizontalLine :: [Text.Text] -- ^ first row
               -> [Text.Text] -- ^ vertical decoration
               -> Text.Text   -- ^ horizontal line
horizontalLine frow vdecor = zipcat isects dashes
  where
    dashes   = map (\cell -> Text.replicate (Text.length cell) "-") frow
    isects   = map (Text.map conv) vdecor
    conv ' ' = '-'
    conv '|' = '+'

-- | Apply both vertical and horizontal decorations to the table.
applyDecoration :: [Bool]        -- ^ horizontal decoration
                -> [Bool]        -- ^ vertical decoration
                -> [[Text.Text]] -- ^ table cell data
                -> [Text.Text]   -- ^ decorated rows
applyDecoration hpres vpres cells = intersperseOn rows hpres hline
  where
    vdecor = verticalDecorators vpres
    hline  = horizontalLine (head cells) vdecor
    rows   = map (zipcat vdecor) cells

-- | Align a cell content based on specified width and style.
alignCell :: Alignment -- ^ alignment style
          -> Int       -- ^ width
          -> Text.Text -- ^ text
          -> Text.Text -- ^ aligned text
alignCell AlignLeft   = flip Text.justifyLeft  ' '
alignCell AlignRight  = flip Text.justifyRight ' '
alignCell AlignCentre = flip Text.center       ' '

-- | Align each cell of the table based on the width and alignment of
-- the column it is in.
alignCells :: [[Text.Text]]  -- ^ table cell data
           -> [Int]          -- ^ column widths
           -> [Alignment]    -- ^ column alignments
           -> [[Text.Text]]  -- ^ aligned table cell data
alignCells cells widths aligns = map (zipWith3 alignCell aligns widths) cells

-- | Create a table layout using elements of ASCII art, thus making the table
-- suitable for the command line environment.
ascii :: [Bool]        -- ^ horizontal decoration
      -> [Bool]        -- ^ vertical decoration
      -> [Alignment]   -- ^ column alignments
      -> [[Text.Text]] -- ^ table cell data
      -> Text.Text     -- ^ table
ascii hpres vpres aligns cells = Text.intercalate "\n" drows
  where
    drows  = applyDecoration hpres vpres acells
    acells = alignCells cells (columnWidths cells) aligns


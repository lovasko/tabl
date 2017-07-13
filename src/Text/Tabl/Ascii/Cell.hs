module Text.Tabl.Ascii.Cell
( apply
) where

import qualified Data.Text as T

import Text.Tabl.Alignment


-- | Table cell.
type Cell = (T.Text, T.Text, T.Text)

-- | Column width expressed in sub-cell parts.
type Width = (Int, Int, Int)

-- | Parse the cell content into three separate subcells, each one with
-- different subalignment. The subalignments are right, centre and left
-- respectively.
split :: Alignment -- ^ alignment
      -> T.Text    -- ^ cell content
      -> Cell      -- ^ cell structure
split AlignLeft          text = (T.empty, T.empty, text)
split AlignCentre        text = (T.empty, text, T.empty)
split AlignRight         text = (text, T.empty, T.empty)
split (AlignText needle) text = (before, match, after)
  where
    (before, rest) = T.breakOn needle text
    (match, after) = T.splitAt (T.length needle) rest
split (AlignIndex func)  text = (before, match, after)
  where
    (before, rest) = T.splitAt (maybe (T.length text) id (func text)) text
    (match, after) = T.splitAt 1 rest

-- | Stretch a cell with whitespace based on specified widths.
stretch :: Width  -- ^ subcell widths
        -> Cell   -- ^ table cell
        -> T.Text -- ^ aligned text
stretch (w1, w2, w3) (p1, p2, p3) = T.concat [right, mid, left]
  where
    right = T.justifyRight w1 ' ' p1
    mid   = T.center       w2 ' ' p2
    left  = T.justifyLeft  w3 ' ' p3

-- | Update the set of widths for a new table row.
updateWidths :: [Cell]  -- ^ table row
             -> [Width] -- ^ old widths
             -> [Width] -- ^ new widths
updateWidths row = zipWith maxElem (map width row)
  where
    maxElem (x1, x2, x3) (y1, y2, y3) = (max x1 y1, max x2 y2, max x3 y3)
    width   (p1, p2, p3) = (T.length p1, T.length p2, T.length p3)

-- | Compute the maximal cell widths for each column.
widths :: [[Cell]] -- ^ table cells
       -> [Width] -- ^ column widths
widths cells = foldr updateWidths zeros cells
  where zeros = replicate (maximum $ map length cells) (0, 0, 0)

-- | Align each cell of the table based on the width and alignment of
-- the column it is in.
apply :: [[T.Text]]  -- ^ table cell data
      -> [Alignment] -- ^ column alignments
      -> [[T.Text]]  -- ^ aligned table cell data
apply table aligns = map (zipWith stretch (widths cells)) cells
  where cells = map (zipWith split aligns) table

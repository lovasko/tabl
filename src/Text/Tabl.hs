{- |
Module      : Text.Tabl
Description : Table layout engine that provides alignment and decoration
Copyright   : (c) 2016-2020 Daniel Lovasko
License     : BSD2

Maintainer  : Daniel Lovasko <daniel.lovasko@gmail.com>
Stability   : stable
Portability : portable

Text.Tabl arranges multiple Text instances into a table layout while
providing means of alignment, and visual decoration both horizontally
and vertically.
-}

module Text.Tabl
( Alignment(..)
, Decoration(..)
, Environment(..)
, tabl
) where

import qualified Data.Text as T

import Text.Tabl.Alignment
import Text.Tabl.Ascii
import Text.Tabl.Decoration
import Text.Tabl.Environment
import Text.Tabl.Latex
import Text.Tabl.Util


-- | Create a table layout based on the specified output environment,
-- decorations and alignments.
tabl
  :: Environment -- ^ output environment
  -> Decoration  -- ^ horizontal decoration
  -> Decoration  -- ^ vertical decoration
  -> [Alignment] -- ^ column alignments
  -> [[T.Text]]  -- ^ table cell data
  -> T.Text      -- ^ final layout
tabl _   _      _      _      []    = T.empty
tabl _   _      _      _      [[]]  = T.empty
tabl env hdecor vdecor aligns cells = render env hpres vpres ealigns ecells
  where
    -- Alternative to a type-class.
    render EnvAscii = ascii
    render EnvLatex = latex

    -- Compute the locations of horizontal and vertical decorations.
    hpres           = presence (length cells        + 1) hdecor
    vpres           = presence (length (head cells) + 1) vdecor

    -- Fill the rows with fewer columns with empty strings.
    ecells          = map (extend columnCount T.empty) cells

    -- Assume alignment to the left for the columns without specification.
    ealigns         = extend columnCount AlignLeft aligns

    -- Compute the final number of columns of the table.
    columnCount     = maximum $ map length cells

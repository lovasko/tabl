{- |
Module      : Text.Tabl
Description : Table layout engine that provides alignment and decoration
Copyright   : (c) Daniel Lovasko, 2016
License     : BSD3

Maintainer  : Daniel Lovasko <daniel.lovasko@gmail.com>
Stability   : stable
Portability : portable

Text.Tabl arranges multiple Text instances into a table layout while
providing means of alignment visual decoration both horizontally and
vertically.
-}

module Text.Tabl
( Alignment(..)
, Decoration(..)
, Environment(..)
, tabl
) where

import Text.Tabl.Alignment
import Text.Tabl.Ascii
import Text.Tabl.Decoration
import Text.Tabl.Environment
import Text.Tabl.Latex
import Text.Tabl.Util

import qualified Data.Text as T

-- | Create a table layout based on specified output environment,
-- decorations and alignments.
tabl :: Environment -- ^ output environment
     -> Decoration  -- ^ horizontal decoration
     -> Decoration  -- ^ vertical decoration
     -> [Alignment] -- ^ column alignments
     -> [[T.Text]]  -- ^ table cell data
     -> T.Text      -- ^ final layout
tabl _   _      _      _      []    = T.empty
tabl _   _      _      _      [[]]  = T.empty
tabl env hdecor vdecor aligns cells = render env hpres vpres ealigns ecells
  where
    render EnvAscii = ascii
    render EnvLatex = latex
    hpres           = presence (length cells + 1) hdecor
    vpres           = presence (length (head cells) + 1) vdecor
    columnCount     = maximum $ map length cells
    ecells          = map (extend columnCount T.empty) cells
    ealigns         = extend columnCount AlignLeft aligns


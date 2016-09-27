{-# LANGUAGE OverloadedStrings #-}

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

import qualified Data.Text as Text

-- | Create a table layout based on specified output environment,
-- decorations and alignments.
tabl :: Environment   -- ^ output environment
     -> Decoration    -- ^ horizontal decoration
     -> Decoration    -- ^ vertical decoration
     -> [Alignment]   -- ^ column alignments
     -> [[Text.Text]] -- ^ table cell data
     -> Text.Text     -- ^ final layout
tabl _ _ _ _ [[]] = ""
tabl env hdecor vdecor aligns cells = render env hpres vpres ealigns ecells
  where
    render EnvAscii = ascii
    render EnvLatex = latex
    hpres           = presence (length cells + 1) hdecor
    vpres           = presence (length (head cells) + 1) vdecor
    columnCount     = maximum $ map length cells
    ecells          = map (extend columnCount "") cells
    ealigns         = extend columnCount AlignLeft aligns


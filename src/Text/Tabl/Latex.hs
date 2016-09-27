{-# LANGUAGE OverloadedStrings #-}

module Text.Tabl.Latex
( latex
) where

import Text.Tabl.Alignment
import Text.Tabl.Decoration
import Text.Tabl.Util
import qualified Data.Text as Text

-- | Convert the table cell data to LaTeX-compatible form.
createTable :: [[Text.Text]] -- ^ table cells
            -> [Text.Text]   -- ^ latexified rows
createTable = map (Text.append " \\\\" . Text.intercalate " & ")

-- | Create the table header with vertical decoration and column alignments.
alignSpecifier :: [Bool]      -- ^ vertical decoration
               -> [Alignment] -- ^ column alignments
               -> Text.Text   -- ^ header
alignSpecifier vpres aligns = Text.concat ["{ ", info, "}"]
  where
    info               = Text.concat $ intersperseOn letters vpres "| "
    letters            = map letter aligns
    letter AlignLeft   = "l "
    letter AlignRight  = "r "
    letter AlignCentre = "c "

-- | Create a LaTeX-compatible source code that represents the requested
-- table layout.
latex :: [Bool]        -- ^ horizontal decoration
      -> [Bool]        -- ^ vertical decoration
      -> [Alignment]   -- ^ column alignments
      -> [[Text.Text]] -- ^ table cell data
      -> Text.Text     -- ^ table
latex hpres vpres aligns cells =
  Text.concat [ "\\begin{tabular}"
              ,  alignSpecifier vpres aligns
              ,  "\n"
              ,  Text.unlines table
              ,  "\\end{tabular}" ]
  where
    table  = intersperseOn (createTable cells) hpres "\\hline"


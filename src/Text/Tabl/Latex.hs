{- |
Module      : Text.Tabl.Latex
Description : LaTeX table rendering engine
Copyright   : (c) Daniel Lovasko, 2016
License     : BSD3

Maintainer  : Daniel Lovasko <daniel.lovasko@gmail.com>
Stability   : stable
Portability : portable

Implementation of the LaTeX environment for table rendering.
-}

{-# LANGUAGE OverloadedStrings #-}

module Text.Tabl.Latex
( latex
) where

import Text.Tabl.Alignment
import Text.Tabl.Util

import qualified Data.Text as T

-- | Convert the table cell data to LaTeX-compatible form.
createTable :: [[T.Text]] -- ^ table cells
            -> [T.Text]   -- ^ latexified rows
createTable = map (flip T.append " \\\\" . T.intercalate " & ")

-- | Create the table header with vertical decoration and column alignments.
alignSpecifier :: [Bool]      -- ^ vertical decoration
               -> [Alignment] -- ^ column alignments
               -> T.Text      -- ^ header
alignSpecifier vpres aligns = T.concat ["{ ", info, "}"]
  where
    info               = T.concat $ intersperseOn letters vpres "| "
    letters            = map letter aligns
    letter AlignLeft   = "l "
    letter AlignRight  = "r "
    letter AlignCentre = "c "

-- | Create a LaTeX-compatible source code that represents the requested
-- table layout.
latex :: [Bool]      -- ^ horizontal decoration
      -> [Bool]      -- ^ vertical decoration
      -> [Alignment] -- ^ column alignments
      -> [[T.Text]]  -- ^ table cell data
      -> T.Text      -- ^ table
latex hpres vpres aligns cells =
  T.concat [ "\\begin{tabular}"
           ,  alignSpecifier vpres aligns
           ,  "\n"
           ,  T.unlines table
           ,  "\\end{tabular}" ]
  where
    table = intersperseOn (createTable cells) hpres "\\hline"

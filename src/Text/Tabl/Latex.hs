{- |
Module      : Text.Tabl.Latex
Description : LaTeX table rendering engine
Copyright   : (c) 2016-2017 Daniel Lovasko
License     : BSD2

Maintainer  : Daniel Lovasko <daniel.lovasko@gmail.com>
Stability   : stable
Portability : portable

Implementation of the LaTeX environment for table rendering.
-}

{-# LANGUAGE OverloadedStrings #-}

module Text.Tabl.Latex
( latex
) where

import qualified Data.Text as T

import Text.Tabl.Alignment
import Text.Tabl.Util


-- | Convert the table cell data to LaTeX-compatible form.
createTable :: [[T.Text]] -- ^ table cells
            -> [T.Text]   -- ^ latexified rows
createTable = map (flip T.append " \\\\" . T.intercalate " & ")

-- | Create the table header with vertical decoration and column
-- alignments.
alignSpecifier :: [Bool]      -- ^ vertical decoration
               -> [Alignment] -- ^ column alignments
               -> T.Text      -- ^ header
alignSpecifier vpres aligns = T.concat ["{ ", info, "}"]
  where
    info                  = T.concat $ intersperseOn letters vpres "| "
    letters               = map letter aligns
    letter AlignLeft      = "l "
    letter AlignRight     = "r "
    letter AlignCentre    = "c "
    letter (AlignText _)  = "c "
    letter (AlignIndex _) = "c "

-- | Create a LaTeX-compatible source code that represents the requested
-- table layout.
latex :: [Bool]      -- ^ horizontal decoration
      -> [Bool]      -- ^ vertical decoration
      -> [Alignment] -- ^ column alignments
      -> [[T.Text]]  -- ^ table cell data
      -> T.Text      -- ^ final layout
latex hpres vpres aligns cells = T.concat
  [ "\\begin{tabular}"
  ,  alignSpecifier vpres aligns
  ,  "\n"
  ,  T.unlines $ intersperseOn (createTable cells) hpres "\\hline"
  ,  "\\end{tabular}" ]

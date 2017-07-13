{- |
Module      : Text.Tabl.Ascii
Description : ASCII-art table rendering engine
Copyright   : (c) 2016-2017 Daniel Lovasko
License     : BSD2

Maintainer  : Daniel Lovasko <daniel.lovasko@gmail.com>
Stability   : stable
Portability : portable

Implementation of the ASCII-art environment for table rendering.
-}

{-# LANGUAGE OverloadedStrings #-}

module Text.Tabl.Ascii
( ascii
) where

import qualified Data.Text as T

import Text.Tabl.Alignment
import qualified Text.Tabl.Ascii.Cell as C
import qualified Text.Tabl.Ascii.Decoration as D


-- | Create a table layout using elements of ASCII art, thus making the table
-- suitable for the command line environment.
ascii :: [Bool]      -- ^ horizontal decoration
      -> [Bool]      -- ^ vertical decoration
      -> [Alignment] -- ^ column alignments
      -> [[T.Text]]  -- ^ table cell data
      -> T.Text      -- ^ table
ascii hpres vpres aligns cells = D.apply hpres vpres (C.apply escaped aligns)
  where escaped = map (map (T.replace "\n" "\\n" . T.replace "\t" "\\t")) cells

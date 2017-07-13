{- |
Module      : Text.Tabl.Alignment
Description : Column alignments
Copyright   : (c) 2016-2017 Daniel Lovasko
License     : BSD2

Maintainer  : Daniel Lovasko <daniel.lovasko@gmail.com>
Stability   : stable
Portability : portable

Definition of column alignment options.
-}

module Text.Tabl.Alignment
( Alignment(..)
) where

import qualified Data.Text as T


-- | Presentation style that is used to define the alignment of each
-- column of the table.
data Alignment
  = AlignLeft                        -- ^ left alignment
  | AlignRight                       -- ^ right alignment
  | AlignCentre                      -- ^ center around the middle character
  | AlignText T.Text                 -- ^ center around a substring
  | AlignIndex (T.Text -> Maybe Int) -- ^ center around a index

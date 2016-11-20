{- |
Module      : Text.Tabl.Alignment
Description : Column alignments
Copyright   : (c) Daniel Lovasko, 2016
License     : BSD3

Maintainer  : Daniel Lovasko <daniel.lovasko@gmail.com>
Stability   : stable
Portability : portable

Definition of column alignment options.
-}

module Text.Tabl.Alignment
( Alignment(..)
) where

-- | Presentation style that is used to define the alignment of each
-- column of the table.
data Alignment
  = AlignLeft   -- ^ left alignment
  | AlignCentre -- ^ centre
  | AlignRight  -- ^ right alignment
  deriving (Show)

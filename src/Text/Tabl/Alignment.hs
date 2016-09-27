module Text.Tabl.Alignment
( Alignment(..)
) where

-- | Presentation style that is used to describe the alignment of each
-- column of the table.
data Alignment
  = AlignLeft   -- ^ "Left      "
  | AlignCentre -- ^ "  Centre  "
  | AlignRight  -- ^ "     Right"


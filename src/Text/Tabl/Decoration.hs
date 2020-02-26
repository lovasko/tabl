{- |
Module      : Text.Tabl.Decoration
Description : Horizontal and vertical decoration
Copyright   : (c) 2016-2020 Daniel Lovasko
License     : BSD2

Maintainer  : Daniel Lovasko <daniel.lovasko@gmail.com>
Stability   : stable
Portability : portable

Definition of type combinators that are used to describe both horizontal
and vertical table decoration.
-}

module Text.Tabl.Decoration
( Decoration(..)
, presence
) where

import qualified Data.Text as T


-- | Decoration style that defines which lines (horizontal or vertical)
-- will be visible in the resulting table.
data Decoration
  = DecorAll                          -- ^ all lines
  | DecorOuter                        -- ^ outer lines
  | DecorOnly [Int]                   -- ^ only certain lines
  | DecorUnion [Decoration]           -- ^ union of more decorations
  | DecorIsect [Decoration]           -- ^ intersection of more decorations
  | DecorIf (Int -> [T.Text] -> Bool) -- ^ based on a predicate result
  | DecorNegate Decoration            -- ^ opposite of a decoration

-- | Convert a decoration to a list of presence information.
presence
  :: Int        -- ^ width
  -> [[T.Text]] -- ^ cells
  -> Decoration -- ^ decoration
  -> [Bool]     -- ^ presence
presence n _  DecorAll        = replicate n True
presence n _  DecorOuter      = [True] ++ replicate (n-2) False ++ [True]
presence n _  (DecorOnly is)  = map (`elem` is) [0..(n-1)]
presence n cs (DecorUnion ds) = combine (||) False n cs ds
presence n cs (DecorIsect ds) = combine (&&) True  n cs ds
presence n cs (DecorIf fn)    = zipWith fn [0..(n-1)] cs
presence n cs (DecorNegate d) = map not (presence n cs d)

-- | Combine multiple decorations into one based on a selected function.
combine
  :: (Bool -> Bool -> Bool) -- ^ combination function
  -> Bool                   -- ^ default value
  -> Int                    -- ^ width
  -> [[T.Text]]             -- ^ cells
  -> [Decoration]           -- ^ decorations
  -> [Bool]                 -- ^ presence
combine fn def n cs ds = foldr step first presences
  where
    first     = replicate n def
    step      = zipWith fn
    presences = map (presence n cs) ds

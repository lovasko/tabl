{- |
Module      : Text.Tabl.Decoration
Description : Horizontal and vertical decoration
Copyright   : (c) 2016-2017 Daniel Lovasko
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

-- | Decoration style that defines which lines (horizontal or vertical)
-- will be visible in the resulting table.
data Decoration
  = DecorNone               -- ^ no lines
  | DecorAll                -- ^ all lines
  | DecorInner              -- ^ inner lines
  | DecorOuter              -- ^ outer lines
  | DecorOnly [Int]         -- ^ only certain lines
  | DecorExcept [Int]       -- ^ all but certain lines
  | DecorUnion [Decoration] -- ^ union of more decorations
  | DecorIsect [Decoration] -- ^ intersection of more decorations
  | DecorIf (Int -> Bool)   -- ^ based on a predicate result

-- | Convert a decoration to a list of presence information.
presence
  :: Int        -- ^ width
  -> Decoration -- ^ decoration
  -> [Bool]     -- ^ presence
presence n DecorNone        = replicate n False
presence n DecorAll         = replicate n True
presence n DecorInner       = [False] ++ replicate (n-2) True ++ [False]
presence n DecorOuter       = [True] ++ replicate (n-2) False ++ [True]
presence n (DecorOnly is)   = map (`elem` is) [0..(n-1)]
presence n (DecorExcept is) = map not (presence n (DecorOnly is))
presence n (DecorUnion ds)  = combine (||) False n ds
presence n (DecorIsect ds)  = combine (&&) True  n ds
presence n (DecorIf func)   = map func [0..(n-1)]

-- | Combine multiple decorations into one based on a selected function.
combine
  :: (Bool -> Bool -> Bool) -- ^ combination function
  -> Bool                   -- ^ default value
  -> Int                    -- ^ width
  -> [Decoration]           -- ^ decorations
  -> [Bool]                 -- ^ presence
combine fn def n ds = foldr step first presences
  where
    first     = replicate n def
    step      = zipWith fn
    presences = map (presence n) ds

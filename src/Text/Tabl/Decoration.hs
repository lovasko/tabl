module Text.Tabl.Decoration
( Decoration(..)
, presence
) where

-- | Decoration style that defines which lines (horizontal or vertical)
-- will be visible in the resulting table.
data Decoration
  = DecorNone               -- ^ No lines.
  | DecorAll                -- ^ All lines.
  | DecorInner              -- ^ Inner lines.
  | DecorOuter              -- ^ Outer lines.
  | DecorOnly [Int]         -- ^ Only certain lines.
  | DecorExcept [Int]       -- ^ All but certain lines.
  | DecorUnion [Decoration] -- ^ All lines from more decorations.
  | DecorIsect [Decoration] -- ^ Only intersecting lines.
  deriving (Show)

-- | Convert a decoration to a list of presence information.
presence :: Int        -- ^ width
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

-- | Combine multiple decorations into one.
combine :: (Bool -> Bool -> Bool) -- ^ combination function
        -> Bool                   -- ^ default value
        -> Int                    -- ^ width
        -> [Decoration]           -- ^ decorations
        -> [Bool]                 -- ^ presence
combine fn def n ds = foldr step first presences
  where
    first     = replicate n def
    step      = zipWith fn
    presences = map (presence n) ds


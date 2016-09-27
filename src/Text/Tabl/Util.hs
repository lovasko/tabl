module Text.Tabl.Util
( extend
, intersperseOn
, zipcat
) where

import Data.Monoid

-- | Extend a list to defined length with one repeated element.
extend :: Int -- ^ expected length
       -> a   -- ^ element to pad with
       -> [a] -- ^ original list
       -> [a] -- ^ extended list
extend n x xs = xs ++ replicate (n - length xs) x

-- | Insert an element before i-th position, if the i-th Bool is True.
intersperseOn :: (Monoid a) -- ^ required for mempty
              => [a]        -- ^ list
              -> [Bool]     -- ^ insert rules
              -> a          -- ^ element to insert
              -> [a]        -- ^ new list
intersperseOn xs bs x = init $ concat $ zipWith glue bs (xs ++ [mempty])
  where
    glue True i  = [x, i]
    glue False i = [i]

-- | Create an object by zipping two lists together. The second list is
-- expected to be one element shorter.
zipcat :: (Monoid a) -- ^ required for mempty
       => [a]        -- ^ first list
       -> [a]        -- ^ second list
       -> a          -- ^ result
zipcat xs ys = mconcat $ zipWith mappend xs (mappend ys [mempty])


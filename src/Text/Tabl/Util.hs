{- |
Module      : Text.Tabl.Util
Description : Various utilities
Copyright   : (c) Daniel Lovasko, 2016
License     : BSD3

Maintainer  : Daniel Lovasko <daniel.lovasko@gmail.com>
Stability   : stable
Portability : portable

Set of general utilities that are used across the whole project.
-}

module Text.Tabl.Util
( bool
, extend
, intersperseOn
, zipcat
) where

-- | Extend a list to a defined length with one repeated element. This
-- function assumes that the list is shorter than the provided length.
extend :: Int -- ^ expected length
       -> a   -- ^ element to pad with
       -> [a] -- ^ original list
       -> [a] -- ^ extended list
extend n x xs = xs ++ replicate (n - length xs) x

-- | Insert an element in front of the i-th position, if the i-th element
-- is True.
intersperseOn :: (Monoid a)
              => [a]    -- ^ list
              -> [Bool] -- ^ insert rules
              -> a      -- ^ element to insert
              -> [a]    -- ^ new list
intersperseOn xs bs x = init $ concat $ zipWith glue bs (xs ++ [mempty])
  where
    glue True i  = [x, i]
    glue False i = [i]

-- | Functional implementation of the if/then/else concept.
bool :: a    -- ^ True option
     -> a    -- ^ False option
     -> Bool -- ^ condition
     -> a    -- ^ result
bool x _ True  = x
bool _ y False = y

-- | Create an object by zipping two lists together. The second list is
-- expected to be one element shorter.
zipcat :: (Monoid a)
       => [a] -- ^ first list
       -> [a] -- ^ second list
       -> a   -- ^ result
zipcat xs ys = mconcat $ zipWith mappend xs (mappend ys [mempty])

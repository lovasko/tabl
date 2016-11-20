{- |
Module      : Text.Tabl.Environment
Description : Table environments
Copyright   : (c) Daniel Lovasko, 2016
License     : BSD3

Maintainer  : Daniel Lovasko <daniel.lovasko@gmail.com>
Stability   : stable
Portability : portable

Definition of various environments for table rendering.
-}

module Text.Tabl.Environment
( Environment(..)
) where

-- | Output environment that declares the way that the table will be
-- rendered.
data Environment
  = EnvAscii -- ^ ASCII art suitable for command line
  | EnvLatex -- ^ LaTeX source code
  deriving (Show)

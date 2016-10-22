module Text.Tabl.Environment
( Environment(..)
) where

-- | Output environment that declares the way that the table will be
-- rendered.
data Environment
  = EnvAscii -- ^ ASCII art suitable for command line
  | EnvLatex -- ^ LaTeX source code
  deriving (Show)


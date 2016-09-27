module Text.Tabl.Environment
( Environment(..)
) where

-- Output environment.
data Environment = EnvAscii -- ^ ASCII art suitable for command line
                 | EnvLatex -- ^ LaTeX source code


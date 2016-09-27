{-# LANGUAGE OverloadedStrings #-}

import Text.Tabl

import qualified Data.Text as Text
import qualified Data.Text.IO as Text

-- | Table containing basic info about programming language creators.
main :: IO ()
main = Text.putStrLn $ tabl EnvLatex hdecor vdecor [] info
  where
    hdecor = DecorUnion [DecorOuter, DecorOnly [1]]
    vdecor = DecorAll
    info   = [ ["Language", "Year", "Author"]
             , ["C", "1972", "Dennis Ritchie"]
             , ["Lisp", "1958", "John McCarthy"]
             , ["Python", "1991", "Guido van Rossum"] ]


{-# LANGUAGE OverloadedStrings #-}

import Text.Tabl

import qualified Data.Text.IO as T

-- | Table containing basic info about programming language creators.
main :: IO ()
main = T.putStrLn $ tabl EnvAscii hdecor vdecor [] info
  where
    hdecor = DecorUnion [DecorOuter, DecorOnly [1]]
    vdecor = DecorAll
    info   = [ ["Language", "Year", "Author"]
             , ["C", "1972", "Dennis Ritchie"]
             , ["Lisp", "1958", "John McCarthy"]
             , ["Python", "1991", "Guido van Rossum"] ]


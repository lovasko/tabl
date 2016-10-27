{-# LANGUAGE OverloadedStrings #-}

import Text.Tabl

import qualified Data.Text.IO as T

-- | Table containing few physics constants.
main :: IO ()
main = T.putStrLn $ tabl EnvAscii hdecor vdecor aligns cells 
  where
    hdecor = DecorUnion [DecorOuter, DecorOnly [1]]
    vdecor = DecorAll
    aligns = [AlignLeft, AlignLeft, AlignRight]
    cells  = [ ["Name", "SI Unit", "Value"]
             , ["Speed of light", "m/s", "299792458"]
             , ["Atmosphere", "Pa", "101325"]
             , ["Absolute zero", "C", "-273.15"] ]


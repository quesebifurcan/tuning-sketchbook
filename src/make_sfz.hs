{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as Text
import Prelude hiding (FilePath)
import Turtle
import Music.Theory.Tuning.Scala

midiRange = [0..127]

hertzToMidi :: Double -> Double
hertzToMidi hertz = 69.0 + (12.0 * (logBase 2 (hertz / 440.0)))

closestMidiPitch :: (Ord c, Num c) => [c] -> c -> c
closestMidiPitch candidates midiCents =
  snd . minimum . map (\x -> (abs (x - midiCents), x)) $
  candidates

unfoldScale :: Scale Integer -> [Music.Theory.Tuning.Cents]
unfoldScale scale =
  let top = (0, scale_octave scale)
      midiCents = map (/100.0) $ scale_cents scale
  in map (closestMidiPitch midiRange) midiCents

argParser :: Parser FilePath
argParser = argPath "sclFile" "Path to an .scl file"

main :: IO ()
main = do
  scaleFile <- options "Create .sfz files for microtonal instruments" argParser
  s <- load ((Text.unpack . format fp) scaleFile)
  print $ unfoldScale s

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as Text
import qualified Control.Foldl as Fold
import Prelude hiding (FilePath)
import Turtle
import Music.Theory.Tuning.Scala
import Music.Theory.Tuning

-- Example:
--
-- $ stack runghc src/make_sfz.hs test_files/young-lm_piano.scl 440
--
-- results in:
--
-- [[69.0,0.0],[71.0,-0.2335409006932565],[71.0,3.9100017307745816e-2],[71.0,0.39606813803636953],[74.0,-0.29219092665488233],[73.0,0.43516815534411535],[76.0,-0.2530909093471223],[76.0,1.9550008653880013e-2],[76.0,0.37651812938250373],[79.0,-0.31174093530874813],[78.0,0.41561814669024955],[81.0,-0.2726409180010023],[81.0,0.0]]

midiRange = [0..127]

hertzToMidi :: Double -> Double
hertzToMidi hertz = 69.0 + (12.0 * (logBase 2 (hertz / 440.0)))

distance :: Num t => t -> t -> [t]
distance a b =
  let diff = (a - b)
  in [abs diff, b, diff]

closestMidiPitch :: (Ord t, Num t) => [t] -> t -> [t]
closestMidiPitch candidates midiCents =
  minimum . map (distance midiCents) $
  candidates

scalePitchToMidiCent :: Double -> Double -> Double
scalePitchToMidiCent baseFreq pitch = (pitch / 100.0) + (hertzToMidi baseFreq)

unfoldScale :: Double -> Scale Integer -> [[Double]]
unfoldScale baseFreq scale =
  let midiCents = map (scalePitchToMidiCent baseFreq) $ scale_cents scale
  in map (tail . closestMidiPitch midiRange) midiCents

argParser :: Parser (FilePath, Double)
argParser =
  (,) <$>
  argPath "sclFile" "Path to an .scl file" <*>
  argDouble "baseFreq" "Base frequency"

-- testFile :: MonadIO io => io [FilePath]
-- testFile =
--   Turtle.fold (lstree "/Users/fred/Documents/TownHallOrgan_SP/Samples/PRE1") Fold.list

pitchParser :: Pattern (Char, Maybe Text, Char)
pitchParser = do
  pitch <- oneOf "ABCDEFG"
  accidental <- optional "#"
  octave <- digit
  return (pitch, accidental, octave)

main :: IO ()
main = do
  (scaleFile, baseFreq) <- options "Create .sfz files for microtonal instruments" argParser
  scale <- load ((Text.unpack . format fp) scaleFile)
  print $ unfoldScale baseFreq scale

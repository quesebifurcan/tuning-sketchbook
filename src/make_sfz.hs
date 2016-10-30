{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as Text
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Control.Foldl as Fold
import Control.Exception
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

midiRange = [0..127] :: [Double]

hertzToMidi :: Double -> Double
hertzToMidi hertz = 69.0 + (12.0 * (logBase 2 (hertz / 440.0)))

distance :: Num t => t -> t -> [t]
distance a b =
  let diff = (a - b)
  in [abs diff, b, diff]

closestMidiPitch candidates midiCents =
  minimum . map (distance midiCents) $
  candidates

scalePitchToMidiCent :: Double -> Double -> Double
scalePitchToMidiCent baseFreq pitch = (pitch / 100.0) + (hertzToMidi baseFreq)

unfoldScale baseFreq scale =
  let midiCents = map (scalePitchToMidiCent baseFreq) $ scale_cents scale
  in map (tail . closestMidiPitch midiRange) midiCents

argParser :: Parser (FilePath, FilePath, Double)
argParser =
  (,,) <$>
  argPath "sclFile" "Path to an .scl file" <*>
  argPath "sampleDir" "Path to a directory with samples" <*>
  argDouble "baseFreq" "Base frequency"

diatonicPitchClasses = Map.fromList [
  ('C', 0)
  , ('D', 2)
  , ('E', 4)
  , ('F', 5)
  , ('G', 7)
  , ('A', 9)
  , ('B', 11)
  ]

data MidiPitch = MidiPitch Int deriving (Show, Eq, Ord)

toMidiPitch (pitchClass, accidental, octave) =
  -- TODO: Map.! and digitToInt not safe
  let pitchClass' = (Map.!) diatonicPitchClasses pitchClass
      accidental' = case accidental of
        (Just "#") -> 1
        Nothing -> 0
      -- TODO: pass in as command line args
      soundingMiddleC = (0, 2)
      octaveOffset = 4 - snd soundingMiddleC
      octave' = (Char.digitToInt octave) + octaveOffset
  -- TODO: sort out Double/Int conflicts
  in fromIntegral (pitchClass' + accidental' + (12 * octave'))

data PitchParseException =
  PitchParseException Text deriving Show

instance Exception PitchParseException

-- TODO: better error handling
buildPathItem filePath =
  let match' = (match (has pitchParser) . format fp . filename) $ filePath
  in case match' of
    []            -> throw (PitchParseException (format ("could not guess pitch name of sample: "%fp%"") filePath))
    (pitchData:_) -> (toMidiPitch pitchData, filePath)

buildPathDict filePaths = Map.fromList $ map buildPathItem filePaths

-- TODO: more extensions
isAudioFile = suffix ".wav" <|> suffix ".aif"

testFile samples = buildPathDict samples

-- TODO: parser for numbers
pitchParser :: Pattern (Char, Maybe Text, Char)
pitchParser = do
  pitch      <- oneOf "ABCDEFG"
  accidental <- optional "#"
  octave     <- digit
  return (pitch, accidental, octave)

-- TODO: custom data types to clean up this
mapping pitches sampleMap =
  let candidates = Map.keys sampleMap
      rootPitches = map head pitches
      deviations = map (\x -> round (100 * (x !! 1))) pitches
      midiPitches = map ((head . drop 1) . closestMidiPitch candidates) rootPitches
  in zip3 rootPitches deviations (map ((Map.!) sampleMap) midiPitches)

-- main :: MonadIO io => io [FilePath]
main :: IO ()
main = do
  (scaleFile, sampleDir, baseFreq) <- options "Create .sfz files for microtonal instruments" argParser
  scaleFile' <- load ((Text.unpack . format fp) scaleFile)
  samples <- (Turtle.fold (find isAudioFile sampleDir) Fold.list)

  let scale = unfoldScale baseFreq scaleFile'
  let sampleMap = testFile samples
  let mapping' = mapping scale sampleMap

  print mapping'

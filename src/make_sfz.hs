{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as Text
import qualified Data.List as List
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Control.Foldl as Fold
import Control.Exception
import Prelude hiding (FilePath)
import Turtle
import Music.Theory.Tuning.Scala
import Music.Theory.Tuning

midiRange :: [Double]
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

diatonicPitchClasses :: Map.Map Char Int
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

toMidiPitch ::
  (IsString a, Num b, Eq a) => (Char, Maybe a, Char) -> b
toMidiPitch (pitchClass, accidental, octave) =
  -- TODO: Map.! and digitToInt not safe
  let pitchClass' = (Map.!) diatonicPitchClasses pitchClass
      accidental' = case accidental of
        (Just "#") -> 1
        Nothing -> 0
      -- TODO: pass in as command line args
      octaveOffset = 2
      octave' = (Char.digitToInt octave) + octaveOffset
  -- TODO: sort out Double/Int conflicts
  in fromIntegral (pitchClass' + accidental' + (12 * octave'))

data PitchParseException =
  PitchParseException Text deriving Show

instance Exception PitchParseException

-- TODO: better error handling
buildPathItem :: Num t => FilePath -> (t, FilePath)
buildPathItem filePath =
  let match' = (match (has pitchParser) . format fp . filename) $ filePath
  in case match' of
    []            -> throw (PitchParseException (format ("could not guess pitch name of sample: "%fp%"") filePath))
    (pitchData:_) -> (toMidiPitch pitchData, filePath)

buildPathDict :: (Ord k, Num k) => [FilePath] -> Map.Map k FilePath
buildPathDict filePaths = Map.fromList $ map buildPathItem filePaths

-- TODO: more extensions
isAudioFile :: Pattern Text
isAudioFile = suffix ".wav" <|> suffix ".aif"

testFile :: (Ord k, Num k) => [FilePath] -> Map.Map k FilePath
testFile samples = buildPathDict samples

-- TODO: parser for numbers
pitchParser :: Pattern (Char, Maybe Text, Char)
pitchParser = do
  pitch      <- oneOf "ABCDEFG"
  accidental <- optional "#"
  octave     <- digit
  return (pitch, accidental, octave)

-- TODO: custom data types to clean up this
mapping ::
  (RealFrac d, Num a, Integral b, Enum a) =>
  [[d]] -> Map.Map d c -> [(a, b, c, d)]
mapping pitches sampleMap =
  let candidates = Map.keys sampleMap
      rootPitches = map head pitches
      deviations = map (\x -> round (100 * (x !! 1))) pitches
      midiPitches = map ((head . drop 1) . closestMidiPitch candidates) rootPitches
  in List.zip4 [24..] deviations (map ((Map.!) sampleMap) midiPitches) midiPitches

formatRegion :: (RealFrac a, Integral n) => (a, n, FilePath, t) -> Text
formatRegion (rootPitch, deviation, filePath, pitchKeyCenter) =
  let s = ("<region> trigger=attack pitch_keycenter="%d%" tune="%d%" lokey="%d%" hikey="%d%" sample="%fp%"")
  in format s (round rootPitch) deviation (round rootPitch) (round rootPitch) ("samples" </> (filename filePath))

header :: Text
header = "<group>\nloop_mode=no_loop\nlovel=0\nhivel=127\n"

argParser :: Parser (FilePath, FilePath, FilePath, Double)
argParser =
  (,,,) <$>
  argPath "sclFile" "Path to an .scl file" <*>
  argPath "sampleDir" "Path to a directory with samples" <*>
  argPath "outDir" "Output directory" <*>
  argDouble "baseFreq" "Base frequency"

main :: IO ()
main = do
  (scaleFile, sampleDir, outDir, baseFreq) <- options "Create .sfz files for microtonal instruments" argParser
  scaleFile' <- load ((Text.unpack . format fp) scaleFile)
  samples <- (Turtle.fold (find isAudioFile sampleDir) Fold.list)
  let scale = unfoldScale baseFreq scaleFile'
  let sampleMap = testFile samples
  let mapping' = mapping scale sampleMap
  mktree outDir
  shell (format ("ln -s "%fp%" "%fp%"") sampleDir (outDir </> "samples")) empty
  output (outDir </> "instrument.sfz") $ select ([header] ++ (map formatRegion mapping'))

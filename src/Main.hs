{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (FilePath)
import Codec.Midi as Midi
import Control.Exception
import qualified Control.Foldl as Fold
import Data.Either
import qualified Data.Text as Text
import System.IO.Error
import Turtle

formatSplitCmd :: FilePath -> Double -> FilePath -> Text
formatSplitCmd =
  format ("ffmpeg -i "%fp%" -f segment -segment_time "%f%" -c copy "%fp%"%03d.aif")

checkExists :: MonadIO m => FilePath -> m ()
checkExists filePath = do
  exists <- testfile filePath
  case exists of
    True -> return ()
    False -> error $ show filePath ++ "does not exist"

splitAudioFile :: MonadIO m => FilePath -> Double -> FilePath -> m ExitCode
splitAudioFile filePath segmentTime out = do
  checkExists filePath
  shell (formatSplitCmd filePath segmentTime out) empty

trimFile :: MonadIO m => FilePath -> m ()
trimFile infile = do
  -- TODO: use actual tempfile
  shell (format ("ffmpeg -y -i "%fp%" -af 'afade=t=out:st=1.5:d=0.3' /tmp/out-tmp.aif") infile) empty
  cp "/tmp/out-tmp.aif" infile

isNoteOn' :: (t, Message) -> Bool
isNoteOn' (_, msg) = isNoteOn msg

getPitch :: (t, Message) -> Key
getPitch (_, event) = key event

getOnset :: (t, t1) -> t
getOnset (onset, _) = onset

-- file = Midi.importFile "test_files/test.mid"

uncons :: [a] -> Maybe (a, [a])
uncons []     = Nothing
uncons (x:xs) = Just (x, xs)

defaultTempo = 120.0

onsetInSeconds :: (Integral a1, Fractional a) => a -> TimeDiv -> a1 -> a
onsetInSeconds tempo timeDiv tick =
  let (TicksPerBeat timeDiv') = timeDiv
  in fromIntegral tick /
     fromIntegral timeDiv' *
     (60.0 / tempo)

data SegmentationData =
  SegmentationData {
    segmentTime :: Double,
    pitches :: [Int]
  } deriving Show

getMidiData :: Either [Char] Midi -> SegmentationData
getMidiData midiFile' =
  let midiFile = case midiFile' of
        (Right midi) -> midi
        (Left msg) -> error msg
      (Midi _ timeDiv tracks)    = midiFile
      trackOne                   = case (uncons tracks) of
        Just (trackOne', _) -> trackOne'
        Nothing -> error "No track found"
      events                     = (filter isNoteOn' . toAbsTime) trackOne
      (firstOnset:secondOnset:_) = map (onsetInSeconds defaultTempo timeDiv . getOnset) events
      segmentTime                = (secondOnset - firstOnset)
      pitches                    = map getPitch events
  in SegmentationData segmentTime pitches

buildFileName :: Integral t => t -> Turtle.FilePath
buildFileName pitch =
  fromText $
  format (""%d%".aif") pitch

run :: MonadIO io => Turtle.FilePath -> Turtle.FilePath -> Turtle.FilePath -> io ()
run infileMidi infileAudio outDir = sh (do
    mktree outDir
    dir <- (mktempdir "/tmp" "samples")
    values <-
      let midiFile = Midi.importFile ((Text.unpack . format fp) infileMidi)
      in
        liftIO (getMidiData <$> midiFile)
    splitAudioFile infileAudio (segmentTime values) (dir </> "out")
    splitFiles <- fold (find (has "out") dir) Fold.list
    outFileNames <- fold (select (map buildFileName $ pitches values)) Fold.list
    (splitFile, outFileName) <- select (zip splitFiles outFileNames)
    trimFile splitFile
    mv splitFile (outDir </> outFileName)
    )

argParser :: Parser (FilePath, FilePath, FilePath)
argParser =
  (,,) <$>
  argPath "infileMidi" "midi" <*>
  argPath "infileAudio" "audio" <*>
  argPath "outDir" "outdir path"

-- TODO: .scl files
main :: IO ()
main = do
  (infileMidi, infileAudio, outDir) <- options "TODO" argParser
  run infileMidi infileAudio outDir

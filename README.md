# Tuning Sketchbook

A collection of scripts for creating and editing microtonal sampler presets.

## Status
WIP, pre-alpha; expect everything to change.

## Dependencies
- [Stack](https://github.com/commercialhaskell/stack)
- [FFmpeg](https://ffmpeg.org/)

## Installation / Usage
```
$ git clone https://github.com/quesebifurcan/tuning-sketchbook
$ stack runghc <path-to-script>
```

## Scripts
- `split_audio.hs` -- segment an audio file according to information in a midi file. Splits audio file at each `NoteOn` event in the midi file. Produces a collection of samples named after the corresponding pitches in the midi file.
- `make_sfz.hs` -- given a collection of (named) samples and a [`.scl`](http://www.huygens-fokker.org/scala/scl_format.html) file, produce a sampler preset (`.sfz`) which can be used in the digital audio workstation of your choice. By default, the resulting `.sfz` file maps the (microtonal) pitches to a chromatic scale.
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

#### split_audio.hs
Segment an audio file according to information in a midi file. Splits audio file at each `NoteOn` event in the midi file. Produces a collection of samples named after the corresponding pitches in the midi file.

#### make_sfz.hs
Given a collection of (named) samples and a [`.scl`](http://www.huygens-fokker.org/scala/scl_format.html) file, produce a sampler preset (`.sfz`) which can be used in the digital audio workstation of your choice. By default, the resulting `.sfz` file maps the (microtonal) pitches to a chromatic scale.

Example; tuning the [Leeds Town Hall Organ](https://www.samplephonics.com/products/free/sampler-instruments/the-leeds-town-hall-organ) to the scale used by La Monte Young in "Well-Tuned Piano".
  

```
$ stack runghc src/make_sfz.hs \
  test_files/young-lm_piano.scl \
  ~/Documents/TownHallOrgan_SP/Samples/PRE1 \
  sample-output \
  220
```

...results in this `.sfz` file:


```
<group>
loop_mode=no_loop
lovel=0
hivel=127

<region> trigger=attack pitch_keycenter=24 tune=0 lokey=24 hikey=24 sample=samples/K_68_Pre1_A2_RR2.wav
<region> trigger=attack pitch_keycenter=25 tune=-23 lokey=25 hikey=25 sample=samples/K_72_Pre1_B2_RR2.wav
<region> trigger=attack pitch_keycenter=26 tune=4 lokey=26 hikey=26 sample=samples/K_72_Pre1_B2_RR2.wav
<region> trigger=attack pitch_keycenter=27 tune=40 lokey=27 hikey=27 sample=samples/K_72_Pre1_B2_RR2.wav
<region> trigger=attack pitch_keycenter=28 tune=-29 lokey=28 hikey=28 sample=samples/K_78_Pre1_D3_RR2.wav
<region> trigger=attack pitch_keycenter=29 tune=44 lokey=29 hikey=29 sample=samples/K_76_Pre1_C#3_RR2.wav
<region> trigger=attack pitch_keycenter=30 tune=-25 lokey=30 hikey=30 sample=samples/K_82_Pre1_E3_RR2.wav
<region> trigger=attack pitch_keycenter=31 tune=2 lokey=31 hikey=31 sample=samples/K_82_Pre1_E3_RR2.wav
<region> trigger=attack pitch_keycenter=32 tune=38 lokey=32 hikey=32 sample=samples/K_82_Pre1_E3_RR2.wav
<region> trigger=attack pitch_keycenter=33 tune=-31 lokey=33 hikey=33 sample=samples/K_88_Pre1_G3_RR2.wav
<region> trigger=attack pitch_keycenter=34 tune=42 lokey=34 hikey=34 sample=samples/K_86_Pre1_F#3_RR2.wav
<region> trigger=attack pitch_keycenter=35 tune=-27 lokey=35 hikey=35 sample=samples/K_92_Pre1_A3_RR2.wav
<region> trigger=attack pitch_keycenter=36 tune=0 lokey=36 hikey=36 sample=samples/K_92_Pre1_A3_RR2.wav
```

**Note:** velocity-splitting, articulations and other nice features are not supported.
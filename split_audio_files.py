import argparse
import collections
import os
import subprocess
import shutil
import tempfile


DESCRIPTION = """
Segment an audio file into multiple parts.

Derive segment names and segment durations from pitches and durations
found in a midi file.

Run like this:

```
python split_audio_files.py \
  --infile-audio test_files/Untitled.aif \
  --infile-midi test_files/test.mid \
  --outdir ./out
```

Example: if --infile-midi contains four NoteOnEvents:

midi.NoteOnEvent(tick=0, channel=0, data=[60, 100])
midi.NoteOffEvent(tick=384, channel=0, data=[60, 64])
midi.NoteOnEvent(tick=384, channel=0, data=[64, 100])
midi.NoteOffEvent(tick=768, channel=0, data=[64, 64])
midi.NoteOnEvent(tick=768, channel=0, data=[68, 100])
midi.NoteOffEvent(tick=1152, channel=0, data=[68, 64])
midi.NoteOnEvent(tick=1152, channel=0, data=[72, 100])
midi.NoteOffEvent(tick=1536, channel=0, data=[72, 64])

...four audio files will be produced in --outdir.
Their names will correspond to the pitch of the NoteOnEvents, e.g.:

out
|-- 60.aif
|-- 64.aif
|-- 68.aif
`-- 72.aif

Requires ffmpeg, python-midi (on branch feature/python3)
"""


FADE_DURATION = 0.3
EARLY_CUTOFF = 0.1
MIN_SEGMENT_DURATION = 1


def trim(infile_path, segment_time):
    prefade_dur = segment_time - FADE_DURATION - EARLY_CUTOFF
    fade_dur = FADE_DURATION
    tempfile_path = './temp.aif'
    cmd_template = (
        'ffmpeg -y '
        '-i {infile_path} '
        '-af "afade=t=out:st={prefade_dur}:d={fade_dur}" '
        '{tempfile_path}'
    )
    cmd = cmd_template.format(
        infile_path=infile_path,
        prefade_dur=prefade_dur,
        fade_dur=fade_dur,
        tempfile_path=tempfile_path,
    )
    subprocess.call(cmd, shell=True)
    shutil.move(tempfile_path, infile_path)


def split_audio_file(infile_path, segment_times):
    _, file_extension = os.path.splitext(infile_path)
    if not file_extension == '.aif':
        msg = 'Expected an .aif file, got {}'
        raise Exception(msg.format(file_extension))
    if not shutil.which('ffmpeg'):
        raise Exception('Could not locate ffmpeg')
    cmd_template = (
        'ffmpeg -i {infile_path} '
        '-f segment -segment_times {segment_times} '
        '-c copy out-%03d.aif'
    )
    segment_times = ','.join([str(x) for x in segment_times])
    cmd = cmd_template.format(
        infile_path=infile_path,
        segment_times=segment_times,
    )
    subprocess.call(cmd, shell=True)


SegmentationData = collections.namedtuple('SegmentationData', [
    'split_points',
    'root_pitches',
    'segment_duration',
])


def get_segmentation_data(midi_file, ppq=96, tempo=120):
    import midi
    pattern = midi.read_midifile(midi_file)
    pattern.make_ticks_abs()
    midi_events = pattern[0]

    onset_filter = lambda event: isinstance(event, midi.NoteOnEvent)
    onset_events = list(filter(onset_filter, midi_events))

    # Segment duration
    segment_duration = (
        onset_in_seconds(onset_events[1].tick, ppq, tempo) -
        onset_in_seconds(onset_events[0].tick, ppq, tempo)
    )
    if segment_duration < MIN_SEGMENT_DURATION:
        msg = (
            'Segment duration can not be shorter than {min_segment_duration} '
            'second, please check that the intervals between successive '
            'NoteOnEvents in the midifile are not too short.'
        )
        raise Exception(msg.format(min_segment_duration=MIN_SEGMENT_DURATION))
    # Root pitches
    pitches = [event.pitch for event in onset_events]
    # Split points
    split_points = []
    curr_onset = 0
    for _ in pitches[:-1]:
        curr_onset += segment_duration
        split_points.append(curr_onset)
    return SegmentationData(
        segment_duration=segment_duration,
        root_pitches=pitches,
        split_points=split_points,
    )


def onset_in_seconds(tick, ppq, tempo):
    return tick / ppq * (60 / tempo)


def main():
    parser = argparse.ArgumentParser(
        description=DESCRIPTION,
        formatter_class=argparse.RawTextHelpFormatter,
    )
    parser.add_argument(
        '--infile-audio',
        dest='infile_audio',
        required=True,
    )
    parser.add_argument(
        '--infile-midi',
        dest='infile_midi',
        required=True,
    )
    parser.add_argument(
        '--outdir',
        dest='outdir',
        required=True,
    )
    args = parser.parse_args()

    infile_audio = os.path.abspath(args.infile_audio)
    assert os.path.isfile(infile_audio)

    infile_midi = os.path.abspath(args.infile_midi)
    assert os.path.isfile(infile_midi)

    outdir = os.path.abspath(args.outdir)
    os.makedirs(outdir, exist_ok=True)

    # Read onsets, root pitches and duration data from midi file
    segmentation_data = get_segmentation_data(infile_midi)

    with tempfile.TemporaryDirectory() as tmpdir:
        os.chdir(tmpdir)
        split_audio_file(infile_audio, segmentation_data.split_points)
        for split_file, pitch in zip(os.listdir('.'),
                                     segmentation_data.root_pitches):
            trim(split_file, segmentation_data.segment_duration)
            outfile = '{pitch}.aif'.format(pitch=pitch)
            basename = os.path.basename(split_file)
            shutil.move(basename, os.path.join(outdir, outfile))


if __name__ == '__main__':
    main()

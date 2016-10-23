import argparse
import collections
import os
import subprocess
import shutil
import tempfile


SPLIT_FILE_PREFIX = 'split-'
TRIMMED_FILE_PREFIX = 'trimmed-'
FADE_DURATION = 0.3
EARLY_CUTOFF = 0.1


def trim(infile_path, segment_time):
    prefade_dur = segment_time - FADE_DURATION - EARLY_CUTOFF
    fade_dur = FADE_DURATION
    basename = os.path.basename(infile_path)
    outfile_path = TRIMMED_FILE_PREFIX + basename
    cmd_template = (
        'ffmpeg -y '
        '-i {infile_path} '
        '-af "afade=t=out:st={prefade_dur}:d={fade_dur}" '
        '{outfile_path}'
    )
    cmd = cmd_template.format(
        infile_path=infile_path,
        prefade_dur=prefade_dur,
        fade_dur=fade_dur,
        outfile_path=outfile_path,
    )
    subprocess.call(cmd, shell=True)


def segment_audio_file(infile_path, segment_time):
    _, file_extension = os.path.splitext(infile_path)
    if not file_extension == '.aif':
        msg = 'Expected an .aif file, got {}'
        raise Exception(msg.format(file_extension))
    if not shutil.which('ffmpeg'):
        raise Exception('Could not locate ffmpeg')
    cmd_template = (
        'ffmpeg -i {infile_path} '
        '-f segment -segment_time {segment_time} '
        '-c copy {split_file_prefix}%03d.aif'
    )
    cmd = cmd_template.format(
        infile_path=infile_path,
        segment_time=segment_time,
        split_file_prefix=SPLIT_FILE_PREFIX,
    )
    subprocess.call(cmd, shell=True)
    for split_file in os.listdir('.'):
        if os.path.basename(split_file).startswith(SPLIT_FILE_PREFIX):
            trim(split_file, segment_time)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument(
        '--infile',
        dest='infile',
        required=True,
    )
    parser.add_argument(
        '--root-pitch',
        dest='root_pitch',
        default=60,
        required=False,
    )
    parser.add_argument(
        '--outdir',
        dest='outdir',
        required=True,
    )
    args = parser.parse_args()

    infile = os.path.abspath(args.infile)
    # TODO: get (sub)directory name from infile?
    outdir = os.path.abspath(args.outdir)
    os.makedirs(outdir, exist_ok=True)

    with tempfile.TemporaryDirectory() as tmpdir:
        os.chdir(tmpdir)
        # TODO: get interval/segment_time from config / file path
        segment_audio_file(infile, 2)
        curr_pitch = int(args.root_pitch)
        for file_ in os.listdir(tmpdir):
            basename = os.path.basename(file_)
            outfile = '{pitch}.aif'.format(pitch=curr_pitch)
            if basename.startswith(TRIMMED_FILE_PREFIX):
                shutil.move(basename, os.path.join(outdir, outfile))
                curr_pitch += 1


if __name__ == '__main__':
    main()

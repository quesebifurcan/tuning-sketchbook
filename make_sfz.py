import tuning

samples = [
    '60.aif',
    '62.aif',
    '64.aif',
    '66.aif',
    '68.aif',
    '70.aif',
    '72.aif',
]

base_freq = 200

# for x in tuning.scale_13_limit_JI():
#     print(float(base_freq * x))

import math

def hertz_to_midi(hertz):
    return 69. + (12. * math.log(hertz / 440., 2))

def closest_midi_pitch(candidates, hertz):
    midi = hertz_to_midi(hertz)
    return min(candidates, key=lambda x: abs(x - midi))

candidates = [60, 62, 64, 68]

print(closest_midi_pitch(candidates, 320))

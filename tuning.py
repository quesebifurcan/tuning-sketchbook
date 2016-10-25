import itertools
from fractions import Fraction

OCTAVE = Fraction(2, 1)


def limit_fraction(f):
    if f >= 2:
        return limit_fraction(f / 2)
    if f < 1:
        return limit_fraction(f * 2)
    return f


def build_scale(ratios):
    combinations = itertools.combinations_with_replacement(ratios, 2)
    pitchclasses = []
    for x, y in combinations:
        pitchclasses.append(limit_fraction(x * y))
    return sorted(set(pitchclasses))


def scale_13_limit_JI():
    base_ratios = [
        Fraction(1, 1),
        Fraction(3, 2),
        Fraction(5, 4),
        Fraction(7, 4),
        # Fraction(11, 8),
        # Fraction(13, 8),
    ]
    inverted_ratios = [(1 / r) for r in base_ratios]
    return build_scale(base_ratios + inverted_ratios)

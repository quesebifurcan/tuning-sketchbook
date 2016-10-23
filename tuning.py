import itertools
from fractions import Fraction

OCTAVE = Fraction(2, 1)


def limit_fraction(f, limit):
    if f >= limit:
        return limit_fraction(f / limit, limit)
    return f


def build_scale(ratios):
    combinations = itertools.combinations_with_replacement(ratios, 2)
    pitchclasses = []
    for x, y in combinations:
        pitchclasses.append(limit_fraction(x * y, OCTAVE))
    return sorted(set(pitchclasses))


def main():
    base_ratios = (
        Fraction(1, 1),
        Fraction(9, 8),
        Fraction(5, 4),
        Fraction(11, 8),
        Fraction(3, 2),
        Fraction(13, 8),
        Fraction(7, 4),
    )
    scale = build_scale(base_ratios)
    print(scale)


if __name__ == '__main__':
    main()

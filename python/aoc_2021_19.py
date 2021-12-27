"""Advent of Code 2021 - Day 19."""

from __future__ import annotations

from doctest import testmod
from itertools import chain
from typing import Iterable, Optional, Tuple

# from enum import Enum, auto
# from sys import stdin


from Coord import Coord3


class Rotation:
    """90 degree rotations of axes (which remain right-handed)."""

    def __init__(self, a: Coord3, b: Coord3):
        self.a = a
        self.b = b
        self.c = Coord3(
            a.y * b.z - a.z * b.y, a.z * b.x - a.x * b.z, a.x * b.y - a.y * b.x
        )

    def rotate(self, v: Coord3) -> Coord3:
        """Rotate the vector."""
        return Coord3(
            self.a.x * v.x + self.b.x * v.y + self.c.x * v.z,
            self.a.y * v.x + self.b.y * v.y + self.c.y * v.z,
            self.a.z * v.x + self.b.z * v.y + self.c.z * v.z,
        )

    @staticmethod
    def all_rotations() -> Iterable[Rotation]:

        dirs = [Coord3(1, 0, 0), Coord3(0, 1, 0), Coord3(0, 0, 1)]

        def flips(a: Coord3, b: Coord3) -> list[tuple[Coord3, Coord3]]:
            return [(a, b), (-a, b), (a, -b), (-a, -b)]

        return (
            Rotation(a, b)
            for a, b in chain(
                *[
                    flips(x_dir, y_dir)
                    for x_dir in dirs
                    for y_dir in dirs
                    if y_dir != x_dir
                ]
            )
        )


class Scanner:
    def __init__(self, ss: Optional[list[str]] = None):
        if ss is None:
            self._number: int = -1
            self._beacons: list[Coord3] = []
        else:
            assert len(ss) > 0, "Empty list making Scanner"
            assert ss[0].startswith("--- scanner "), (
                "Scanner initial line missing prefix: '" + ss[0] + "'"
            )
            assert ss[0].endswith(" ---"), (
                "Scanner initial line missing suffix: '" + ss[0] + "'"
            )
            self._number = int(ss[0].removeprefix("--- scanner ").removesuffix(" ---"))
            self._beacons = [
                Coord3(*[int(i) for i in row.split(",")]) for row in ss[1:]
            ]

    def rotated(self, r: Rotation) -> Scanner:
        """Return a copy of the scanner after applying the rotation."""
        other = Scanner()
        other._number = self._number
        other._beacons = [r.rotate(b) for b in self._beacons]
        return other

    def is_match(self, other: Scanner, offset: Coord3, min_match: int) -> bool:
        """Test if other scanner matches with the given offset and at least `min_match` beacons."""
        matches_found: int = 0
        # print("Testing self", [b.as_tuple() for b in self._beacons])
        # print("Versus others", [b.as_tuple() for b in other._beacons])
        for p in self._beacons:
            for q in other._beacons:
                if q + offset == p:
                    matches_found += 1
                    break
        return matches_found >= min_match

    def find_match(
        self, other: Scanner, min_match: int
    ) -> Optional[Tuple[Coord3, Rotation]]:
        """Return relative position of other scanner if a match with at least `min_match` beacons found.

        >>> test1[0].find_match(test1[1], 3)[0]
        Coord3(x=5, y=2, z=0)
        >>> all(test2[0].find_match(s, 6) is not None for s in test2[1:])
        True
        """
        for r in Rotation.all_rotations():
            other_rotated: Scanner = other.rotated(r)
            for p in self._beacons:
                for q in other_rotated._beacons:
                    offset = p - q
                    if self.is_match(other_rotated, offset, min_match):
                        return offset, r
        return None


def beacons(scanners: list[Scanner], min_match: int) -> list[Coord3]:
    """Return all the beacons found from combining the scanners."""
    pass


test1: list[Scanner] = [
    Scanner(["--- scanner 0 ---", "0,2,0", "4,1,0", "3,3,0"]),
    Scanner(["--- scanner 1 ---", "-1,-1,0", "-5,0,0", "-2,1,0"]),
]

test2: list[Scanner] = [
    Scanner(
        [
            "--- scanner 0 ---",
            "-1,-1,1",
            "-2,-2,2",
            "-3,-3,3",
            "-2,-3,1",
            "5,6,-4",
            "8,0,7",
        ]
    ),
    Scanner(
        [
            "--- scanner 0 ---",
            "1,-1,1",
            "2,-2,2",
            "3,-3,3",
            "2,-1,3",
            "-5,4,-6",
            "-8,-7,0",
        ]
    ),
    Scanner(
        [
            "--- scanner 0 ---",
            "-1,-1,-1",
            "-2,-2,-2",
            "-3,-3,-3",
            "-1,-3,-2",
            "4,6,5",
            "-7,0,8",
        ]
    ),
    Scanner(
        [
            "--- scanner 0 ---",
            "1,1,-1",
            "2,2,-2",
            "3,3,-3",
            "1,3,-2",
            "-4,-6,5",
            "7,0,8",
        ]
    ),
    Scanner(
        ["--- scanner 0 ---", "1,1,1", "2,2,2", "3,3,3", "3,1,2", "-6,-4,-5", "0,7,-8"]
    ),
]


test3: list[Scanner] = [
    Scanner(
        [
            "--- scanner 0 ---",
            "404,-588,-901",
            "528,-643,409",
            "-838,591,734",
            "390,-675,-793",
            "-537,-823,-458",
            "-485,-357,347",
            "-345,-311,381",
            "-661,-816,-575",
            "-876,649,763",
            "-618,-824,-621",
            "553,345,-567",
            "474,580,667",
            "-447,-329,318",
            "-584,868,-557",
            "544,-627,-890",
            "564,392,-477",
            "455,729,728",
            "-892,524,684",
            "-689,845,-530",
            "423,-701,434",
            "7,-33,-71",
            "630,319,-379",
            "443,580,662",
            "-789,900,-551",
            "459,-707,401",
        ]
    ),
    Scanner(
        [
            "--- scanner 1 ---",
            "686,422,578",
            "605,423,415",
            "515,917,-361",
            "-336,658,858",
            "95,138,22",
            "-476,619,847",
            "-340,-569,-846",
            "567,-361,727",
            "-460,603,-452",
            "669,-402,600",
            "729,430,532",
            "-500,-761,534",
            "-322,571,750",
            "-466,-666,-811",
            "-429,-592,574",
            "-355,545,-477",
            "703,-491,-529",
            "-328,-685,520",
            "413,935,-424",
            "-391,539,-444",
            "586,-435,557",
            "-364,-763,-893",
            "807,-499,-711",
            "755,-354,-619",
            "553,889,-390",
        ]
    ),
    Scanner(
        [
            "--- scanner 2 ---",
            "649,640,665",
            "682,-795,504",
            "-784,533,-524",
            "-644,584,-595",
            "-588,-843,648",
            "-30,6,44",
            "-674,560,763",
            "500,723,-460",
            "609,671,-379",
            "-555,-800,653",
            "-675,-892,-343",
            "697,-426,-610",
            "578,704,681",
            "493,664,-388",
            "-671,-858,530",
            "-667,343,800",
            "571,-461,-707",
            "-138,-166,112",
            "-889,563,-600",
            "646,-828,498",
            "640,759,510",
            "-630,509,768",
            "-681,-892,-333",
            "673,-379,-804",
            "-742,-814,-386",
            "577,-820,562",
        ]
    ),
    Scanner(
        [
            "--- scanner 3 ---",
            "-589,542,597",
            "605,-692,669",
            "-500,565,-823",
            "-660,373,557",
            "-458,-679,-417",
            "-488,449,543",
            "-626,468,-788",
            "338,-750,-386",
            "528,-832,-391",
            "562,-778,733",
            "-938,-730,414",
            "543,643,-506",
            "-524,371,-870",
            "407,773,750",
            "-104,29,83",
            "378,-903,-323",
            "-778,-728,485",
            "426,699,580",
            "-438,-605,-362",
            "-469,-447,-387",
            "509,732,623",
            "647,635,-688",
            "-868,-804,481",
            "614,-800,639",
            "595,780,-596",
        ]
    ),
    Scanner(
        [
            "--- scanner 4 ---",
            "727,592,562",
            "-293,-554,779",
            "441,611,-461",
            "-714,465,-776",
            "-743,427,-804",
            "-660,-479,-426",
            "832,-632,460",
            "927,-485,-438",
            "408,393,-506",
            "466,436,-512",
            "110,16,151",
            "-258,-428,682",
            "-393,719,612",
            "-211,-452,876",
            "808,-476,-593",
            "-575,615,604",
            "-485,667,467",
            "-680,325,-822",
            "-627,-443,-432",
            "872,-547,-609",
            "833,512,582",
            "807,604,487",
            "839,-516,451",
            "891,-625,532",
            "-652,-548,-490",
            "30,-46,-14",
        ]
    ),
]

if __name__ == "__main__":
    testmod()

    print(test3[0].find_match(test3[1], 12))

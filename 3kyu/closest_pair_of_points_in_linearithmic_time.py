# https://www.codewars.com/kata/5376b901424ed4f8c20002b7

from math import dist
from itertools import combinations

def closest_pair(points):
    sorted_by_x = sorted(points, key=lambda p: p[0])
    pair, _ = closest_pair_by_xy(sorted_by_x)
    return pair

def closest_pair_by_xy(points):
    size = len(points)
    if size <= 3: return closest_pair_between(points)

    middle_id = size // 2
    mx, my = points[middle_id]
    left_min_pair, dl = closest_pair_by_xy(points[0:middle_id])
    right_min_pair, dr = closest_pair_by_xy(points[middle_id:size])
    
    strip = [p for p in points if abs(p[0]-mx) < min(dl, dr)]
    middle_min_pair, dm = closest_pair_between(strip)
    return min((left_min_pair, dl), (right_min_pair, dr), (middle_min_pair, dm), key=lambda p: p[1])

def closest_pair_between(points):
    min_d = float('inf')
    min_p, min_q = [], []
    for p, q in combinations(points, 2):
        d = dist(p, q)
        if d < min_d:
            min_p, min_q, min_d = p, q, d
    return (min_p, min_q), min_d


# TESTS
def verify(points, expected):
    actual = closest_pair(points)
    if not actual or len(actual) != 2 or not all(pt and isinstance(pt,tuple) and len(pt)==2 for pt in actual):
        print(f"Output should be a tuple or list of tuples: ((a,b), (x,y)), but got {actual}")
    else:
        exp,act = (sorted(stuff) for stuff in (expected, actual))
        assert act == exp

def it1():
    points = (
        (2, 2), # A
        (2, 8), # B
        (5, 5), # C
        (6, 3), # D
        (6, 7), # E
        (7, 4), # F
        (7, 9)  # G
    )
    expected = ((6, 3), (7, 4))
    verify(points, expected)
    
def it2():
    points   = ((2, 2), (6, 3))
    expected = points
    verify(points, expected)
    
def it3():
    points =(
        (2, 2), # A
        (2, 8), # B
        (5, 5), # C
        (5, 5), # C
        (6, 3), # D
        (6, 7), # E
        (7, 4), # F
        (7, 9)  # G
    )
    expected =((5, 5), (5, 5))
    verify(points, expected)

if __name__ == '__main__':
    print("Example of the description")
    it1()
    print("Two points")
    it2()
    print("Duplicated points")
    it3()

# https://www.codewars.com/kata/5657d8bdafec0a27c800000f

def ccw_turn(ax, ay, bx, by, cx, cy):
    ux, uy = (bx - ax), (by - ay)
    vx, vy = (cx - bx), (cy - by)
    return ux*vy - uy*vx

def half_hull(sorted_points):
    S = []
    for p in sorted_points:
        while len(S) > 1 and not ccw_turn(*S[-2], *S[-1], *p) > 0:
            S.pop()
        S.append(p)
    S.pop()
    return S

def hull_method(pointlist):
    sorted_points = sorted(pointlist)
    return half_hull(sorted_points) + half_hull(reversed(sorted_points))

if __name__ == '__main__':
    TESTS = [ ([[0, 0], [5, 3], [0, 5]],                         [[0, 0], [0, 5], [5, 3]]),
              ([[0, 0], [5, 3], [0, 5], [2, 3]],                 [[0, 0], [0, 5], [5, 3]]),
              ([[0, 0], [5, 3], [0, 5], [0, 3]],                 [[0, 0], [0, 5], [5, 3]]),
              ([[0, 0], [5, 3], [0, 5], [5, 3]],                 [[0, 0], [0, 5], [5, 3]]),
              ([[0, 0], [5, 3], [0, 5], [0, 3], [2, 3], [5, 3]], [[0, 0], [0, 5], [5, 3]])
            ]

    for test, expt in TESTS:
        actual = sorted(hull_method(test))
        if actual != expt:
            print(f"expected {expt}, but was {actual}")

# https://www.codewars.com/kata/59c1d64b9f0cbcf5740001ab

def angle3(a, b, c):
    u = ((b[0] - a[0]),(b[1] - a[1]))
    v = ((c[0] - b[0]),(c[1] - b[1]))
    return u[0]*v[1] - u[1]*v[0]

def get_slope(p1, p2): 
    if p1[0] == p2[0]:
        return float('inf')
    return (p1[1]-p2[1])/(p1[0]-p2[0])

# using graham scan algorithm
# https://en.wikipedia.org/wiki/Graham_scan
def convex_hull_points(points):
    points.sort(key = lambda p: [p[0], p[1]] )
    p0 = points.pop(0)
    points.sort(key=lambda p: (get_slope(p,p0), -p[1],p[0]))
    S = [p0, points[0]]
    for i in range(1, len(points)):
        while(angle3(S[-2], S[-1], points[i]) < 0):
            S.pop()
        S.append(points[i])
    return S

def polygon_area(points):
    area = 0
    for i in range(len(points)):
        x0, y0 = points[i-1]
        x1, y1 = points[i]
        area += x0*y1 - y0*x1
    return 0.5*area

def convex_hull_area(points):
    if len(points) < 3:
        return 0
    convex_points = convex_hull_points(list(points))
    area = polygon_area(convex_points)
    return round(area,2)

if __name__ == '__main__':
    tests_array = [
       ({(0,0), (0,3), (4,0)}, 6),
       ({(0, 0), (0, 2), (2, 0), (2, 2)}, 4),
       ({(1, 1), (-1, 1), (-2, 0), (-1, -1), (1, -1), (2, 0)}, 6),
       ({(0, 0), (0, 3), (4, 0), (1, 1)}, 6),
       ({(0, 0), (0, 2), (2, 0), (2, 2), (1, 1)}, 4),
       ({(1, 1), (-1, 1), (-2, 0), (-1, -1), (1, -1), (2, 0), (0, 0)}, 6),
       ({(0, 0), (0, 1), (1, 0), (1, 1), (2, 0), (2, 1), (3, 0)}, 2.5),
       ({(0, 5), (5, 0), (-5, 0), (-3, -5), (3, -5), (0, 3), (3, 0), (-3, 0), (-1, -3), (1, -3)}, 65),
       ({(0, 0), (0, 1), (0, 2), (0, 3), (0, 4), (0, 5)}, 0),]
    for i, (input_data, answer) in enumerate(tests_array):
        result = convex_hull_area(input_data)
        if result == answer:
            print('{} test correct'.format(i+1))
        else:
            print('{} test failed\nexpected: {}, but was: {}'.format(i+1, answer, result))

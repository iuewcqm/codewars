# https://www.codewars.com/kata/5908242330e4f567e90000a3

from numpy import*;circleIntersection=lambda a,b,r:int(r*r*(lambda x:x-sin(x))(2*arccos(min(1,linalg.norm(array(a)-array(b))/2/r))))

# from math import *
# def circleIntersection(a,b,r):
#     length = hypot(a[0]-b[0], a[1]-b[1])
#     angle = length/2/r
#     if angle > 1:
#         return 0
#     x = 2*acos(angle)
#     d = x-sin(x)
#     result = int(r*r*d)
#     print(result)
#     return result

print(x := circleIntersection([0, 0],[7, 0],5), x == 14)
print(x := circleIntersection([0, 0],[0, 10],10), x == 122)
print(x := circleIntersection([5, 6],[5, 6],3), x == 28)
print(x := circleIntersection([-5, 0],[5, 0],3), x == 0)
print(x := circleIntersection([10, 20],[-5, -15],20), x == 15)
print(x := circleIntersection([-7, 13],[-25, -5],17), x == 132)
print(x := circleIntersection([-20, -4],[-40, 29],7), x == 0)
print(x := circleIntersection([38, -18],[46, -29],10), x == 64)
print(x := circleIntersection([-29, 33],[-8, 13],15), x == 5)
print(x := circleIntersection([-12, 20],[43, -49],23), x == 0)

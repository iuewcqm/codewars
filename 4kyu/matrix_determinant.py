# https://www.codewars.com/kata/52a382ee44408cea2500074c

def submatrix(m, x):
    return [m[i][:x]+m[i][x+1:] for i in range(1, len(m))]

def determinant(m):
    if len(m) == 1: return m[0][0]
    det = 0
    for i, a in enumerate(m[0]):
        subdet = determinant(submatrix(m, i))
        det += a * (-1)**i * subdet
    return det

if __name__ == '__main__':
    m1 = [[4,6],
          [3,8]]
    
    m2 = [[2,4,2],
          [3,1,1],
          [1,2,0]]
    
    m3 = [[1,2,3,4],
          [1,2,3,4],
          [1,2,3,4],
          [1,2,3,4]]
    
    print(determinant([[5]]), 5)
    print(determinant(m1), 14)
    print(determinant(m2), 10)
    print(determinant(m3), 0)

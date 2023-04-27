# https://www.codewars.com/kata/555b1890a75b930e63000023

def sub_arr(n, m=1):
    for i in range(m, n//2+1):
        for j in sub_arr(n-i, i):
            yield [i] + j
    yield [n]
    
def combos(n):
    return list(sub_arr(n))

if __name__ == '__main__':
    print(combos(1))
    print(combos(2))
    print(combos(5))
    print(combos(10))

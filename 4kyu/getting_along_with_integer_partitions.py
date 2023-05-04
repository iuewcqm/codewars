# https://www.codewars.com/kata/55cf3b567fc0e02b0b00000b

from math import prod as product
from statistics import mean, median

def enum(n, m=1):
    yield [n]
    for i in range(m, n//2+1):
        for j in enum(n-i, i):
            yield [i] + j

def prod(n):
    return set(sorted([product(p) for p in enum(n)]))

def part(n):
    p = list(prod(n))
    return 'Range: {} Average: {:.2f} Median: {:.2f}'.format(max(p)-min(p), mean(p), median(p))

if __name__ == '__main__':
    print(part(3))
    print(part(5))
    print(part(50))

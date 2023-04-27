# https://www.codewars.com/kata/555b1890a75b930e63000023

def combos(n):
    used = []
    def sub_arr(n, comb=[]):
        for i in range(1, n):
            yield from sub_arr(n-i, comb+[i])
        comb.append(n)
        comb.sort()
        if comb not in used:
            used.append(comb)
            yield comb
    return list(sub_arr(n))

if __name__ == '__main__':
    print(combos(1))
    print(combos(2))
    print(combos(5))
    print(combos(10))

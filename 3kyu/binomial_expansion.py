# https://www.codewars.com/kata/540d0fdd3b6532e5c3000b5b

from math import factorial
import re

SPLIT_RE = re.compile(r"\(([-\d+]*)([a-z])\+*(\-*\d+)\)")


def binom(n, k):
    return factorial(n) / \
        (factorial(k)*factorial(n-k))


def split_terms(expr):
    c, x, y = SPLIT_RE.split(expr)[1:-1]
    if c == '':
        c = 1
    elif c == '-':
        c = -1
    c = int(c)
    y = int(y)
    return c, x, y


def expand(expr):
    expr = expr.split('^')
    expr, n = expr[0], int(expr[1])
    if n == 0:
        return "1"

    c, x, y = split_terms(expr)
    terms = []
    for k in range(n+1):
        exp = n-k
        kx = f"{x}^{exp}"
        if exp == 1:
            kx = f"{x}"
        elif exp == 0:
            kx = ""
        r = int(binom(n, k) * c**exp * y**k)
        kr = f"+{r}" if r > 0 else f"{r}"
        if r == 1 and exp != 0:
            kr = "+"
        elif r == -1 and exp != 0:
            kr = "-"
        terms.append(kr+kx)

    expanded = ''.join(terms)
    if expanded.startswith('+'):
        expanded = expanded[1:]
    return expanded


if __name__ == "__main__":
    print(expand("(x+1)^0") == "1")
    print(expand("(x+1)^1") == "x+1")
    print(expand("(x+1)^2") == "x^2+2x+1")
    print(expand("(x-1)^0") == "1")
    print(expand("(x-1)^1") == "x-1")
    print(expand("(x-1)^2") == "x^2-2x+1")
    print(expand("(5m+3)^4") == "625m^4+1500m^3+1350m^2+540m+81")
    print(expand("(2x-3)^3") == "8x^3-36x^2+54x-27")
    print(expand("(7x-7)^0") == "1")
    print(expand("(-5m+3)^4") == "625m^4-1500m^3+1350m^2-540m+81")
    print(expand("(-2k-3)^3") == "-8k^3-36k^2-54k-27")
    print(expand("(-7x-7)^0") == "1")

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


def compute_var(x, n, k):
    exp = n-k
    var = f"{x}^{exp}"
    if exp == 1:
        var = str(x)
    elif exp == 0:
        var = ""
    return var


def compute_coeff(c, y, n, k):
    exp = n-k
    res = int(binom(n, k) * c**exp * y**k)
    coeff = f"+{res}" if res > 0 else str(res)
    if res == 1 and exp != 0:
        coeff = "+"
    elif res == -1 and exp != 0:
        coeff = "-"
    return coeff


def build_term(c, x, y, n, k):
    coeff = compute_coeff(c, y, n, k)
    var = compute_var(x, n, k)
    return str(coeff)+var


def expand_by_coeffs(c, x, y, n):
    terms = []
    for k in range(n+1):
        term = build_term(c, x, y, n, k)
        terms.append(term)

    expanded = ''.join(terms)
    if expanded.startswith('+'):
        expanded = expanded[1:]
    return expanded


def expand(expr):
    expr = expr.split('^')
    expr, n = expr[0], int(expr[1])
    if n == 0:
        return "1"

    c, x, y = split_terms(expr)
    return expand_by_coeffs(c, x, y, n)


if __name__ == "__main__":
    TESTS = [
        ("(x+1)^0", "1"),
        ("(x+1)^1", "x+1"),
        ("(x+1)^2", "x^2+2x+1"),
        ("(x-1)^0", "1"),
        ("(x-1)^1", "x-1"),
        ("(x-1)^2", "x^2-2x+1"),
        ("(5m+3)^4", "625m^4+1500m^3+1350m^2+540m+81"),
        ("(2x-3)^3", "8x^3-36x^2+54x-27"),
        ("(7x-7)^0", "1"),
        ("(-5m+3)^4", "625m^4-1500m^3+1350m^2-540m+81"),
        ("(-2k-3)^3", "-8k^3-36k^2-54k-27"),
        ("(-7x-7)^0", "1"),
    ]

    for expr, answ in TESTS:
        actual = expand(expr)
        if actual != answ:
            print(f"Expected:\n\t{answ}, but was:\n\t{actual}")
        else:
            print('Ok')

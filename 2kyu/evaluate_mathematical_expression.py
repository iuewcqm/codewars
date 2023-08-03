# https://www.codewars.com/kata/52a78825cdfc2cfc87000005

import re
from operator import add, sub, mul, truediv as div

OPS = {
    '+': add, 
    '-': sub,
    '*': mul,
    '/': div
      } 

tokenizer = re.compile(r'[.\d]+|[()\+\-\*\/]')

def calc(expr):
    tokens = tokenizer.findall(expr)
    res, _ = expression(tokens, 0)
    return res
    
def expression(tokens, id):
    value, id = term(tokens, id)

    while id < len(tokens) and tokens[id] != ')':
        tok = tokens[id]
        if tok in '+-':
            next, id = term(tokens, id+1)
            value = OPS[tok](value, next)

    return value, id

def term(tokens, id):
    value, id = factor(tokens, id)

    while id < len(tokens) and tokens[id] in '*/':
        op = tokens[id]
        next, id = factor(tokens, id+1)
        value = OPS[op](value, next)

    return value, id

def factor(tokens, id):
    tok = tokens[id]

    if tok == '(':
        value, id = expression(tokens, id+1)
    elif tok == '-':
        value, id = factor(tokens, id+1)
        value, id = -value, id-1
    else:
        value = float(tok)

    return value, id+1
    
### TESTS
if __name__ == '__main__':
    TESTS = [("1 + 1", 2),
             ("8/16", 0.5),
             ("2 + 2 + 2 - 2", 4),
             ("2 * 2 + 2", 6),
             ("2 + 2 - 2", 2),
             ("(2 + 3) * 2 + 7 * 3", 31),
             ("(2 + 2) * 2", 8),
             ("-(2 + 2) * 2", 8),
             ("1.5 * -1.5", -2.25),
             ("3 -(-1)", 4),
             ("2 + -2", 0),
             ("10- 2- -5", 13),
             ("(((10)))", 10),
             ("-7 * -(6 / 3)", 14)
             ]
    for exp, ans in TESTS:
        actual = calc(exp)
        if actual == ans:
            print(f"{exp} ok")
        else:
            print(f"{exp} expected: {ans}, but was: {actual}")

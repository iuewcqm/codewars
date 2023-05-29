# https://www.codewars.com/kata/581bc0629ad9ff9873000316

import re

regex = re.compile(r'[0-9]+\.[0-9]+|[0-9]+|[\+\-\*\$]')
valid = set('0123456789.+-*$')

def expression(tokens):
    x = term(tokens)
    if len(tokens) > 1:
        if tokens[1] == '+': return x + expression(tokens[2:])
        if tokens[1] == '-': return x - expression(tokens[2:])
    return x


def term(tokens):
    number = float(tokens[0])
    if len(tokens) > 1:
        if tokens[1] == '*': return number * term(tokens[2:])
        if tokens[1] == '$': return number / term(tokens[2:])
    return number

def calculate(expr):
    print(expr)
    if len(set(expr) | valid) > len(valid):
        return "400: Bad request"

    tokens = regex.findall(expr)
    return expression(tokens)


if __name__ == '__main__':
    cases = (
        ("1.1", 1.1),                 # returns the number if no commands given
        ("10+5", 15),                 # addition
        ("8-2", 6),                   # subtraction
        ("4*3", 12),                  # muliplication
        ("18$2", 9),                  # division
        ("5+8-8*2$4", 9),             # multiple commands
        ("3x+1", "400: Bad request")  # handles incorrect input
    )

    for expr, answ in cases:
        actual = calculate(expr)
        if actual != answ:
            print(f'In "{expr}". Expected {answ}, actual {actual}')

# https://www.codewars.com/kata/5647c3858d4acbbe550000ad

# solution
PATTERN = r'^(0|1(10)*(0|11)(01*01|01*00(10)*(0|11))*1)+$'
# ---

# tests
import re

def test(actual, expected, message):
    if actual != expected: print(message)

tests = [(False, "" ),
         (False, "abc"),
         (True,  "000"),
         (True,  "101"),
         (True,  "1010"),
         (True,  "10100"),
         (True,  "{:b}".format(65020)),
         (True,  "{:b}".format(6039865)),
         
         (False, "10110101"),
         (False, "1110001"),
         
         (False,  "{:b}".format(21)),
         (False,  "{:b}".format(15392)),
         (False,  "{:b}".format(23573)),
         (False,  "{:b}".format(19344)),
         
         (False,  "{:b}".format(43936)),
         (False,  "{:b}".format(32977)),
         (False,  "{:b}".format(328)),
         (False,  "{:b}".format(5729)),
        ]

for exp,s in tests:
    test(bool(re.match(PATTERN, s)), exp, "Should work with '{}'".format(s))

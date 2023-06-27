# https://www.codewars.com/kata/54de279df565808f8b00126a

import re

# solution
PATTERN = re.compile(r'^((0|11)|10(1|00)*01)*$')
# ---

# tests
tests = [
        (" 0" , False, "The input should consist only of binary digits - no spaces"),
        ("abc", False, "The input should consist only of binary digits - no alpha characters"),
        ("000", True,  "There might be leading `0`s"),
        ("110", True,  "Should match numbers divisible by 3"),
        ("111", False, "Should not match numbers not divisible by 3"),
        ("{:b}".format(12345678), True, "Should work with larger numbers"),
        ]

for test, result, message in tests:
    actual = PATTERN.match(test)
    actual = False if actual else True
    if not actual:
        print(message)

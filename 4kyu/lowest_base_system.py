# https://www.codewars.com/kata/58bc16e271b1e4c5d3000151

def get_min_base(number):
    for c in range(3, number.bit_length()+1):
        base = int(number**(1/(c-1)))
        if ((base**c-1)//(base-1))==number:
            return base
    return number-1

print(get_min_base(3))
print(get_min_base(7))
print(get_min_base(21))
print(get_min_base(57))
print(get_min_base(58))
print(get_min_base(1111))

# https://www.codewars.com/kata/52cf02cd825aef67070008fa

pattern = '?.6YIcflxVC5WE94UA1OoD70MkvRuPqHabdhpF,82QsLirJejtNmzZKgnB3SwTyXG '

def decode(s):
    return ''.join([pattern[(pattern.index(s[i])-i-1)%len(pattern)] if s[i] in pattern else s[i] for i in range(len(s))])

print(decode("atC5kcOuKAr!"))

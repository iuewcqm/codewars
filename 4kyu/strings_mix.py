# https://www.codewars.com/kata/5629db57620258aa9d000014

from string import ascii_lowercase as alphabet

def mix(s1, s2):
    letters = (set(s1) | set(s2)) & set(alphabet)
    mixed = [(ch, s1.count(ch), s2.count(ch)) for ch in letters]
    mixed = [(ch*max(x,y), '2=1'[(x>y)-(x<y)+1]) for ch, x, y in mixed if x > 1 or y > 1]
    mixed = [f"{prefix}:{seq}" for seq, prefix in mixed]
    mixed.sort(key=lambda x: (-len(x), x.lower()))
    return '/'.join(mixed)
    
if __name__ == "__main__":
    TESTS = [("Are they here", "yes, they are here", "2:eeeee/2:yy/=:hh/=:rr"),
             ("Sadus:cpms>orqn3zecwGvnznSgacs","MynwdKizfd$lvse+gnbaGydxyXzayp", '2:yyyy/1:ccc/1:nnn/1:sss/2:ddd/=:aa/=:zz'),
             ("looping is fun but dangerous", "less dangerous than coding", "1:ooo/1:uuu/2:sss/=:nnn/1:ii/2:aa/2:dd/2:ee/=:gg"),
             (" In many languages", " there's a pair of functions", "1:aaa/1:nnn/1:gg/2:ee/2:ff/2:ii/2:oo/2:rr/2:ss/2:tt"),
             ("Lords of the Fallen", "gamekult", "1:ee/1:ll/1:oo"),
             ("codewars", "codewars", ""),
             ("A generation must confront the looming ", "codewarrs", "1:nnnnn/1:ooooo/1:tttt/1:eee/1:gg/1:ii/1:mm/=:rr")]
    for s1, s2, expected in TESTS:
        actual = mix(s1, s2)
        if actual != expected:
            print(f"for ({s1}, {s2})\nexpected: '{expected}', but was: '{actual}'")

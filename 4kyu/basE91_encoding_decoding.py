# https://www.codewars.com/kata/58a57c6bcebc069d7e0001fe

table = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789!#$%&()*+,./:;<=>?@[]^_`{|}~"'
d_table = dict((v, k) for k, v in enumerate(table))


def b91decode(strng):
    output = ''
    v = -1
    b = n = 0
    for i in strng:
        if i not in d_table:
            continue
        c = d_table[i]
        if v < 0:
            v = c
        else:
            v += c * 91
            b |= v << n
            n += 13 if (v & 8191) > 88 else 14
            while True:
                output += chr(b & 255)
                b >>= 8
                n -= 8
                if n <= 7:
                    break
            v = -1
    if v+1:
        output += chr((b | v << n) & 255)
    return output


def b91encode(strng):
    output = ''
    b = n = 0
    for i in strng:
        b |= ord(i) << n
        n += 8
        v = 0
        if n > 13:
            v = b & 8191
            if v > 88:
                b >>= 13
                n -= 13
            else:
                v = b & 16383
                b >>= 14
                n -= 14
            output += table[v % 91] + table[v // 91]
    if n:
        output += table[b % 91]
        if n > 7 or b > 90:
            output += table[b // 91]
    return output


# tests
if __name__ == '__main__':
    print(b91encode('test') == 'fPNKd')
    print(b91encode('Hello World!') == '>OwJh>Io0Tv!8PE')
    print(b91decode('fPNKd') == 'test')
    print(b91decode('>OwJh>Io0Tv!8PE') == 'Hello World!')

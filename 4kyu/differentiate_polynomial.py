# https://www.codewars.com/kata/566584e3309db1b17d000027

def diff_x(token):
    k, d = token.split('x')
    k = 1 if k == '' else -1 if k == '-' else int(k)
    d = 1 if d == '' else int(d[1:])
    return lambda x: k*d * x**(d-1)

def tokenize(expr):
    tokenized = []
    for token in expr.split('+'):
        pos_tok, *neg_toks = token.split('-')
        if pos_tok != '': 
            tokenized.append(pos_tok)
        tokenized.extend(f'-{tok}' for tok in neg_toks)
    return tokenized

def differentiate(equation, point):
    result = 0
    for token in tokenize(equation):
        if 'x' in token:
            result += diff_x(token)(point)
    return result

if __name__ == '__main__':
    print(differentiate("12x+2", 3), 12)
    print(differentiate("x^2-x", 3), 5)
    print(differentiate("-5x^2+10x+4", 3), -20)

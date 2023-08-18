# https://www.codewars.com/kata/5265b0885fda8eac5900093b

### SOLUTION
import re

class Compiler(object):
    def compile(self, program):
        return self.pass3(self.pass2(self.pass1(program)))
        
    def tokenize(self, program):
        """turn a program string into an array of tokens.  each token
           is either '[', ']', '(', ')', '+', '-', '*', '/', a variable
           name or a number (as a string)"""
        token_iter = (m.group(0) for m in re.finditer(r'[-+*/()[\]]|[A-Za-z]+|\d+', program))
        return [int(tok) if tok.isdigit() else tok for tok in token_iter]

    input_args = []

    def pass1(self, program):
        """Returns an un-optimized AST"""
        tokens = self.tokenize(program)
        args_end_id = tokens.index(']')
        self.input_args = tokens[1:args_end_id]
        tokens = tokens[args_end_id+1:]

        ast = self.expression(tokens, 0)[0]
        return ast

    def expression(self, tokens, id):
        value, id = self.term(tokens, id)
        
        while id < len(tokens) and tokens[id] != ')':
            tok = tokens[id]
            if tok in '+-':
                next, id = self.term(tokens, id+1)
                value = { 'op': tok, 'a': value, 'b': next }
                
        return value, id

    def term(self, tokens, id):
        value, id = self.factor(tokens, id)

        while id < len(tokens) and tokens[id] in '*/':
            op = tokens[id]
            next, id = self.factor(tokens, id+1)
            value = { 'op': op, 'a': value, 'b': next }

        return value, id

    def factor(self, tokens, id):
        tok = tokens[id]

        if tok == '(':
            value, id = self.expression(tokens, id+1)
        elif tok in self.input_args:
            value = { 'op': 'arg', 'n': self.input_args.index(tok) }
        else:
            value = { 'op': 'imm', 'n': int(tok)} 

        return value, id+1

        
    def pass2(self, ast):
        """Returns an AST with constant expressions reduced"""
        pass

    def pass3(self, ast):
        """Returns assembly instructions"""
        pass


### TESTS

# syntax
#    function   ::= '[' arg-list ']' expression
#
#    arg-list   ::= /* nothing */
#                 | variable arg-list
#
#    expression ::= term
#                 | expression '+' term
#                 | expression '-' term
#
#    term       ::= factor
#                 | term '*' factor
#                 | term '/' factor
#
#    factor     ::= number
#                 | variable
#                 | '(' expression ')'

# ast
#  { 'op': '+', 'a': a, 'b': b }  -  add subtree a to subtree b
#  { 'op': '-', 'a': a, 'b': b }  -  subtract subtree b from subtree a
#  { 'op': '*', 'a': a, 'b': b }  -  multiply subtree a by subtree b
#  { 'op': '/', 'a': a, 'b': b }  -  divide subtree a from subtree b
#  { 'op': 'arg', 'n': n }        -  reference to n-th argument, n integer
#  { 'op': 'imm', 'n': n }        -  immediate value n, n integer

# assembly
#   "IM n" - load the constant value n into R0
#   "AR n" - load the n-th input argument into R0
#   "SW"   - swap R0 and R1
#   "PU"   - push R0 onto the stack
#   "PO"   - pop the top value off of the stack into R0
#   "AD"   - add R1 to R0 and put the result in R0
#   "SU"   - subtract R1 from R0 and put the result in R0
#   "MU"   - multiply R0 by R1 and put the result in R0
#   "DI"   - divide R0 by R1 and put the result in R0

def simulate(asm, argv):
    r0, r1 = None, None
    stack = []
    for ins in asm:
        if ins[:2] == 'IM' or ins[:2] == 'AR':
            ins, n = ins[:2], int(ins[2:])
        if ins == 'IM':   r0 = n
        elif ins == 'AR': r0 = argv[n]
        elif ins == 'SW': r0, r1 = r1, r0
        elif ins == 'PU': stack.append(r0)
        elif ins == 'PO': r0 = stack.pop()
        elif ins == 'AD': r0 += r1
        elif ins == 'SU': r0 -= r1
        elif ins == 'MU': r0 *= r1
        elif ins == 'DI': r0 /= r1
    return r0

if __name__ == "__main__":
    prog = '[ x y z ] ( 2*3*x + 5*y - 3*z ) / (1 + 3 + 2*2)';
    t1 = {'op':'/','a':{'op':'-','a':{'op':'+','a':{'op':'*','a':{'op':'*','a':{'op':'imm','n':2},'b':{'op':'imm','n':3}},'b':{'op':'arg','n':0}},'b':{'op':'*','a':{'op':'imm','n':5},'b':{'op':'arg','n':1}}},'b':{'op':'*','a':{'op':'imm','n':3},'b':{'op':'arg','n':2}}},'b':{'op':'+','a':{'op':'+','a':{'op':'imm','n':1},'b':{'op':'imm','n':3}},'b':{'op':'*','a':{'op':'imm','n':2},'b':{'op':'imm','n':2}}}};
    
    t2 = {'op':'/','a':{'op':'-','a':{'op':'+','a':{'op':'*','a':{'op':'imm','n':6},'b':{'op':'arg','n':0}},'b':{'op':'*','a':{'op':'imm','n':5},'b':{'op':'arg','n':1}}},'b':{'op':'*','a':{'op':'imm','n':3},'b':{'op':'arg','n':2}}},'b':{'op':'imm','n':8}};
        
    c = Compiler()
    
    p1 = c.pass1(prog)
    print(p1 == t1, 'Pass1')
    print(p1)
    
    # p2 = c.pass2(p1)
    # print(p2, t2, 'Pass2')
    
    # p3 = c.pass3(p2)
    
    # order_of_ops_prog = '[ x y z ] x - y - z + 10 / 5 / 2 - 7 / 1 / 7'
    # order_of_ops = c.pass3(c.pass2(c.pass1(order_of_ops_prog)))

    # print(order_of_ops)      

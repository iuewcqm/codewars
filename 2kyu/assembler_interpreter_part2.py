# https://www.codewars.com/kata/58e61f3d8ff24f774400002c

class Assembler():
    def __init__(self, program):
        self.rip = 0
        self.result_messages = []
        self.registers = {}
        self.labels = {}
        self.call_instruction_addrs = []
        self.compare_results = []
        self.instructions = {'mov':self.__mov, 'inc':self.__inc, 'dec':self.__dec, 'add':self.__add, 'sub':self.__sub, 'mul':self.__mul, 'div':self.__div, 'jmp':self.__jmp, 'cmp':self.__cmp, 'jne':self.__jne, 'je':self.__je, 'jge':self.__jge, 'jg':self.__jg, 'jle':self.__jle, 'jl':self.__jl, 'call':self.__call, 'ret':self.__ret, 'msg':self.__msg, 'end':self.__end }
        self.program = self.__parse_program(program)

    def __parse_program(self, program):
        instructs = []
        program = [inst.strip() for inst in program.split('\n')]
        program = [inst.partition(' ') for inst in program if inst != '' and not inst.startswith(';')]
        for func, _, args in program:
            if ':' in func:
                self.labels[func[:-1]] = len(instructs)
            else:
                instructs.append((self.instructions[func], self.__parse_arguments(args)))
        return instructs

    def __parse_arguments(self, args):
        args = args.split(';')[0]
        is_open_quote = False
        tokens = [""]
        for ch in args:
            if ch == "'":
                is_open_quote = not is_open_quote 
            elif ch == "," and not is_open_quote:
                tokens.append("")
            elif not (ch == " " and not is_open_quote):
                tokens[-1] += ch
        return tokens
    
    def exec(self):
        while self.rip >= 0:
            func, args = self.program[self.rip]
            func(args)
            if self.rip >= len(self.program): return -1
        return '\n'.join(self.result_messages)

    def __mov(self, args):
        x, y = args
        self.registers[x] = self.registers[y] if y in self.registers else int(y)
        self.rip += 1

    def __inc(self, args):
        self.registers[args[0]] += 1
        self.rip += 1

    def __dec(self, args):
        self.registers[args[0]] -= 1
        self.rip += 1

    def __add(self, args):
        x, y = args
        self.registers[x] += self.registers[y] if y in self.registers else int(y)
        self.rip += 1

    def __sub(self, args):
        x, y = args
        self.registers[x] -= self.registers[y] if y in self.registers else int(y)
        self.rip += 1

    def __mul(self, args):
        x, y = args
        self.registers[x] *= self.registers[y] if y in self.registers else int(y)
        self.rip += 1

    def __div(self, args):
        x, y = args
        self.registers[x] //= self.registers[y] if y in self.registers else int(y)
        self.rip += 1

    def __jmp(self, args):
        self.rip = self.labels[args[0]]

    def __cmp(self, args):
        x, y = args
        x = self.registers[x] if x in self.registers else int(x)
        y = self.registers[y] if y in self.registers else int(y)
        self.compare_results.append((x > y) - (x < y))
        self.rip += 1

    def __jne(self, args):
        if self.compare_results[-1] != 0:
            self.rip = self.labels[args[0]]
        else: self.rip += 1

    def __je(self, args):
        if self.compare_results[-1] == 0:
            self.rip = self.labels[args[0]]
        else: self.rip += 1

    def __jge(self, args):
        if self.compare_results[-1] >= 0:
            self.rip = self.labels[args[0]]
        else: self.rip += 1

    def __jg(self, args):
        if self.compare_results[-1] == 1:
            self.rip = self.labels[args[0]]
        else: self.rip += 1

    def __jle(self, args):
        if self.compare_results[-1] <= 0:
            self.rip = self.labels[args[0]]
        else: self.rip += 1

    def __jl(self, args):
        if self.compare_results[-1] == -1:
            self.rip = self.labels[args[0]]
        else: self.rip += 1

    def __call(self, args):
        self.call_instruction_addrs.insert(0, self.rip)
        self.rip = self.labels[args[0]]

    def __ret(self, args):
        self.rip = self.call_instruction_addrs.pop() + 1

    def __msg(self, args):
        args = [str(self.registers[arg]) if arg in self.registers else arg for arg in args]
        self.result_messages.append("".join(args))
        self.rip += 1

    def __end(self, args):
        self.rip = -1


def assembler_interpreter(program):
    assembler = Assembler(program)
    return assembler.exec()

if __name__ == '__main__':
    TESTS = [
    ("Any program...",
    '''
    ; My first program
    mov  a, 5
    inc  a
    call function
    msg  '(5+1)/2 = ', a    ; output message
    end

    function:
        div  a, 2
        ret
    ''', '(5+1)/2 = 3'),
    
    ("Factorial",
    '''
    mov   a, 5
    mov   b, a
    mov   c, a
    call  proc_fact
    call  print
    end

    proc_fact:
        dec   b
        mul   c, b
        cmp   b, 1
        jne   proc_fact
        ret

    print:
        msg   a, '! = ', c ; output text
        ret
    ''', '5! = 120'),

    ("Fibonacci", '''
    mov   a, 8            ; value
    mov   b, 0            ; next
    mov   c, 0            ; counter
    mov   d, 0            ; first
    mov   e, 1            ; second
    call  proc_fib
    call  print
    end

    proc_fib:
        cmp   c, 2
        jl    func_0
        mov   b, d
        add   b, e
        mov   d, e
        mov   e, b
        inc   c
        cmp   c, a
        jle   proc_fib
        ret

    func_0:
        mov   b, c
        inc   c
        jmp   proc_fib

    print:
        msg   'Term ', a, ' of Fibonacci series is: ', b        ; output text
        ret
    ''', 'Term 8 of Fibonacci series is: 21'),

    ('Modulo', '''
    mov   a, 11           ; value1
    mov   b, 3            ; value2
    call  mod_func
    msg   'mod(', a, ', ', b, ') = ', d        ; output
    end

    ; Mod function
    mod_func:
        mov   c, a        ; temp1
        div   c, b
        mul   c, b
        mov   d, a        ; temp2
        sub   d, c
        ret
    ''', 'mod(11, 3) = 2'),

    ('gcd', '''
    mov   a, 81         ; value1
    mov   b, 153        ; value2
    call  init
    call  proc_gcd
    call  print
    end

    proc_gcd:
        cmp   c, d
        jne   loop
        ret

    loop:
        cmp   c, d
        jg    a_bigger
        jmp   b_bigger

    a_bigger:
        sub   c, d
        jmp   proc_gcd

    b_bigger:
        sub   d, c
        jmp   proc_gcd

    init:
        cmp   a, 0
        jl    a_abs
        cmp   b, 0
        jl    b_abs
        mov   c, a            ; temp1
        mov   d, b            ; temp2
        ret

    a_abs:
        mul   a, -1
        jmp   init

    b_abs:
        mul   b, -1
        jmp   init

    print:
        msg   'gcd(', a, ', ', b, ') = ', c
        ret
    ''','gcd(81, 153) = 9'),

    ('Failing', '''
    call  func1
    call  print
    end

    func1:
        call  func2
        ret

    func2:
        ret

    print:
        msg 'This program should return -1'
    ''', -1),

    ('Power', '''
    mov   a, 2            ; value1
    mov   b, 10           ; value2
    mov   c, a            ; temp1
    mov   d, b            ; temp2
    call  proc_func
    call  print
    end

    proc_func:
        cmp   d, 1
        je    continue
        mul   c, a
        dec   d
        call  proc_func

    continue:
        ret

    print:
        msg a, '^', b, ' = ', c
        ret
    ''', '2^10 = 1024')
    ]

    for title, program, expected in TESTS:
        actual = assembler_interpreter(program)
        if actual == expected:
            print(f"{title}: {actual}")
        else:
            print(f"error in {title}\nexpected: {expected}, but was: {actual}")

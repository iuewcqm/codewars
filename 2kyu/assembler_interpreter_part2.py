# https://www.codewars.com/kata/58e61f3d8ff24f774400002c

class Assembler():
    def __init__(self, program):
        self.rip = 0
        self.result_messages = []
        self.registers = {}
        self.labels = {}
        self.call_instruction_addrs = []
        self.compare_results = []
        self.program = self.__parse_program(program)

    def __parse_program(self, program):
        instructs = []
        program = [inst.strip() for inst in program.split('\n')]
        program = [inst.partition(' ') for inst in program if inst != '' and not inst.startswith(';')]
        for func, _, args in program:
            if ':' in func:
                self.labels[func[:-1]] = len(instructs)
            else:
                instructs.append((func, *self.__parse_arguments(args)))
        return instructs

    def __parse_arguments(self, args):
        if args == "": return []
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
        while 0 <= self.rip < len(self.program):
            func, *args = self.program[self.rip]
            if func == "end": return '\n'.join(self.result_messages)
            getattr(self, func)(*args)
            self.rip += 1
        return -1

    def __get_value(self, reg):
        return self.registers[reg] if reg in self.registers else int(reg)

    def mov(self, x, y):
        self.registers[x] = self.__get_value(y)

    def inc(self, x):
        self.registers[x] += 1

    def dec(self, x):
        self.registers[x] -= 1

    def add(self, x, y):
        self.registers[x] += self.__get_value(y)

    def sub(self, x, y):
        self.registers[x] -= self.__get_value(y)

    def mul(self, x, y):
        self.registers[x] *= self.__get_value(y)

    def div(self, x, y):
        self.registers[x] //= self.__get_value(y)

    def jmp(self, x):
        self.rip = self.labels[x]-1

    def cmp(self, x, y):
        x = self.__get_value(x)
        y = self.__get_value(y)
        self.compare_results.append((x > y) - (x < y))

    def jne(self, x):
        if self.compare_results[-1] != 0:
            self.rip = self.labels[x]-1

    def je(self, x):
        if self.compare_results[-1] == 0:
            self.rip = self.labels[x]-1

    def jge(self, x):
        if self.compare_results[-1] >= 0:
            self.rip = self.labels[x]-1

    def jg(self, x):
        if self.compare_results[-1] == 1:
            self.rip = self.labels[x]-1

    def jle(self, x):
        if self.compare_results[-1] <= 0:
            self.rip = self.labels[x]-1

    def jl(self, x):
        if self.compare_results[-1] == -1:
            self.rip = self.labels[x]-1

    def call(self, x):
        self.call_instruction_addrs.insert(0, self.rip)
        self.rip = self.labels[x]-1

    def ret(self):
        self.rip = self.call_instruction_addrs.pop()

    def msg(self, *args):
        args = [str(self.registers[arg]) if arg in self.registers else arg for arg in args]
        self.result_messages.append("".join(args))

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

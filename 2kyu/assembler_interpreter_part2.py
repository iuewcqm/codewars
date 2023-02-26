# https://www.codewars.com/kata/58e61f3d8ff24f774400002c

def mov(args, rip):
    x, y = args
    registers[x] = registers[y] if y in registers else int(y)
    return rip + 1

def inc(args, rip):
    registers[args[0]] += 1
    return rip + 1

def dec(args, rip):
    registers[args[0]] -= 1
    return rip + 1

def add(args, rip):
    x, y = args
    registers[x] += registers[y] if y in registers else int(y)
    return rip + 1

def sub(args, rip):
    x, y = args
    registers[x] -= registers[y] if y in registers else int(y)
    return rip + 1

def mul(args, rip):
    x, y = args
    registers[x] *= registers[y] if y in registers else int(y)
    return rip + 1

def div(args, rip):
    x, y = args
    registers[x] //= registers[y] if y in registers else int(y)
    return rip + 1

def jmp(args, rip):
    return labels[args[0]]

def cmp(args, rip):
    x, y = args
    x = registers[x] if x in registers else int(x)
    y = registers[y] if y in registers else int(y)
    compare_results.append((x > y) - (x < y))
    return rip + 1

def jne(args, rip):
    if compare_results[-1] != 0:
        return labels[args[0]]
    return rip+1

def je(args, rip):
    if compare_results[-1] == 0:
        return labels[args[0]]
    return rip+1

def jge(args, rip):
    if compare_results[-1] >= 0:
        return labels[args[0]]
    return rip+1

def jg(args, rip):
    if compare_results[-1] == 1:
        return labels[args[0]]
    return rip+1

def jle(args, rip):
    if compare_results[-1] <= 0:
        return labels[args[0]]
    return rip+1

def jl(args, rip):
    if compare_results[-1] == -1:
        return labels[args[0]]
    return rip+1

def call(args, rip):
    call_instruction_addrs.insert(0, rip)
    return labels[args[0]]

def ret(args, rip):
    return call_instruction_addrs.pop()+1

def msg(args, rip):
    global result_message
    message = ""
    for i in range(len(args)):
        if args[i] in registers:
            message += str(registers[args[i]])
        elif args[i] == args[i+1] == "'":
            message += "', '"
        else:
            message += args[i]
    message = message.replace("'", "")
    # message = "".join([str(registers[a]) if a in registers else a.replace('\'', '') for a in args])
    print(message)
    result_message = message
    return rip+1

def end(args, rip):
    return -1

instructions = {'mov': mov, 'inc': inc, 'dec': dec, 'add':add, 'sub':sub, 'mul':mul, 'div':div, 'jmp':jmp, 'cmp':cmp, 'jne':jne, 'je':je, 'jge':jge, 'jg':jg, 'jle':jle, 'jl':jl, 'call':call, 'ret':ret, 'msg':msg, 'end':end }
labels = {}
registers = {}

call_instruction_addrs = []
compare_results = []
result_message = ""

def parse_arguments(args):
    args = args.split(';')[0]
    args = list(map(lambda x: x.strip(), args.split(',')))
    return args

def parse_program(program):
    instructs = []
    program = map(lambda i: i.strip(), program.split('\n'))
    program = [inst for inst in program if inst != '']
    for inst in program:
        func, _, args = inst.partition(' ')
        args = parse_arguments(args)
        if ':' in func:
            labels[func[:-1]] = len(instructs)
        elif func.startswith(';'):
            continue
        else:
            instructs.append((instructions[func], args))
    return instructs

def assembler_interpreter(program):
    global result_message
    registers.clear()
    labels.clear()
    program = parse_program(program)
    result_message = ""
    rip = 0
    print(labels)
    print("program:")
    for func, args in program:
        print(func.__name__, args)
    print()
    print("rip: ")
    while rip >= 0:
        try:
            func, args = program[rip]
            # print(rip, func.__name__, args)
            rip = func(args, rip)
        except:
            return -1
    return result_message

if __name__ == '__main__':
    program1 = """
    ; My first program
    mov a, 5
    inc a
    call function
    msg '(5+1)/2 = ', a    ; output message
    end

    function:
        div  a, 2
        ret
    """

    program2 = """
    mov a, 5
    mov b,10
    call function
    msg 'someshit' ; print someshit
    end

    function:
    mov c, 10
    call function1
    
    function1:
    mov d, 50
    ret
    """
    
    program3 = """
    mov a, 5
    mov b,10
    msg 'someshit' ; print someshit
    end
    """

    program4 = """
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
    """

    program5 = """
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
    """

    program6 = '''
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
    '''
    
    program7 = '''
    mov e, 13   ; instruction mov e, 13
    mov u, 4   ; instruction mov u, 4
    call func
    msg 'Random result: ', q
    end

    func:
        cmp e, u
        jle exit
        mov q, e
        add q, u
        ret
    ; Do nothing
    exit:
    msg 'Do nothing'
    '''
    print(assembler_interpreter(program7))

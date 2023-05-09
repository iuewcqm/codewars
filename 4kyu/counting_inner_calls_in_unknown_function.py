# https://www.codewars.com/kata/53efc28911c36ff01e00012c

from sys import settrace

def count_calls(func, *args, **kwargs):
    calls = -1
    
    def tracer(frame, event, arg=None):
        nonlocal calls
        if event == 'call': 
            calls += 1
        return tracer
    
    settrace(tracer)
    rv = func(*args, **kwargs)

    return calls, rv

def add(a, b):
  return a + b
  
def add_ten(a):
  return add(a, 10)
  
def misc_fun():
  return add(add_ten(3), add_ten(9))
  
if __name__ == '__main__':
    print(count_calls(add, 8, 12), (0, 20))
    print(count_calls(add_ten, 5), (1, 15))
    print(count_calls(misc_fun),   (5, 32))

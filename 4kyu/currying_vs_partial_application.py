# https://www.codewars.com/kata/53cf7e37e9876c35a60002c9

import inspect

def curry_partial(f, *initial_args):
    if not callable(f): return f
    argc = len(inspect.signature(f).parameters)
    if argc <= len(initial_args): return f(*initial_args[:argc])
    return lambda *a: curry_partial(f, *(initial_args+a))

def add(x, y, z):
    return x + y + z

if __name__ == '__main__':
    print(curry_partial(add)(1)(2)(3))
    print(curry_partial(add)(1, 2)(3))
    print(curry_partial(add)(1)(2, 3))
    print(curry_partial(add, 1)(2)(3))
    print(curry_partial(add, 1)(2, 3))
    print(curry_partial(add, 1, 2)(3))
    print(curry_partial(add, 1, 2, 3))
    print(curry_partial(add)()(1)()()(2)(3, 4, 5, 6))
    print(curry_partial(add)()(1)()()(2)(3, 4, 5, 6))
    print(curry_partial(add, 1)(2, 3, 4, 5))
    
    print(curry_partial(curry_partial(curry_partial(add, 1), 2), 3))
    print(curry_partial(curry_partial(add, 1, 2), 3))
    print(curry_partial(curry_partial(add, 1), 2)(3))
    print(curry_partial(curry_partial(add, 1)(2), 3))
    
    print(curry_partial(curry_partial(add, 1), 2, 3))
    print(curry_partial(curry_partial(curry_partial(add, 1)), 2, 3))

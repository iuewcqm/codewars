# https://www.codewars.com/kata/53cf7e37e9876c35a60002c9

def curry_partial(f, *args):
    if callable(f):
        argc = f.__code__.co_argcount
        if argc <= len(args): return f(*args[:argc or None])
        return lambda *a: curry_partial(f, *(args + a))
    return f

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

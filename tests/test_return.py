import sys

def my_function(x):
    return 2

def my_str_fn():
    return "immastring"

def my_function2(x):
    if x:
        return set()
    else:
        return "x"

def my_void_function(y):
    print (y)

def my_dead_code_function(z):
    print (z)
    if z:
        return
    print (z)

def is_eq(x, y):
    return x == y

x = my_function2()
is_eq(x, "test")
sys.argv.append(x)

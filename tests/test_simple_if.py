def collatz(n):
    if n == 1:
        return 0
    else:
        if n % 2 == 0:
            return 1 + collatz(n / 2)
        else:
            return 1 + collatz(n * 3 + 1)

class Duck:
    def __init__(self):
        pass

    def walk(self):
        return 4

def another_test(arg):
    print (arg.strip())
    print (arg.Duck())

def crap():
    string_var = "Literal String"
    another_test(string_var)

var = "more stuff"
another_test(var)

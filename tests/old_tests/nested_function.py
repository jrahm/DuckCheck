def outer_function(x):
    x.test1()

    z = "immastring"

    def inner_function(y):

        print (x.test2())
        print (y.test3())

        def inner_function_2(z):
            print (z)

        outer_function(x)
        outer_function(z)

    test2 = "str"

    inner_function(test2) # Error

st = "Hello"
outer_function(st)

inner_function(st) # Error

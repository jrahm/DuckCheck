class TestClass:

    def __init__(self):
        self.x = 5

        def inner_function(y):

            print (y.test_attribute)

            def inner_function_2(z):

                z.test_z_attribute()

            st = "This is a string"
            inner_function_2(st)

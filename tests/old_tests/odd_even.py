import os
def odd(x):
    if x == 1:
        return "yes"
    elif x == 0:
        return "no"
    else:
        return even(x - 1)

def even(x):
    if x == 0:
        return "yes"
    elif x == 1:
        return "no"
    else:
        return odd(x - 1)

x = 2
print(even(x))

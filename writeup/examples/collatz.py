def collatz(i):
    if i == 1:
        return 0
    elif i % 2 == 0:
        return 1 + collatz(i / 2)
    else:
        return 1 + collatz(i * 3 + 1)

print("the answer is: " + collatz(9))

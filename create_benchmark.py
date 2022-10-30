print("1337 ", end="")

for n in range (1, 100):
    for op in ["+", "-", "*", "/"]:
        print("{} {} ".format(op, n), end="")

for n in range (1, 100):
    for op in ["+", "-", "*", "/"]:
        print("{} (functie() {{ {} }}()) ".format(op, n), end="")
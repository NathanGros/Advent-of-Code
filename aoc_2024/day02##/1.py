f = open("input.txt", "r")
lines = [l[:-1] for l in f.readlines()]
linenumbers = [[int(i) for i in line.split()] for line in lines]


print(
    sum(
        (all(l[i] <= l[i + 1] for i in range(len(l) - 1))
        or all(l[i] >= l[i + 1] for i in range(len(l) - 1)))
        and all((0 < abs(l[i+1] - l[i]) <= 3) for i in range(len(l) - 1))
        for l in linenumbers
    )
)

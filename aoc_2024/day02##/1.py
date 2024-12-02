lines = [list(map(int, l.split())) for l in open("input.txt")]

print(sum(
    (all(l[i] <= l[i + 1] for i in range(len(l) - 1))
    or all(l[i] >= l[i + 1] for i in range(len(l) - 1)))
    and all((0 < abs(l[i+1] - l[i]) <= 3) for i in range(len(l) - 1))
    for l in lines
))

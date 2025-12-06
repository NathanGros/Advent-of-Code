lines = list(zip(*[l.split() for l in open("input.txt")]))

res = 0
for l in lines:
    lineRes = 0
    if l[-1] == '+':
        for n in l[:-1]:
            lineRes += int(n)
    elif l[-1] == '*':
        lineRes = 1
        for n in l[:-1]:
            lineRes *= int(n)
    res += lineRes
print(res)

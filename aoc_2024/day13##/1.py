lines = [l.split() for l in open("input.txt")]
groups = []
for i in range(int((len(lines)+1)/4)):
    groups.append([lines[4*i], lines[4*i+1], lines[4*i+2]])

data = [[int(a[2][2:-1]), int(a[3][2:]), int(b[2][2:-1]), int(b[3][2:]), int(p[1][2:-1]), int(p[2][2:])] for [a, b, p] in groups]

res = 0
for [ax, ay, bx, by, x, y] in data:
    i = (bx * y - by * x) / (ay * bx - ax * by)
    j = (x - i * ax) / bx
    isInt = False
    if (x - i * ax) % bx == 0 and (bx * y - by * x) % (ay * bx - ax * by) == 0:
        isInt = True
    if isInt:
        res += 3 * int(i) + int(j)
print(res)

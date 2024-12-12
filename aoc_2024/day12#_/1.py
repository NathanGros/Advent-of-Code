lines = [list(l.split()[0]) for l in open("input.txt")]
print(lines)

def isInBounds(x, y):
    return not ((x < 0) or (x > len(lines) - 1) or (y < 0) or (y > len(lines[0]) - 1))

def getRegion(x0, y0, c):
    toExplore = [(x0, y0)]
    inRegion = []
    while len(toExplore) > 0:
        (x, y) = toExplore[0]
        toExplore.pop(0)
        if (x, y) not in inRegion and isInBounds(x, y) and lines[x][y] == c:
            lines[x][y] = '.'
            inRegion.append((x, y))
            toExplore.append((x-1, y))
            toExplore.append((x+1, y))
            toExplore.append((x, y-1))
            toExplore.append((x, y+1))
    return inRegion

regions = []
for i in range(len(lines)):
    for j in range(len(lines[0])):
        if lines[i][j] != '.':
            regions.append(getRegion(i, j, lines[i][j]))

def perimeter(r):
    p = 0
    for (x, y) in r:
        if (x-1, y) not in r:
            p += 1
        if (x+1, y) not in r:
            p += 1
        if (x, y-1) not in r:
            p += 1
        if (x, y+1) not in r:
            p += 1
    return p

res = 0
for r in regions:
    res += perimeter(r) * len(r)
print(res)

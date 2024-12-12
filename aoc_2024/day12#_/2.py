lines = [list(l.split()[0]) for l in open("input.txt")]

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

def sides(r):
    sideList = []
    for (x, y) in r:
        if (x-1, y) not in r:
            sideList.append((x, y, x, y+1))
        if (x+1, y) not in r:
            sideList.append((x+1, y, x+1, y+1))
        if (x, y-1) not in r:
            sideList.append((x, y, x+1, y))
        if (x, y+1) not in r:
            sideList.append((x, y+1, x+1, y+1))
    foundSides = []
    for i in range(len(sideList)):
        s1 = sideList[i]
        matched = False
        for j in range(len(foundSides)):
            s2 = foundSides[j]
            if s1[0] == s1[2] == s2[0] == s2[2]:
                if s1[3] == s2[1]:
                    matched = True
                    foundSides[j] = (s2[0], s1[1], s2[2], s2[3])
                elif s2[3] == s1[1]:
                    matched = True
                    foundSides[j] = (s2[0], s2[1], s2[2], s1[3])
            if s1[1] == s1[3] == s2[1] == s2[3]:
                if s1[2] == s2[0]:
                    matched = True
                    foundSides[j] = (s1[0], s2[1], s2[2], s2[3])
                if s2[2] == s1[0]:
                    matched = True
                    foundSides[j] = (s2[0], s2[1], s1[2], s2[3])
        if not matched:
            foundSides.append(s1)
    print(foundSides)
    return len(foundSides)

res = 0
for r in regions:
    res += sides(r) * len(r)
print(res)

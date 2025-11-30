lines = [list(l.split()[0]) for l in open("input.txt")]

def printMat(l):
    for line in l:
        print("".join(line))

def printRegion(r, c):
    minx = r[0][0]
    miny = r[0][1]
    maxx = r[0][0]
    maxy = r[0][1]
    for x, y in r:
        if x < minx:
            minx = x
        elif x > maxx:
            maxx = x
        if y < miny:
            miny = y
        elif y > maxy:
            maxy = y
    regionMat = []
    for i in range(maxx - minx + 1):
        regionMat.append([])
        for j in range(maxy - miny + 1):
            if (minx + i, miny + j) in r:
                regionMat[i].append("██")
            else:
                regionMat[i].append("  ")
    printMat(regionMat)

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

def sides(r):
    sideList = []
    #directions: 0 = N, 1 = E, 2 = S, 3 = W
    for (x, y) in r:
        if (x-1, y) not in r:
            sideList.append((x, y, x, y+1, 0))
        if (x+1, y) not in r:
            sideList.append((x+1, y, x+1, y+1, 2))
        if (x, y-1) not in r:
            sideList.append((x, y, x+1, y, 3))
        if (x, y+1) not in r:
            sideList.append((x, y+1, x+1, y+1, 1))
    sideList = sorted(sideList)
    foundSides = []
    for i in range(len(sideList)):
        s1 = sideList[i]
        matched = False
        for j in range(len(foundSides)):
            s2 = foundSides[j]
            if s1[0] == s1[2] == s2[0] == s2[2] and s1[4] == s2[4]:
                if s1[3] == s2[1]:
                    matched = True
                    foundSides[j] = (s2[0], s1[1], s2[2], s2[3], s1[4])
                elif s2[3] == s1[1]:
                    matched = True
                    foundSides[j] = (s2[0], s2[1], s2[2], s1[3], s1[4])
            if s1[1] == s1[3] == s2[1] == s2[3] and s1[4] == s2[4]:
                if s1[2] == s2[0]:
                    matched = True
                    foundSides[j] = (s1[0], s2[1], s2[2], s2[3], s1[4])
                if s2[2] == s1[0]:
                    matched = True
                    foundSides[j] = (s2[0], s2[1], s1[2], s2[3], s1[4])
        if not matched:
            foundSides.append(s1)
    return len(foundSides)

res = 0
for i in range(len(lines)):
    for j in range(len(lines[0])):
        if lines[i][j] != '.':
            c = lines[i][j]
            region = getRegion(i, j, c)
            debug = False
            if sides(region) % 2 != 0:
                printRegion(region, c)
                print("sides:", sides(region))
                print("area:", len(region))
                print()
            res += sides(region) * len(region)
print(res)

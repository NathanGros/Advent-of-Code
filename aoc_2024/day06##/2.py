lines = [list(l[:-1]) for l in open("input.txt")]
for i in range(len(lines)):
    for j in range(len(lines[0])):
        if lines[i][j] == '.':
            lines[i][j] = 0
        elif lines[i][j] == '^':
            lines[i][j] = 10
        elif lines[i][j] == '#':
            lines[i][j] = -1

def printl(lines):
    for i in range(len(lines)):
        for j in range(len(lines[0])):
            if lines[i][j] == -1:
                print('#', end="")
            elif lines[i][j] >= 10:
                print('^', end="")
            else:
                print(lines[i][j], end="")
        print("")
    print(" ")



def copyMatrix(lines):
    l = []
    for i in range(len(lines)):
        l.append([])
        for j in lines[i]:
            l[i].append(j)
    return l

def rotate(lines):
    rotated = []
    for i in range(len(lines[0])):
        rotated.append([])
        for j in range(len(lines)):
            c = lines[j][len(lines)-1-i]
            rotated[i].append(c)
    return rotated

def walkUp(lines, i, j):
    index = i
    while lines[index][j] != -1:
        if index == 0:
            lines[0][j] = 1
            return 1
        if lines[index][j] >= 5 and lines[index][j] < 10:
            return 2
        else:
            if lines[index][j] >= 10:
                lines[index][j] -= 9
            else:
                lines[index][j] += 1
            index -= 1
    index += 1
    lines[index][j] += 10
    return 0

def stepGuard(lines):
    found = False
    for i in range(len(lines)):
        for j in range(len(lines[i])):
            if lines[i][j] >= 10:
                return walkUp(lines, i, j)
    return 0

def findLoop(i, j, lines):
    testLines = copyMatrix(lines)
    testLines[i][j] = -1
    isEnd = stepGuard(testLines)
    while isEnd == 0:
        testLines = rotate(testLines)
        isEnd = stepGuard(testLines)
        if isEnd == 2:
            return 1
    return 0



posI, posJ = -1, -1
for i in range(len(lines)):
    for j in range(len(lines[i])):
            if lines[i][j] >= 10:
                posI, posJ = i, j

originalPath = copyMatrix(lines)
isEnd = stepGuard(originalPath)
rotateCount = 0
while isEnd == 0:
    originalPath = rotate(originalPath)
    rotateCount += 1
    isEnd = stepGuard(originalPath)
for i in range(4 - rotateCount%4):
    originalPath = rotate(originalPath)

res = 0
# res += findLoop(13, 117, lines)
for i in range(len(lines)):
    for j in range(len(lines[i])):
        o = originalPath[i][j]
        if not (o == -1 or o == 0 or lines[i][j] >= 10):
            res += findLoop(i, j, lines)
            print(i, j, res)
print(res)

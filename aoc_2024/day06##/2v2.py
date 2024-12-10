lines = [list(l[:-1]) for l in open("input.txt")]

def printm(lines):
    for l in lines:
        print("".join(l))
    print()

startX = 0
startY = 0
orient = [-1, 0]
for i in range(len(lines)):
    for j in range(len(lines[0])):
        if lines[i][j] == '^':
            startX = i
            startY = j

printm(lines)

def isInBounds(x, y):
    if x < 0 or x >= len(lines) or y < 0 or y >= len(lines[0]):
        return False
    else:
        return True

def step(posX, posY, orient):
    newPosX = posX + orient[0]
    newPosY = posY + orient[1]
    if not isInBounds(newPosX, newPosY):
        return [-1, -1], orient
    else:
        while lines[newPosX][newPosY] == '#':
            orient = [orient[1], -1 * orient[0]]
            newPosX = posX + orient[0]
            newPosY = posY + orient[1]
        return [newPosX, newPosY], orient

def isLoop(posX, posY, orient):
    l = []
    while True:
        # print(posX, posY)
        [posX, posY], orient = step(posX, posY, orient)
        if not isInBounds(posX, posY):
            return False
        elif [posX, posY, orient] in l:
            return True
        else:
            l.append([posX, posY, orient])

res = 0
for i in range(len(lines)):
    for j in range(len(lines[0])):
        if lines[i][j] == '.':
            print(i, j)
            lines[i][j] = '#'
            if isLoop(startX, startY, [-1, 0]):
                res += 1
            lines[i][j] = '.'
print(res)

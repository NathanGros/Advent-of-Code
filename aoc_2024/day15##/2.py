lines = [l.split() for l in open("input.txt")]
splitIndex = 0
for i in range(len(lines)):
    if lines[i] == []:
        splitIndex = i
mat = [list(l[0]) for l in lines[:splitIndex]]
wideMat = []
for i in range(len(mat)):
    wideMat.append([])
    for j in range(len(mat[0])):
        match mat[i][j]:
            case '.':
                wideMat[i].append('.')
                wideMat[i].append('.')
            case '#':
                wideMat[i].append('#')
                wideMat[i].append('#')
            case 'O':
                wideMat[i].append('[')
                wideMat[i].append(']')
            case '@':
                wideMat[i].append('@')
                wideMat[i].append('.')
mat = wideMat
mov = ''.join([l[0] for l in lines[splitIndex+1:]])

def printm(mat):
    for l in mat:
        print(''.join(l))

def canPush(mat, direction, x, y):
    match mat[y + direction[1]][x]:
        case '[':
            return canPush(mat, direction, x, y + direction[1]) and canPush(mat, direction, x + 1, y + direction[1])
        case ']':
            return canPush(mat, direction, x, y + direction[1]) and canPush(mat, direction, x - 1, y + direction[1])
        case '.':
            return True
        case '#':
            return False

def getBoxPos(mat, direction, x, y):
    match mat[y + direction[1]][x]:
        case '[':
            return [(x, y + direction[1])] + getBoxPos(mat, direction, x, y + direction[1]) + getBoxPos(mat, direction, x + 1, y + direction[1])
        case ']':
            return [(x-1, y + direction[1])] + getBoxPos(mat, direction, x, y + direction[1]) + getBoxPos(mat, direction, x - 1, y + direction[1])
        case '.':
            return []

def step(mat, direction, x, y):
    newX = x + direction[0]
    newY = y + direction[1]
    if mat[newY][newX] == '#':
        return (mat, x, y)
    if mat[newY][newX] == '.':
        return (mat, newX, newY)
    #the robot encounters a box
    #if horizontal boxes
    if direction[1] == 0:
        i = 0
        while mat[newY + i * direction[1]][newX + i * direction[0]] == '[' or mat[newY + i * direction[1]][newX + i * direction[0]] == ']':
            i += 1
        #if cannot push the boxes
        if mat[newY + i * direction[1]][newX + i * direction[0]] == '#':
            return (mat, x, y)
        #push the boxes
        i = 0
        while mat[newY + i * direction[1]][newX + i * direction[0]] != '.':
            if mat[newY + i * direction[1]][newX + i * direction[0]] == '[':
                mat[newY + i * direction[1]][newX + i * direction[0]] = ']'
            elif mat[newY + i * direction[1]][newX + i * direction[0]] == ']':
                mat[newY + i * direction[1]][newX + i * direction[0]] = '['
            i += 1
        if direction[0] == 1:
            mat[newY + i * direction[1]][newX + i * direction[0]] = ']'
        else:
            mat[newY + i * direction[1]][newX + i * direction[0]] = '['
        mat[newY][newX] = '.'
        return (mat, newX, newY)
    #if vertical boxes
    if not canPush(mat, direction, x, y):
        return (mat, x, y)
    #push the boxes
    boxPos = getBoxPos(mat, direction, x, y)
    for (x, y) in boxPos:
        mat[y][x] = '.'
        mat[y][x+1] = '.'
    for (x, y) in boxPos:
        mat[y+direction[1]][x] = '['
        mat[y+direction[1]][x+1] = ']'
    return (mat, newX, newY)

y, x = 0, 0
for i in range(len(mat)):
    for j in range(len(mat[0])):
        if mat[i][j] == '@':
            mat[i][j] = '.'
            y, x = i, j

direction = [0, 0]
for c in mov:
    match c:
        case '<':
            direction = [-1, 0]
        case '>':
            direction = [1, 0]
        case '^':
            direction = [0, -1]
        case 'v':
            direction = [0, 1]
    mat, x, y = step(mat, direction, x, y)

res = 0
for i in range(len(mat)):
    for j in range(len(mat[0])):
        if mat[i][j] == '[':
            res += 100 * i + j
print(res)

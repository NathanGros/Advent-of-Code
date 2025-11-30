lines = [l.split() for l in open("input.txt")]
splitIndex = 0
for i in range(len(lines)):
    if lines[i] == []:
        splitIndex = i
mat = [list(l[0]) for l in lines[:splitIndex]]
mov = ''.join([l[0] for l in lines[splitIndex+1:]])

def printm(mat):
    for l in mat:
        print(''.join(l))

def step(mat, direction, x, y):
    newX = x + direction[0]
    newY = y + direction[1]
    if mat[newY][newX] == '#':
        return (mat, x, y)
    if mat[newY][newX] == '.':
        return (mat, newX, newY)
    #the robot encounters a box
    i = 0
    while mat[newY + i * direction[1]][newX + i * direction[0]] == 'O':
        i += 1
    #if cannot push the boxes
    if mat[newY + i * direction[1]][newX + i * direction[0]] == '#':
        return (mat, x, y)
    #push the boxes
    mat[newY + i * direction[1]][newX + i * direction[0]] = 'O'
    mat[newY][newX] = '.'
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
        if mat[i][j] == 'O':
            res += 100 * i + j
print(res)

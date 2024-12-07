lines = [list(l[:-1]) for l in open("input.txt")]

def rotate(lines):
    rotated = []
    for i in range(len(lines[0])):
        rotated.append([])
        for j in range(len(lines)):
            rotated[i].append(lines[j][len(lines)-1-i])
    return rotated

def walkUp(lines, i, j):
    index = i
    while lines[index][j] != '#':
        if index == 0:
            lines[0][j] = 'X'
            return True
        lines[index][j] = 'X'
        index -= 1
    index += 1
    lines[index][j] = '^'
    return False

def stepGuard(lines):
    found = False
    for i in range(len(lines)):
        for j in range(len(lines[i])):
            if not found:
                if lines[i][j] == '^':
                    exit = walkUp(lines, i, j)
                    found = True
                    if exit:
                        return True
    return False
    

isEnd = stepGuard(lines)
while not isEnd:
    lines = rotate(lines)
    isEnd = stepGuard(lines)

res = 0
for l in lines:
    for c in l:
        if c == 'X' or c == '^':
            res += 1
print(res)

lines = [list(l.split()[0]) for l in open("input.txt")]

def nbNeighbors(lines, x, y):
    neighbors = 0
    if x > 0:
        neighbors += 1 if lines[x-1][y] == '@' else 0
    if y > 0:
        neighbors += 1 if lines[x][y-1] == '@' else 0
    if x < len(lines)-1:
        neighbors += 1 if lines[x+1][y] == '@' else 0
    if y < len(lines[0])-1:
        neighbors += 1 if lines[x][y+1] == '@' else 0
    if x > 0 and y > 0:
        neighbors += 1 if lines[x-1][y-1] == '@' else 0
    if x > 0 and y < len(lines[0])-1:
        neighbors += 1 if lines[x-1][y+1] == '@' else 0
    if x < len(lines)-1 and y > 0:
        neighbors += 1 if lines[x+1][y-1] == '@' else 0
    if x < len(lines)-1 and y < len(lines[0])-1:
        neighbors += 1 if lines[x+1][y+1] == '@' else 0
    return neighbors


res = 0
finished = False
while not finished:
    finished = True
    for i in range(len(lines)):
        for j in range(len(lines[0])):
            if lines[i][j] != '@':
                continue
            if nbNeighbors(lines, i, j) < 4:
                res += 1
                lines[i][j] = '.'
                finished = False

print(res)

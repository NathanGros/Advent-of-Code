lines = [list(l.split()[0]) for l in open("input.txt")]
# print(lines)

def markNodes(lines):
    for i in range(1, len(lines) - 1):
        for j in range(1, len(lines[0]) - 1):
            if lines[i][j] != '.':
                continue
            if lines[i][j+1] != '#' and lines[i][j-1] != '#' and lines[i+1][j] == '#' and lines[i-1][j] == '#':
                lines[i][j] = ' '
            if lines[i][j+1] == '#' and lines[i][j-1] == '#' and lines[i+1][j] != '#' and lines[i-1][j] != '#':
                lines[i][j] = ' '

def makeGraph(lines):
    markNodes(lines)
    links = {}
    for i in range(1, len(lines) - 1):
        for j in range(1, len(lines[0]) - 1):
            # find node
            if lines[i][j] != '#' and lines[i][j] != ' ':
                neighbors = []
                # 4 directions
                facing = [1, 0]
                for k in range(4):
                    posX = i + facing[0]
                    posY = j + facing[1]
                    # stepping until neighbor
                    while lines[posX][posY] == ' ':
                        posX += facing[0]
                        posY += facing[1]
                    # check if this is a node or a wall
                    if lines[posX][posY] != '#':
                        neighbors.append((posX, posY))
                    # rotation
                    facingTmp = facing[1]
                    facing[1] = facing[0]
                    facing[0] = -1 * facingTmp
                links[(i, j)] = neighbors
    return links

def computePath(pos1, pos2, facing):
    x1, y1 = pos1
    x2, y2 = pos2
    dx = x2 - x1
    dy = y2 - y1
    if dx == 0:
        if facing == 'H':
            return ('H', abs(dy))
        return ('H', 1000 + abs(dy))
    else:
        if facing == 'V':
            return ('V', abs(dx))
        return ('V', 1000 + abs(dx))

def djikstra(links, posS, posE):
    visited = {}
    # toVisit entry: [(posX, posY), horizontalPaths:[[(posX, posY), (posX, posY)]], horizontalPoints, verticalPaths:[], verticalPoints]
    # TODO
    toVisit = [[posS, 0, [], 0]]
    while toVisit != []:
        # find minimum points node
        minPoints = toVisit[0][1]
        minIndex = 0
        for i in range(len(toVisit)):
            if toVisit[i][1] < minPoints:
                minPoints = toVisit[i][1]
                minIndex = i
        minNode = toVisit[minIndex]
        # mark node as visited if inferior points than previous path
        toVisit.pop(minIndex)
        if minNode[0] not in visited:
            visited[minNode[0]] = minNode[1]
        else:
            if minNode[1] < visited[minNode[0]]:
                visited[minNode[0]] = minNode[1]
        # add new neighbors to toVisit
        newNeighbors = links[minNode[0]]
        for i in range(len(newNeighbors)):
            # neighbor already visited
            if newNeighbors[i] in visited:
                continue
            # search neighbor in toVisit
            newFacing, newPoints = computePath(minNode[0], newNeighbors[i], minNode[2])
            found = 0
            for visitingNode in toVisit:
                # neighbor already in toVisit
                if visitingNode[0] == newNeighbors[i]:
                    found = 1
                    # check if useless
                    if visitingNode[2] == newFacing and visitingNode[1] <= minNode[1] + newPoints:
                        continue
                    toVisit.append([newNeighbors[i], minNode[1] + newPoints, newFacing])
                    break
            # brand new neighbor
            if found == 0:
                toVisit.append([newNeighbors[i], minNode[1] + newPoints, newFacing])
    return visited[posE]

sx = -1
sy = -1
for i in range(len(lines)):
    for j in range(len(lines[0])):
        if lines[i][j] == 'S':
            sx = i
            sy = j
            break
ex = -1
ey = -1
for i in range(len(lines)):
    for j in range(len(lines[0])):
        if lines[i][j] == 'E':
            ex = i
            ey = j
            break

print(djikstra(makeGraph(lines), (sx, sy), (ex, ey)))

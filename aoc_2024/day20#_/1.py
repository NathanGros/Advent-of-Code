lines = [list(l.split()[0]) for l in open("input.txt")]

sx = 0
sy = 0
for i in range(len(lines)):
    for j in range(len(lines[0])):
        if lines[i][j] == 'S':
            sx = j
            sy = i
            break
ex = 0
ey = 0
for i in range(len(lines)):
    for j in range(len(lines[0])):
        if lines[i][j] == 'E':
            ex = j
            ey = i
            break

def step(lines, pos, time):
    lines[pos[1]][pos[0]] = time
    if str(lines[pos[1]-1][pos[0]]) in ".SE":
        return [pos[0], pos[1]-1]
    elif str(lines[pos[1]+1][pos[0]]) in ".SE":
        return [pos[0], pos[1]+1]
    elif str(lines[pos[1]][pos[0]-1]) in ".SE":
        return [pos[0]-1, pos[1]]
    elif str(lines[pos[1]][pos[0]+1]) in ".SE":
        return [pos[0]+1, pos[1]]
    else:
        print("error")

pos = [sx, sy]

time = 0
track = [[sx, sy, 0]]
while pos != [ex, ey]:
    pos = step(lines, pos, time)
    time += 1
    track.append([pos[0], pos[1], time])
print(track)
print(len(track))

cheats = []
for [x1, y1, t1] in track:
    for [x2, y2, t2] in track:
        if not (x1 == x2 and y1 == y2) and t2 > t1:
            if (y1 == y2 and abs(x2 - x1) == 2) or (x1 == x2 and abs(y2 - y1) == 2):
                cheats.append([x1, y1, x2, y2, t2 - t1 - 2])
print("cheats:")
print(cheats)

res = 0
for c in cheats:
    if c[4] >= 100:
        res += 1
print(res)

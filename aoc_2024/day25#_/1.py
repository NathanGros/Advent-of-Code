lines = [l.split() for l in open("input.txt")]

inLocks = True
locks = []
keys = []
buffer = []
for l in lines:
    if l == []:
        if buffer[0][0] == '.':
            keys.append(buffer)
        else:
            locks.append(buffer)
        buffer = []
    else:
        buffer.append(l[0])
if buffer[0][0] == '.':
    keys.append(buffer)
else:
    locks.append(buffer)
buffer = []

lockHeights = []
for lock in locks:
    l = []
    for i in range(len(lock[0])):
        for j in range(len(lock)):
            if lock[j][i] == '.':
                l.append(j-1)
                break
    lockHeights.append(l)

keyHeights = []
for key in keys:
    l = []
    for i in range(len(key[0])):
        for j in range(len(key)-1, -1, -1):
            if key[j][i] == '.':
                l.append(len(key)-j-2)
                break
    keyHeights.append(l)

print(lockHeights)
print(keyHeights)

heightSum = len(keys[0]) - 2

res = 0
for l in lockHeights:
    for k in keyHeights:
        fits = True
        for i in range(len(l)):
            if l[i] + k[i] > heightSum:
                fits = False
        if fits:
            res += 1
print(res)

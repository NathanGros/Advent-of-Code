lines = [l.split() for l in open("input.txt")]
temp = [[p.split(','), v.split(',')] for [p, v] in lines]
data = [[int(a[0][2:]), int(a[1]), int(b[0][2:]), int(b[1])] for [a, b] in temp]
print(data)

def step(data, w, h):
    for i in range(len(data)):
        [a, b, c, d] = data[i]
        a2 = (a + c) % w
        b2 = (b + d) % h
        data[i] = [a2, b2, c, d]
    return data

gridWidth = 101
gridHeight = 103

for i in range(100):
    data = step(data, gridWidth, gridHeight)
    print(data)

halfWidth = int(gridWidth / 2)
halfHeight = int(gridHeight / 2)
tl = 0
tr = 0
bl = 0
br = 0
for [a, b, c, d] in data:
    if a < halfWidth and b < halfHeight:
        tl += 1
    elif a > halfWidth and b < halfHeight:
        tr += 1
    elif a < halfWidth and b > halfHeight:
        bl += 1
    elif a > halfWidth and b > halfHeight:
        br += 1
print(tl * tr * bl * br)

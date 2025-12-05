lines = [l.split() for l in open("input.txt")]

intervals = [list(map(int, a[0].split('-'))) for a in lines[:lines.index([])]]
ids = [int(a[0]) for a in lines[lines.index([])+1:]]

res = 0
for id in ids:
    for [a, b] in intervals:
        if a <= id and id <= b:
            res += 1
            break

print(res)

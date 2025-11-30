lines = [l.split()[0].split('-') for l in open("input.txt")]

connections = {}
for [pc1, pc2] in lines:
    if pc1 not in connections:
        connections[pc1] = []
    if pc2 not in connections:
        connections[pc2] = []
    if pc2 not in connections[pc1]:
        connections[pc1].append(pc2)
    if pc1 not in connections[pc2]:
        connections[pc2].append(pc1)

groups = []
for pc1 in connections:
    for pc2 in connections[pc1]:
        l = [pc1, pc2]
        for i in connections[pc1]:
            if i in connections[pc2]:
                l.append(i)
            if len(l) >= 3:
                groups.append(l)
                l = [pc1, pc2]
groups = list(dict.fromkeys([tuple(sorted(g)) for g in groups]))

res = 0
for g in groups:
    for pc in g:
        if pc[0] == 't':
            print(g)
            res += 1
            break
print(res)

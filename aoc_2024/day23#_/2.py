lines = [l.split()[0].split('-') for l in open("small_input.txt")]

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
print(connections)

groups = []
for pc1 in connections:
    for pc2 in connections[pc1]:
        l = [pc1, pc2]
        for i in connections[pc1]:
            if i in connections[pc2]:
                l.append(i)
        if len(l) >= 3:
            groups.append(l)
groups = list(dict.fromkeys([tuple(sorted(g)) for g in groups]))
print(groups)

maxg = []
maxlen = 0
for g in groups:
    if len(g) == maxlen:
        maxg.append(g)
    if len(g) > maxlen:
        maxlen = len(g)
        maxg = [g]
print(maxg)

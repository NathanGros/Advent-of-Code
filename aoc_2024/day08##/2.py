lines = [list(l[:-1]) for l in open("input.txt")]

def isInBounds(n):
    return not ((n[0] < 0) or (n[0] > len(lines) - 1) or (n[1] < 0) or (n[1] > len(lines[0]) - 1))

#get dictionnary of antenna positions, grouped by antenna type
antennas = {}
for i in range(len(lines)):
    for j in range(len(lines[0])):
        c = lines[i][j]
        if c != '.':
            if c in antennas:
                antennas[c].append([i, j])
            else:
                antennas[c] = [[i, j]]

#make a list of antinodes positions
antinodes = []
for a in antennas:
    l = antennas[a]
    for i in range(len(l)):
        for j in range(len(l)):
            if i != j:
                [p1, p2] = l[i]
                [p3, p4] = l[j]
                #add mirror of l[j] from l[i]'s perspective and all of their multiples
                node = [p1, p2]
                while isInBounds(node):
                    antinodes.append(node)
                    node = [node[0]+p1-p3, node[1]+p2-p4]

#only keep in bounds antinodes
validAntinodes = []
for n in antinodes:
    if isInBounds(n) and n not in validAntinodes:
        validAntinodes.append(n)
print(len(validAntinodes))

lines = [l[:-1] for l in open("input.txt")]

endCrates = 0
for l in lines:
    if l == '':
        endCrates -= 2
        break
    endCrates += 1

nbPiles = int((len(lines[0])+1)/4)
crates = [[] for i in range(nbPiles)]
for i in range(endCrates, -1, -1):
    for j in range(nbPiles):
        label = lines[i][1+4*j]
        if label != ' ':
            crates[j].append(label)

for i in range(endCrates+3, len(lines)):
    instruct = lines[i].split()
    for j in range(int(instruct[1])):
        dst = int(instruct[5])-1
        src = int(instruct[3])-1
        crates[dst].append(crates[src].pop(len(crates[src]) - int(instruct[1]) + j))

res = ""
for pile in crates:
    res += pile.pop()
print(res)

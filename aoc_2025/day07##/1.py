lines = [list(l.split()[0]) for l in open("input.txt")]

occupied = [lines[0].index('S')]

split = 0
for l in lines[1:]:
    newOccupied = []
    for i in range(len(l)):
        if i not in occupied:
            continue
        if l[i] == '.':
            newOccupied.append(i)
        elif l[i] == '^':
            split += 1
            if i-1 >= 0 and i-1 not in newOccupied:
                newOccupied.append(i-1)
            if i+1 < len(l) and i+1 not in newOccupied:
                newOccupied.append(i+1)
    occupied = newOccupied

print(split)

lines = [l[:-1] for l in open("input.txt")]

endRules = 0
for l in lines:
    if l == '':
        endRules += 1
        break
    endRules += 1

updates = []
for i in range(endRules, len(lines)):
    updates.append(lines[i].split(','))

res = 0
for i in range(len(lines) - endRules):
    update = updates[i]
    printable = True
    for j in range(0, endRules-1):
        rule = lines[j].split('|')
        for k in range(len(update)-1):
            for l in range(k+1, len(update)):
                if rule[1] == update[k] and rule[0] == update[l]:
                    printable = False
    if printable:
        res += int(update[int((len(update)+1)/2)-1])
print(res)

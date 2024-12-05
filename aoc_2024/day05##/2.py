lines = [l[:-1] for l in open("input.txt")]

endRules = 0
for l in lines:
    if l == '':
        endRules += 1
        break
    endRules += 1

def isPrintable(update):
    for j in range(0, endRules-1):
        rule = lines[j].split('|')
        for k in range(len(update)-1):
            for l in range(k+1, len(update)):
                if rule[1] == update[k] and rule[0] == update[l]:
                    return False
    return True

def insertSort(rules, l):
    sortedUpdate = []
    for e in l:
        insertIndex = 0
        canInsert = False
        while (not canInsert) and insertIndex < len(l):
            flag = True
            for r in rules:
                if r[0] == e and r[1] in sortedUpdate:
                    if sortedUpdate.index(r[1]) < insertIndex:
                        flag = False
                elif r[1] == e and r[0] in sortedUpdate:
                    if insertIndex <= sortedUpdate.index(r[0]):
                        flag = False
            canInsert = flag
            if not canInsert:
                insertIndex += 1
        sortedUpdate.insert(insertIndex, e)
    return sortedUpdate



updates = []
for i in range(endRules, len(lines)):
    updates.append(lines[i].split(','))

unprintables = []
for i in range(len(lines) - endRules):
    update = updates[i]
    printable = isPrintable(update)
    if not printable:
        if update not in unprintables:
            unprintables.append(update)

rules = []
for i in range(0, endRules-1):
    rules.append(lines[i].split('|'))

res = 0
for update in unprintables:
    sortedUpdate = insertSort(rules, update)
    res += int(sortedUpdate[int((len(sortedUpdate)+1)/2)-1])
print(res)

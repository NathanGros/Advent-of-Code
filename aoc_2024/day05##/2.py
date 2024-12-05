lines = [l[:-1] for l in open("input.txt")]

endRules = 0
for i in range(len(lines)):
    if lines[i] == '':
        endRules = i+1

def isPrintable(update):
    #check if all rules correctly applied
    for j in range(0, endRules-1):
        rule = lines[j].split('|')
        for k in range(len(update)-1):
            for l in range(k+1, len(update)):
                if rule[1] == update[k] and rule[0] == update[l]:
                    return False
    return True

def insertSort(rules, update):
    sortedUpdate = []
    #for every element , insert it at a position where no rules are broken
    for e in update:
        insertIndex = 0
        #iterate through sortedUpdate to find insertIndex
        while insertIndex < len(sortedUpdate):
            canInsert = True
            #check if no rules are broken
            for r in rules:
                if ((r[0] == e and r[1] in sortedUpdate and sortedUpdate.index(r[1]) < insertIndex)
                or (r[1] == e and r[0] in sortedUpdate and insertIndex <= sortedUpdate.index(r[0]))):
                    canInsert = False
            if canInsert:
                break
            else:
                insertIndex += 1
        sortedUpdate.insert(insertIndex, e)
    return sortedUpdate



updates = [l.split(',') for l in lines[endRules:]]
unprintables = [u for u in updates if not isPrintable(u)]
rules = [l.split('|') for l in lines[:endRules-1]]

res = 0
for update in unprintables:
    sortedUpdate = insertSort(rules, update)
    #get middle number
    res += int(sortedUpdate[int((len(sortedUpdate)+1)/2)-1])
print(res)

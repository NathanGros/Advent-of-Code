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

updates = [l.split(',') for l in lines[endRules:]]

res = 0
for i in range(len(lines) - endRules):
    update = updates[i]
    if isPrintable(update):
        #get middle number
        res += int(update[int((len(update)+1)/2)-1])
print(res)

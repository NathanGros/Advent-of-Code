lines = [l.split() for l in open("input.txt")]
splitIndex = 0
for i in range(len(lines)):
    if lines[i] == []:
        splitIndex = i
states = {}
for [a, b] in lines[:splitIndex]:
    states[a[:-1]] = int(b)
unprocessedWires = [[l[4], l[1], l[0], l[2]] for l in lines[splitIndex+1:]]

while len(unprocessedWires) > 0:
    for i in range(len(unprocessedWires)-1, -1, -1):
        [out, gate, w1, w2] = unprocessedWires[i]
        #process gate if both its inputs are known
        if w1 in states and w2 in states:
            match gate:
                case 'AND':
                    result = states[w1] and states[w2]
                case 'OR':
                    result = states[w1] or states[w2]
                case 'XOR':
                    result = (states[w1] + states[w2] == 1)
            states[out] = int(result)
            unprocessedWires.pop(i)
print(states)

res = 0
bit = 0
twoPower = 1
while ('z' + '0' * (2 - len(str(bit))) + str(bit)) in states:
    res += twoPower * states['z' + '0' * (2 - len(str(bit))) + str(bit)]
    bit += 1
    twoPower *= 2
print(res)

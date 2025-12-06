lines = list(zip(*[list(l)[:-1] for l in open("input.txt")]))

def makeNumber(l):
    l2 = [0 if d == ' ' else int(d) for d in l]
    for i in range(len(l2)-1, -1, -1):
        if l2[i] == 0:
            del l2[i]
    n = 0
    for i in range(len(l2)):
        n += 10**i * int(l2[-1 - i])
    return n

def getOperationResult(operation, numbers):
    operationRes = 0
    if operation == '+':
        operationRes = sum(numbers)
    elif operation == '*':
        operationRes = 1
        for n in numbers:
            operationRes *= n
    return operationRes

res = 0
operation = ' '
numbers = []
for i in range(len(lines)):
    l = list(lines[i])
    # new operation
    if all(d == ' ' for d in l):
        res += getOperationResult(operation, numbers)
        numbers = []
        operation = ' '
        continue
    # find operation
    if l[-1] != ' ':
        operation = l[-1]
    numbers.append(makeNumber(l[:-1]))
    # last line condition
    if i == len(lines) - 1:
        res += getOperationResult(operation, numbers)

print(res)

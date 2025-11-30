lines = [l.split()[0] for l in open("input.txt")]

def digitalCode(s, pos):
    code = list(s)
    [y1, x1] = pos
    inputList = [""]
    for digit in code:
        match digit:
            case 'A':
                y2, x2 = 3, 2
            case '0':
                y2, x2 = 3, 1
            case '1':
                y2, x2 = 2, 0
            case '2':
                y2, x2 = 2, 1
            case '3':
                y2, x2 = 2, 2
            case '4':
                y2, x2 = 1, 0
            case '5':
                y2, x2 = 1, 1
            case '6':
                y2, x2 = 1, 2
            case '7':
                y2, x2 = 0, 0
            case '8':
                y2, x2 = 0, 1
            case '9':
                y2, x2 = 0, 2
        #if there is a hole on the path avoid it
        if y1 == 3 and x2 == 0:
            for i in range(len(inputList)):
                for j in range(y1 - y2):
                    inputList[i] += '^'
                for j in range(x1 - x2):
                    inputList[i] += '<'
                inputList[i] += 'A'
        elif x1 == 0 and y2 == 3:
            for i in range(len(inputList)):
                for j in range(x2 - x1):
                    inputList[i] += '>'
                for j in range(y2 - y1):
                    inputList[i] += 'v'
                inputList[i] += 'A'
        #if there is no hole then multiply path in both directions
        else:
            for i in range(len(inputList)):
                s = inputList[i]
                if y2 > y1:
                    for j in range(y2 - y1):
                        s += 'v'
                    if x2 > x1:
                        for j in range(x2 - x1):
                            inputList[i] += '>'
                            s += '>'
                    else:
                        for j in range(x1 - x2):
                            inputList[i] += '<'
                            s += '<'
                    for j in range(y2 - y1):
                        inputList[i] += 'v'
                else:
                    for j in range(y1 - y2):
                        inputList[i] += '^'
                    if x2 > x1:
                        for j in range(x2 - x1):
                            inputList[i] += '>'
                            s += '>'
                    else:
                        for j in range(x1 - x2):
                            inputList[i] += '<'
                            s += '<'
                    for j in range(y1 - y2):
                        s += '^'
                inputList.append(s + 'A')
                inputList[i] += 'A'
        x1, y1 = x2, y2
    return list(dict.fromkeys(inputList))

def directionalCode(s, pos):
    code = list(s)
    [y1, x1] = pos
    inputList = [""]
    for digit in code:
        match digit:
            case 'A':
                y2, x2 = 0, 2
            case '^':
                y2, x2 = 0, 1
            case 'v':
                y2, x2 = 1, 1
            case '<':
                y2, x2 = 1, 0
            case '>':
                y2, x2 = 1, 2
        #if there is a hole on the path avoid it
        if y1 == 0 and x2 == 0:
            for i in range(len(inputList)):
                for j in range(y2 - y1):
                    inputList[i] += 'v'
                for j in range(x1 - x2):
                    inputList[i] += '<'
                inputList[i] += 'A'
        elif x1 == 0 and y2 == 0:
            for i in range(len(inputList)):
                for j in range(x2 - x1):
                    inputList[i] += '>'
                for j in range(y1 - y2):
                    inputList[i] += '^'
                inputList[i] += 'A'
        #if there is no hole then multiply path in both directions
        else:
            for i in range(len(inputList)):
                s = inputList[i]
                if y2 > y1:
                    for j in range(y2 - y1):
                        s += 'v'
                    if x2 > x1:
                        for j in range(x2 - x1):
                            inputList[i] += '>'
                            s += '>'
                    else:
                        for j in range(x1 - x2):
                            inputList[i] += '<'
                            s += '<'
                    for j in range(y2 - y1):
                        inputList[i] += 'v'
                else:
                    for j in range(y1 - y2):
                        inputList[i] += '^'
                    if x2 > x1:
                        for j in range(x2 - x1):
                            inputList[i] += '>'
                            s += '>'
                    else:
                        for j in range(x1 - x2):
                            inputList[i] += '<'
                            s += '<'
                    for j in range(y1 - y2):
                        s += '^'
                inputList.append(s + 'A')
                inputList[i] += 'A'
        x1, y1 = x2, y2
    return list(dict.fromkeys(inputList))

res = 0
for code in lines:
    pos = [3, 2]
    inputs1 = digitalCode(code, pos)
    print(inputs1)
    pos = [0, 2]
    inputs2 = []
    for i in inputs1:
        inputs2 += directionalCode(i, pos)
    print(inputs2)
    pos = [0, 2]
    inputs3 = []
    for i in inputs2:
        inputs3 += directionalCode(i, pos)
    print(inputs3)
    minlength = len(inputs3[0])
    for i in inputs3:
        inputLen = len(i)
        if inputLen < minlength:
            minlength = inputLen
    print(int(code[:-1]), minlength)
    # res += int(code[:-1]) * len(input3)
    print()
print(res)

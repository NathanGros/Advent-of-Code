lines = [list(map(int, list(l[:-1]))) for l in open("input.txt")]

res = 0
for l in lines:
    joltage = 0
    startIndex = 0
    for i in range(11, -1, -1):
        possibles = l[startIndex:len(l)-i]
        digit = max(possibles)
        startIndex += possibles.index(digit) + 1
        joltage += pow(10, i) * digit
    res += joltage

print(res)

lines = [l for l in open("input.txt")]

res = 0
dial = 50
for line in lines:
    if line[0] == 'L':
        dial -= int(line[1:])
        while dial < 0:
            dial += 100
    else:
        dial += int(line[1:])
        while dial >= 100:
            dial -= 100
    if dial == 0:
        res += 1

print(res)

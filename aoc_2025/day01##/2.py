lines = [l for l in open("input.txt")]

res = 0
dial = 50
for line in lines:
    rotation = int(line[1:])
    if line[0] == 'L':
        # full circle
        while rotation > 100:
            res += 1
            rotation -= 100
        # edge case
        if dial == 0:
            dial -= rotation
            if dial < 0:
                dial += 100
        # apply last rotation and check if it passed by 0
        else:
            dial -= rotation
            if dial <= 0:
                res += 1
            if dial < 0:
                dial += 100
    else:
        # full circle
        while rotation > 100:
            res += 1
            rotation -= 100
        # apply last rotation and check if it passed by 0
        dial += rotation
        if dial >= 100:
            res += 1
            dial -= 100

print(res)

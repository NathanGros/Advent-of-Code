lines = [l[:-1].split(',') for l in open("input.txt")][0]
lines = [l.split('-') for l in lines]

def isIdValid(s):
    for n in range(1, len(s) // 2 + 1):
        # only get divisors of len(s)
        if len(s) % n != 0:
            continue
        checkDigit = 0
        flagInvalidId = True
        while (checkDigit < len(s)):
            # print(s[checkDigit:checkDigit+n])
            if s[checkDigit:checkDigit+n] != s[:n]:
                flagInvalidId = False
            checkDigit += n
        if flagInvalidId:
            return False
    return True

res = 0
for l in lines:
    for id in range(int(l[0]), int(l[1]) + 1):
        if not isIdValid(str(id)):
            res += id
print(res)

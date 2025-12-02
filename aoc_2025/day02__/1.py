lines = [l[:-1].split(',') for l in open("input.txt")][0]
lines = [l.split('-') for l in lines]

def isIdValid(s):
    if len(s) % 2 == 1:
        return True
    patternLen = len(s) // 2
    return s[:patternLen] != s[patternLen:]

res = 0
for l in lines:
    for id in range(int(l[0]), int(l[1]) + 1):
        if not isIdValid(str(id)):
            res += id
print(res)

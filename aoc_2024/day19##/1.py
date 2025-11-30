lines = [l.split() for l in open("input.txt")]
available = [a.split(",")[0] for a in lines[0]]
patterns = []
for i in range(2, len(lines)):
    patterns.append(lines[i][0])

dic = {}
dic[''] = True

def canMakePattern(pattern, available):
    if pattern in dic:
        return dic[pattern]
    res = False
    for a in available:
        if pattern.startswith(a):
            res = res or canMakePattern(pattern[len(a):], available)
    dic[pattern] = res
    return res


res = 0
for p in patterns:
    if canMakePattern(p, available):
        res += 1
print(res)

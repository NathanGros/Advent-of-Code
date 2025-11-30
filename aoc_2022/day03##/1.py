lines = [(l[:int(len(l)/2)], l[int(len(l)/2):]) for l in open("input.txt")]

def val(c):
    value = ord(c)
    if ord('a') <= value <= ord('z'):
        return value - ord('a') + 1
    else:
        return value - ord('A') + 27

res = 0
for (s1, s2) in lines:
    for c in s1:
        if c in s2:
            res += val(c)
            break

print(res)

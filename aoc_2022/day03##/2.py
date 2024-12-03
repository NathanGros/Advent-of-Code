lines = [l.split()[0] for l in open("input.txt")]

groups = []
for i in range(int(len(lines)/3)):
    groups.append((lines[3*i+0], lines[3*i+1], lines[3*i+2]))
    
def val(c):
    value = ord(c)
    if ord('a') <= value <= ord('z'):
        return value - ord('a') + 1
    else:
        return value - ord('A') + 27

res = 0
for (s1, s2, s3) in groups:
    for c in s1:
        if c in s2 and c in s3:
            res += val(c)
            break

print(res)

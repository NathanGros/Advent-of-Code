lines = [l[:-1] for l in open("input.txt")]

#check horizontal
res = 0
for l in lines:
    for i in range(len(l) - 3):
        s = l[i:i+4]
        if s == 'XMAS' or s == 'SAMX':
            res += 1

#check vertical
for i in range(len(lines[0])):
    for j in range(len(lines) - 3):
        s = lines[j][i] + lines[j+1][i] + lines[j+2][i] + lines[j+3][i]
        if s == 'XMAS' or s == 'SAMX':
            res += 1

#check diagonal upper left to bottom right
for i in range(len(lines[0]) - 3):
    for j in range(len(lines) - 3):
        s = lines[j][i] + lines[j+1][i+1] + lines[j+2][i+2] + lines[j+3][i+3]
        if s == 'XMAS' or s == 'SAMX':
            res += 1

#check diagonal upper right to bottom left
for i in range(len(lines[0]) - 3):
    for j in range(len(lines) - 3):
        s = lines[j][len(lines[0])-i - 1] + lines[j+1][len(lines[0])-i - 2] + lines[j+2][len(lines[0])-i - 3] + lines[j+3][len(lines[0])-i - 4]
        if s == 'XMAS' or s == 'SAMX':
            res += 1

print(res)

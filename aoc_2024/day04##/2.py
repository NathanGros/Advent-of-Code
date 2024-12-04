lines = [l.split()[0] for l in open("input.txt")]

res=0
for i in range(len(lines[0]) - 2):
    for j in range(len(lines) - 2):
        s = lines[j][i] + lines[j][i+2] + lines[j+1][i+1] + lines[j+2][i] + lines[j+2][i+2]
        if s == 'MMASS' or s == 'SMASM' or s == 'SSAMM' or s == 'MSAMS':
            res += 1

print(res)

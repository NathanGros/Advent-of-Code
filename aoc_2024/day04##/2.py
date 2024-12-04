lines = [l[:-1] for l in open("input.txt")]

res=0
for i in range(len(lines[0]) - 2):
    for j in range(len(lines) - 2):
        #make a string which contains the caracters of the cross starting at i j and going down and right
        s = lines[j][i] + lines[j][i+2] + lines[j+1][i+1] + lines[j+2][i] + lines[j+2][i+2]
        #check for the 4 possible orientations of the searched cross
        if s == 'MMASS' or s == 'SMASM' or s == 'SSAMM' or s == 'MSAMS':
            res += 1

print(res)

lines = [l for l in open("input.txt")]

mul = []
index = -1
buffer = ""
flag = False
flagisdoing = True
for l in lines:
    for c in l:
        buffer += c
        #stop reading inside of mul
        if c == ")":
            flag = False
        #if invalid character inside of mul, cancel everything
        if flag == True and flagisdoing and c != "," and c != "0" and c != "1" and c != "2" and c != "3" and c != "4" and c != "5" and c != "6" and c != "7" and c != "8" and c != "9":
            mul.pop()
            index -= 1
            flag = False
        #fill mul
        if flag and flagisdoing:
            mul[index] += c
        #start reading inside of mul
        if buffer[-4:] == "mul(" and flagisdoing:
            flag = True
            mul.append("")
            index += 1
        #start reading muls if do encountered
        if buffer[-4:] == "do()":
            flagisdoing = True
        #stop reading muls if don't encountered
        if buffer[-7:] == "don't()":
            flagisdoing = False

print(sum([int(a) * int(b) for [a, b] in [s.split(",") for s in mul]]))

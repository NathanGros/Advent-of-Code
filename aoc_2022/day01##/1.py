lines = [list(map(int, l.split())) for l in open("input.txt")]

l = [0]
for i in lines:
    if i == []:
        l.append(0)
    else:
        l[len(l)-1] += i[0]

print(max(l))

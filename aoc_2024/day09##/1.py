lines = [list(map(int, list(l.split()[0]))) for l in open("input.txt")][0]

#make list of blocks
l = []
blockId = 0
isEmpty = 0
for n in lines:
    if isEmpty == 0:
        for i in range(n):
            l.append(blockId)
        blockId += 1
    else:
        for i in range(n):
            l.append(-1)
    #switch isEmpty between 1 and 0
    isEmpty = 1 - isEmpty

for i in range(len(l)-1, 0, -1):
    if (-1) in l:
        if l[i] != -1:
            #insert last element at the position of the first -1
            l[l.index(-1)] = l.pop(i)
        else:
            l.pop()
    #exit when there is no space left
    else:
        break

res = 0
for i in range(len(l)):
    res += l[i] * i
print(res)

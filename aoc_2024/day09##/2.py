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

blockLength = 1
for i in range(len(l)-2, -1, -1):
    print(i)
    #get size of rightmost block
    if l[i] == l[i+1]:
        blockLength += 1
    #if end of block reached, try to move it
    else:
        #check if the block is not an empty block
        if l[i+1] != -1:
            insertBlockLength = 0
            #search for a place to move the block
            for j in range(i+blockLength):
                #if first -1 found
                if insertBlockLength == 0 and l[j] == -1:
                    insertBlockLength = 1
                #reset counter
                elif l[j] != -1:
                    insertBlockLength = 0
                #increase insertBlockLength size
                elif j > 0 and l[j-1] == l[j] == -1:
                    insertBlockLength += 1
                #if place to move found then move the block
                if insertBlockLength >= blockLength:
                    for k in range(blockLength):
                        l[j - insertBlockLength + k + 1] = l[i+1+k]
                        l[i+1+k] = -1
                    break
        blockLength = 1
        

res = 0
for i in range(len(l)):
    if l[i] != -1:
        res += l[i] * i
print(res)

lines = [list(map(int, list(l.split()[0]))) for l in open("test_input.txt")][0]
print(lines)

idBlockR = int((len(lines)-1)/2)
totalLength = 0
for i in range(int((len(lines)+1)/2)):
    totalLength += lines[2*i]

res = 0
idBlockL = -1
index = 0
isMovedBlock = 1
nbCachedR = lines[len(lines)-1]
indexR = 0
done = False
for n in lines:
    if not done:
        isMovedBlock = 1-isMovedBlock
        if isMovedBlock == 0:
            idBlockL += 1
        for i in range(n):
            if not done:
                if idBlockL == idBlockR:
                    done = True
                if isMovedBlock == 0:
                    print(index, idBlockL)
                    res += index * idBlockL
                else:
                    if nbCachedR == 0:
                        idBlockR -= 1
                        indexR += 1
                        if idBlockL == idBlockR:
                            done = True
                            break
                        nbCachedR = lines[len(lines) - 1 - 2*indexR]-1
                    else:
                        nbCachedR -= 1
                    print(index, idBlockR)
                    res += index * idBlockR
                # print(res, idBlockL, idBlockR, isMovedBlock, n, index, nbCachedR)
                index += 1

print()
print(res)

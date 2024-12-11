line = [list(map(int, l.split())) for l in open("input.txt")][0]

#dictionary of number of descendants of a stone after n blinks
stones = {}

def findNbChildren(stone, n):
    #if no blink then no children so return 1
    if n == 0:
        return 1
    #return the stoneue if it is already know
    if (stone, n) in stones:
        return stones[stone, n]
    #if the stoneue is not in the dictionary then compute it
    if stone == 0:
        res = findNbChildren(1, n-1)
    elif len(str(stone)) % 2 == 1:
        res = findNbChildren(stone * 2024, n-1)
    else:
        s = str(stone)
        halflength = int(len(s)/2)
        res = findNbChildren(int(s[:halflength]), n-1) + findNbChildren(int(s[halflength:]), n-1)
    #add the stone to the dictionary
    stones[stone, n] = res
    return res

res = 0
for n in line:
    #compute for every stone of the input, for 75 blinks
    res += findNbChildren(n, 75)
print(res)

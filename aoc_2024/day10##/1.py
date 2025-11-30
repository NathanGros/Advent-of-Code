lines = [list(map(int, list(l.split()[0]))) for l in open("input.txt")]

def findScore(height, width, i, j, n):
    if n == 9:
        return [(i, j)]
    else:
        #check if there is a higher number in the four directions and recursion on them
        l = []
        if i > 0 and lines[i-1][j] == n+1:
            l += findScore(height, width, i-1, j, n+1)
        if j > 0 and lines[i][j-1] == n+1:
            l += findScore(height, width, i, j-1, n+1)
        if i < height-1 and lines[i+1][j] == n+1:
            l += findScore(height, width, i+1, j, n+1)
        if j < width-1 and lines[i][j+1] == n+1:
            l += findScore(height, width, i, j+1, n+1)
        return l
    

res = 0
for i in range(len(lines)):
    for j in range(len(lines[0])):
        #process all zeros
        if lines[i][j] == 0:
            l = list(dict.fromkeys(findScore(len(lines), len(lines[0]), i, j, 0)))
            res += len(l)
print(res)

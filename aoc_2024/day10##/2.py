lines = [list(map(int, list(l.split()[0]))) for l in open("input.txt")]

def findRating(height, width, i, j, n):
    if n == 9:
        return 1
    else:
        #check if there is a higher number in the four directions and recursion on them
        sum = 0
        if i > 0 and lines[i-1][j] == n+1:
            sum += findRating(height, width, i-1, j, n+1)
        if j > 0 and lines[i][j-1] == n+1:
            sum += findRating(height, width, i, j-1, n+1)
        if i < height-1 and lines[i+1][j] == n+1:
            sum += findRating(height, width, i+1, j, n+1)
        if j < width-1 and lines[i][j+1] == n+1:
            sum += findRating(height, width, i, j+1, n+1)
        return sum
    

res = 0
for i in range(len(lines)):
    for j in range(len(lines[0])):
        #process all zeros
        if lines[i][j] == 0:
            rating = findRating(len(lines), len(lines[0]), i, j, 0)
            res += rating
print(res)

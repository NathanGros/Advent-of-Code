lines = [list(map(int, list(l[:-1]))) for l in open("input.txt")]

res = 0
for l in lines:
    digit1 = max(l[:-1])
    digit2 = max(l[l[:-1].index(digit1)+1:])
    res += 10*digit1 + digit2

print(res)

lines = [[int(a), list(map(int, b.split()))] for (a, b) in [l.split(':') for l in open("input.txt")]]

def copyFirsts(l):
    return ([e for e in l][:-1], l[len(l)-1])

def solvable(testVal, numbers):
    #if there is only 1 number then it has to be the same, or else it's not solvable
    if len(numbers) == 1:
        return testVal == numbers[0]
    #get last element of operational numbers
    numbers, n = copyFirsts(numbers)
    #if we can divide then test division and substraction
    if testVal%n == 0:
        return solvable(testVal/n, numbers) or solvable(testVal-n, numbers)
    #if we can't divide then just test substraction
    else:
        return solvable(testVal-n, numbers)

res = 0
for l in lines:
    testVal = l[0]
    numbers = l[1]
    if solvable(l[0], l[1]):
        res += testVal
print(res)

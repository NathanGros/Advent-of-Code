line = [list(map(int, l.split())) for l in open("input.txt")][0]

#dictionary of number of descendants of a value after n blinks
vals = {}

def findNb(val, n):
    #if no blink then return 1
    if n == 0:
        return 1
    else:
        #return the value if it is already know
        if (val, n) in vals:
            return vals[val, n]
        #if the value is not in the dictionary then compute it and add it to the dictionary
        else:
            l = []
            #problem rules
            if val == 0:
                l.append(1)
            elif len(str(val)) % 2 == 1:
                l.append(val * 2024)
            else:
                s = str(val)
                halfLength = int(len(s)/2)
                l.append(int(s[:halfLength]))
                l.append(int(s[halfLength:]))
            res = 0
            #compute descendants
            for v in l:
                key = findNb(v, n-1)
                vals[v, n-1] = key
                res += key
            return res

res = 0
for n in line:
    #compute for every stone of the input, for 75 blinks
    res += findNb(n, 75)
print(res)

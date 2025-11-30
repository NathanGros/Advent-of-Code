line = [list(map(int, l.split())) for l in open("input.txt")][0]

#compute for every stone of the input, for 25 blinks
for i in range(25):
    l = []
    for stone in line:
        #problem rules
        if stone == 0:
            l.append(1)
        elif len(str(stone)) % 2 == 1:
            l.append(stone * 2024)
        else:
            s = str(stone)
            halfLength = int(len(s)/2)
            l.append(int(s[:halfLength]))
            l.append(int(s[halfLength:]))
    line = l

print(len(line))

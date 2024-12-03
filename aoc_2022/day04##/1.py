lines = [l.split()[0].split(',') for l in open("input.txt")]
tablestemp = [(l[0].split('-'), l[1].split('-')) for l in lines]
tables = [[int(a), int(b), int(c), int(d)] for ([a, b], [c, d]) in tablestemp]

def overlap(a, b, c, d):
    if (a >= c and b <= d) or (c >= a and d <= b):
        return 1
    else:
        return 0

print(sum(overlap(a, b, c, d) for [a, b, c, d] in tables))

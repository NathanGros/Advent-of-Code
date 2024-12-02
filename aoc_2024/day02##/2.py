lines = [list(map(int, l.split())) for l in open("input.txt")]

def isvalid(l):
    for i in range(len(l)):
        l2 = l.copy()
        l2.pop(i)
        conditions = [[
            l2[i] < l2[i+1],
            l2[i] > l2[i+1],
            0 < abs(l2[i+1] - l2[i]) <= 3
            ] for i in range(len(l2) - 1)
        ]
        if (all([c[0] for c in conditions]) or all([c[1] for c in conditions])) and all([c[2] for c in conditions]):
            return True
    return False

print(sum(isvalid(l) for l in lines))

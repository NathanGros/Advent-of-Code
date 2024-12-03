lines = [l.split() for l in open("input.txt")]
print(lines)

def value(c):
    match c:
        case 'X':
            return 1
        case 'Y':
            return 2
        case 'Z':
            return 3

def win(c1, c2):
    match c1, c2:
        case 'A', 'X':
            return 3
        case 'A', 'Y':
            return 6
        case 'A', 'Z':
            return 0
        case 'B', 'X':
            return 0
        case 'B', 'Y':
            return 3
        case 'B', 'Z':
            return 6
        case 'C', 'X':
            return 6
        case 'C', 'Y':
            return 0
        case 'C', 'Z':
            return 3


scores = [win(a, b) + value(b) for (a, b) in lines]
print(sum(scores))

lines = [l.split() for l in open("input.txt")]
print(lines)

def win(c):
    match c:
        case 'X':
            return 0
        case 'Y':
            return 3
        case 'Z':
            return 6

def value(c1, c2):
    match c1, c2:
        case 'A', 'X':
            return 3
        case 'A', 'Y':
            return 1
        case 'A', 'Z':
            return 2
        case 'B', 'X':
            return 1
        case 'B', 'Y':
            return 2
        case 'B', 'Z':
            return 3
        case 'C', 'X':
            return 2
        case 'C', 'Y':
            return 3
        case 'C', 'Z':
            return 1


scores = [value(a, b) + win(b) for (a, b) in lines]
print(sum(scores))

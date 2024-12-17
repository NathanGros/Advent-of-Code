lines = [l.split() for l in open("input.txt")]
a = int(lines[0][2])
b = int(lines[1][2])
c = int(lines[2][2])
program = list(map(int, lines[4][1].split(',')))
instructions = []
for i in range(int(len(program)/2)):
    instructions.append([program[2*i], program[2*i + 1]])
print(instructions)

def getCombo(combo):
    match combo:
        case 0:
            return 0
        case 1:
            return 1
        case 2:
            return 2
        case 3:
            return 3
        case 4:
            return a
        case 5:
            return b
        case 6:
            return c

def adv(combo, a):
    power = getCombo(combo)
    return int(a / (2 ** power))

def bxl(combo, b):
    return b ^ combo

def bst(combo):
    return getCombo(combo) % 8

def jnz(combo, ptr, a):
    if a == 0:
        return ptr
    else:
        return combo - 1

def bxc(b, c):
    return b ^ c

def out(combo):
    return getCombo(combo) % 8

def bdv(combo, a):
    return adv(combo, a)

def cdv(combo, a):
    return adv(combo, a)

#tests
# a = 2024
# b = 0
# c = 0
# instructions = [[0, 1], [5, 4], [3, 0]]
#program
ptr = 0
outList = []
while ptr < len(instructions):
    [opCode, operand] = instructions[ptr]
    match opCode:
        case 0:
            print("adv")
            a = adv(operand, a)
        case 1:
            print("bxl")
            b = bxl(operand, b)
        case 2:
            print("bst")
            b = bst(operand)
        case 3:
            print("jnz")
            ptr = jnz(operand, ptr, a)
        case 4:
            print("bxc")
            b = bxc(b, c)
        case 5:
            print("out")
            outList.append(out(operand))
        case 6:
            print("bdv")
            b = bdv(operand, a)
        case 7:
            print("cdv")
            c = cdv(operand, a)
    ptr += 1

print("a:", a)
print("b:", b)
print("c:", c)
print(','.join(map(str, outList)))

# lines = [l.split() for l in open("input.txt")]
# a = int(lines[0][2])
# b = int(lines[1][2])
# c = int(lines[2][2])
# program = list(map(int, lines[4][1].split(',')))
# instructions = []
# for i in range(int(len(program)/2)):
#     instructions.append([program[2*i], program[2*i + 1]])
# print(instructions)
#
# def getCombo(combo):
#     match combo:
#         case 0:
#             return 0
#         case 1:
#             return 1
#         case 2:
#             return 2
#         case 3:
#             return 3
#         case 4:
#             return a
#         case 5:
#             return b
#         case 6:
#             return c
#
# def adv(combo, a):
#     power = getCombo(combo)
#     return int(a / (2 ** power))
#
# def bxl(combo, b):
#     return b ^ combo
#
# def bst(combo):
#     return getCombo(combo) % 8
#
# def jnz(combo, ptr, a):
#     if a == 0:
#         return ptr
#     else:
#         return combo - 1
#
# def bxc(b, c):
#     return b ^ c
#
# def out(combo):
#     return getCombo(combo) % 8
#
# def bdv(combo, a):
#     return adv(combo, a)
#
# def cdv(combo, a):
#     return adv(combo, a)
#
# #tests
# # a = 2024
# # b = 0
# # c = 0
# # instructions = [[0, 1], [5, 4], [3, 0]]
# #program
#
# res = 1000000000000000
# programList = [program]
# outList = []
# while outList != program:
#     outList = []
#     a = res
#     ptr = 0
#     while ptr < len(instructions):
#         [opCode, operand] = instructions[ptr]
#         match opCode:
#             case 0:
#                 a = adv(operand, a)
#             case 1:
#                 b = bxl(operand, b)
#             case 2:
#                 b = bst(operand)
#             case 3:
#                 ptr = jnz(operand, ptr, a)
#             case 4:
#                 b = bxc(b, c)
#             case 5:
#                 outList.append(out(operand))
#             case 6:
#                 b = bdv(operand, a)
#             case 7:
#                 c = cdv(operand, a)
#         ptr += 1
#     res += 1
#     print("loop", outList, res)
#     if not outList in programList:
#         programList.append(outList)
# res -= 1
#
# print(res)

def execute(a):
    out = []
    while a != 0:
        b = (a % 8) ^ 5
        c = int(a / (2 ** b))
        b = (b ^ 6) ^ c
        out.append(b % 8)
        a = int(a / 8)
    return out

print(execute(60589763))

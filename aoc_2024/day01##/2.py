lines = [list(map(int, l.split())) for l in open("input.txt")]
lists = list(zip(*lines))
print(sum([i * lists[1].count(i) for i in lists[0]]))

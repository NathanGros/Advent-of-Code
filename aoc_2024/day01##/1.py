lines = [list(map(int, l.split())) for l in open("input.txt")]
sorted_lists = list(zip(*list(map(sorted, zip(*lines)))))
print(sum([abs(b - a) for (a, b) in sorted_lists]))

lines = [list(l.split()[0]) for l in open("input.txt")]

timelines = [[lines[0].index('S'), 1]]

def addTimeline(timelines, t):
    found = False
    for t2 in timelines:
        if t2[0] == t[0]:
            t2[1] = t2[1] + t[1]
            found = True
    if not found:
        timelines.append(t)
    return timelines

nbTimelines = 1
for l in lines[1:]:
    newTimelines = []
    for t in timelines:
        if l[t[0]] == '.':
            newTimelines = addTimeline(newTimelines, t)
        elif l[t[0]] == '^':
            nbTimelines += t[1]
            if t[0]-1 >= 0:
                newTimelines = addTimeline(newTimelines, [t[0]-1, t[1]])
            if t[0]+1 < len(l):
                newTimelines = addTimeline(newTimelines, [t[0]+1, t[1]])
    timelines = newTimelines

print(nbTimelines)

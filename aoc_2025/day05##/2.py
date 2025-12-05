lines = [l.split() for l in open("input.txt")]

intervals = [list(map(int, a[0].split('-'))) for a in lines[:lines.index([])]]
correctedIntervals = []

for [a, b] in intervals:
    # just add it if correctedIntervals is empty
    if len(correctedIntervals) == 0:
        correctedIntervals.insert(0, [a, b])
        continue

    # find corrected interval start
    j = len(correctedIntervals) - 1
    c = correctedIntervals[j][0]
    d = correctedIntervals[j][1]
    while j > 0 and c > a:
        j -= 1
        c = correctedIntervals[j][0]
        d = correctedIntervals[j][1]
    start = a
    startExists = False
    if c < a and d >= a - 1:
        start = c
        startExists = True

    # find corrected interval end
    k = j
    e = correctedIntervals[k][0]
    f = correctedIntervals[k][1]
    while k < len(correctedIntervals) - 1 and f < b:
        k += 1
        e = correctedIntervals[k][0]
        f = correctedIntervals[k][1]
    end = b
    endExists = False
    if f > b and e <= b + 1:
        end = f
        endExists = True

    # insert interval [start, end] in correctedIntervals

    insertList = []
    # add all correctedIntervals values
    for interval in correctedIntervals:
        insertList.append(interval[0])
        insertList.append(interval[1])
    # add start and end without duplicates
    for index in range(len(insertList) - 1, -1, -1):
        if insertList[index] == start or insertList[index] == end:
            del insertList[index]
    insertList.append(start)
    insertList.append(end)
    insertList.sort()
    # delete values between start and end
    for index in range(len(insertList) - 1, -1, -1):
        if start < insertList[index] < end:
            del insertList[index]
    # insert back the corrected intervals in correctedIntervals
    correctedIntervals = []
    for i in range(0, len(insertList), 2):
        correctedIntervals.append([insertList[i], insertList[i+1]])

res = 0
for [a, b] in correctedIntervals:
    res += b - a + 1
print(res)

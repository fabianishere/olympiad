import itertools, functools

n = int(input())
sectors = [n] + [int(input()) for _ in range(0, n - 1)]

_min = lambda x: min(x) if x else 0

def distances(items):
    length = len(items)
    items = list(enumerate(items + items + items))
    return [_min([abs(it[0] - index) for it in items if it[1] == items[index][1] + 1]) for index in range(length + 1, length * 2)]

def shuffle(items, ignore=[]):
    for i in range(0, n):
        for j in range(0, n):
            if (i, j) in ignore or (j, i) in ignore: continue
            new = list(items)
            new[i], new[j] = new[j], new[i]
            yield new, (i, j)

prev = (0, (sectors, (0, 0)))
ignore = []
while True:
    nex = max(map(lambda x: (sum(map(pow, distances(x[0]), itertools.repeat(2))), x), shuffle(prev[1][0], ignore)), key=lambda x: x[0])
    if nex[0] == prev[0]: break
    elif nex[0] < prev[0]: continue
    ignore.append(nex[1][1])
    prev = nex
print(prev[0])
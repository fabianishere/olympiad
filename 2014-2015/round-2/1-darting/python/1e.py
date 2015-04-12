import itertools, functools

n = int(input())
sectors = [n] + [int(input()) for _ in range(0, n - 1)]

def distances(items):
    length = len(items)
    items = list(enumerate(items + items + items))
    return [min([abs(it[0] - index) for it in items if it[1] == items[index][1] + 1]) for index in range(length + 1, length * 2)]

print(sum(map(pow, distances(sectors), itertools.repeat(2))))
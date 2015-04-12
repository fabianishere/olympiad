import itertools

n = int(input())
sectors = [n] + [int(input()) for _ in range(0, n - 1)]

def neighbours(items):
    before = itertools.chain([next(reversed(items))], items)
    after = itertools.chain(items, [next(iter(items))])
    next(after)
    return zip(before, items, after)

print(len(list(filter(lambda x: not (x[0] <= n // 2 < x[1] or x[1] <= n // 2 < x[0]), neighbours(sectors)))))






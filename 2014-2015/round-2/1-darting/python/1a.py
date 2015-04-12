import itertools

n = int(input())
sectors = [n] + [int(input()) for _ in range(0, n - 1)]

def neighbours(items):
    before = itertools.chain([next(reversed(items))], items)
    after = itertools.chain(items, [next(iter(items))])
    next(after)
    return zip(before, items, after)

print(len(list(filter(lambda x: not (all(y < x[1] for y in (x[0], x[2])) or all(y > x[1] for y in (x[0], x[2]))), neighbours(sectors)))))


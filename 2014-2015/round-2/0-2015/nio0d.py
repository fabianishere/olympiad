import itertools
m = []
while True:
    e = int(input())
    m.append(e)
    if e in m[:-1]: break
def rec_sum(iterable, initial=0):
    while True:
        initial += next(iterable)
        yield initial
start = m[:m.index(m[-1])]
loop = m[m.index(m[-1]):-1]
if sum(loop) % 2015 == 0:
    print(0)
else:
    print(next(filter(lambda x: x % 2015 == 0, rec_sum(itertools.cycle(loop), sum(start)))))


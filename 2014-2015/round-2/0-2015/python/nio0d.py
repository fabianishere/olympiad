import itertools
m = []
while True:
    e = int(input())
    m.append(e)
    if e in m[:-1]: break

start = m[:m.index(m[-1])]
loop = m[m.index(m[-1]):-1]
sums = itertools.accumulate(itertools.chain(start, itertools.cycle(loop)))
test = lambda x: x % 2015 == 0
if sum(loop) % 2015 == 0 and not list(filter(test, itertools.accumulate(itertools.chain(start, loop)))):
    print(0)
else: 
    print(next(filter(lambda t: test(t[1]), enumerate(sums)))[0] + 1)

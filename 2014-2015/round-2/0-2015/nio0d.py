import itertools
m = []
while True:
    e = int(input())
    m.append(e)
    if e in m[:-1]: break
def sumr(iterable, initial=0):
    while True:
        try:
            initial += next(iterable)
            yield initial
        except StopIteration:
            return
start = m[:m.index(m[-1])]
loop = m[m.index(m[-1]):-1]
sums = sumr(itertools.chain(start, itertools.cycle(loop)))
test = lambda x: x % 2015 == 0
if sum(loop) % 2015 == 0 and not list(filter(test, sumr(itertools.chain(start, loop)))):
    print(0)
else: 
    print(next(filter(lambda t: test(t[1]), enumerate(sums)))[0] + 1)

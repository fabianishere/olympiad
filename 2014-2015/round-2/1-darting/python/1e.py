import itertools
n = int(input())
s = itertools.chain([n], ([int(input()) for _ in range(0, n - 1)]))
sectors = list(s)
sectors_e = list(enumerate(sectors))
def mapping(o):
    index, sector = o
    dists = [abs(t[0] - index) for t in sectors_e if t[1] == sector + 1]
    dists.extend([abs(index + len(sectors) - t[0]) for t in sectors_e if t[1] == sector + 1])
    dists.extend([abs(len(sectors) - index + t[0]) for t in sectors_e if t[1] == sector + 1])
    if sector + 1 in sectors: return min(dists)
    else: return 0

print(sum(map(lambda d: d**2, map(mapping, sectors_e))))

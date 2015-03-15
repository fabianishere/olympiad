n = int(input())
sectors = list(enumerate([int(input()) for _ in range(0, n - 1)]))

def mapping(o):
    index, sector = o
    if index > 0: prev = sectors[index - 1]
    else: prev = (0, sectors[-1][1])
    if index + 1 < len(sectors): next = sectors[index + 1]
    else: next = (0, sectors[0][1])
    return (prev[1], sector, next[1])

def mapping_distance(o):
    prev, sector, next = o
    if prev != -1: return abs(sector - prev)
    else: return -1

filtering = lambda d: d >= 0

print(min(list(filter(filtering, map(mapping_distance, map(mapping, sectors))))))



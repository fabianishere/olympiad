m = []
while True:
    e = int(input())
    m.append(e)
    if e in m[:-1]: break
start = m.index(m[-1])
print(start)
print(len(m) - start - 1)

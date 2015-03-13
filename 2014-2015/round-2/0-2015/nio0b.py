m = []
while True:
    e = int(input())
    m.append(e)
    if e in m[:-1]: break

last = m[-1]
start = m.index(last)
print(m)
print(start)
print(len(m) - start - 1)

m = []
while True:
    e = int(input())
    m.append(e)
    if e in m[:-1]: break

ls = m.index(m[-1])
ll = len(m) - ls - 1
print(m[ls + (2015 - ls - 1) % ll])

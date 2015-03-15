m = []
while True:
    e = int(input())
    if e in m: break
    m.append(e)

print(max(m))
print(min(m))

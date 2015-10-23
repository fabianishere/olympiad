import sys
m = [list(map(int, input())) for _ in range(0, 10)]

def inspect(m, x, y, n):
    if 0 <= x < 10 and 0 <= y < 10:
        if m[y][x] > 0:
            return n
        m[y][x] = n + 3
        inspect(m, x + 1, y, n)
        inspect(m, x - 1, y, n)
        inspect(m, x, y + 1, n)
        inspect(m, x, y - 1, n)
        return n + 1
    return n

m = list(m)
y = 0
n = 0
while y != 10:
    x = 0
    while x != 10:
        n = inspect(m, x, y, n)
        x = x + 1
    y = y + 1
print(n)
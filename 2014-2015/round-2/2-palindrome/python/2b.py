word = input()
palindromes = []
for i in range(0, len(word)):
	for j in range(0, len(word)):
		if i == j: continue
		p = word[i:j]
		if p == p[::-1] and p: palindromes.append(p)
print(max(filter(lambda  x: x > 1, map(lambda x: len(x), set(palindromes)))))

word = input()

def palindromes(word):
	palindromes = []
	for i in range(0, len(word)):
		for j in range(0, len(word)):
			if i == j: continue
			p = word[i:j]
			if p == p[::-1] and p: palindromes.append(p)
		return palindromes()

def lenp(palindromes):
	return map(lambda x: len(x), palindromes())

ws = []
for i in range(0, len(word)):
	for j in range(0, 25):
		w = list(word)
		w[i] = chr(65 + j)
		ws.append("".join(w))
print(ws)



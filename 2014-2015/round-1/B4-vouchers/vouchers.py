vouchers = [1, 2, 3, 5, 8, 9, 18, 19, 46, 154, 313]

def main():
	for b in range(1, max(vouchers)):
		# Amount can be created with one voucher.
		if b in vouchers:
			continue
		bins = []
		ws = sorted(vouchers, reverse=True)
		for w in ws:
			for x in bins:
				if sum(x) + w <= b:
					x.append(w)
					w = None
					break
			if w != None:
				bins.append([w])
		r = list(map(sum, bins))
		if b not in list(map(sum, bins)):
			print(b)
			return
main()
			

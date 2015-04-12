#!/usr/bin/env python3
# -*- coding: utf-8 -*- 

# Copyright 2014 Fabian M.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# 	http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
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

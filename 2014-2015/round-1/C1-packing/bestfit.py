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

# Best fit algorithm implementation in Python as solution for problem C1.
# Reads the size of each bin, the amount of numbers and the numbers from
#	the standard input and calculates the least amount of bins needed
#	to pack these numbers in a bin of the given size.
if __name__ == "__main__":
	b, n = [int(input()) for x in range(0, 2)]
	ws = sorted([int(input()) for x in range(0, n)], reverse=True)
	bins = []
	for w in ws:
		for x in bins:
			if sum(x) + w <= b:
				x.append(w)
				w = None
				break
		if w != None:
			bins.append([w])
		bins = [bins[index] for index, x in sorted(enumerate(map(sum, bins)), key=lambda k: k[1], reverse=True)]
	print(len(bins))

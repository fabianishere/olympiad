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
import itertools

# Read four columns and rows of zero's and ones and print the
#	amount of adjacents for zero and one.
if __name__ == "__main__":
	adjacents = [[0] * 4, [0] * 4]
	rows = list(map(lambda x: list(map(lambda x: int(x), input())), range(0, 4)))
	for row in rows + list(zip(*rows[::-1])):
		for key, group in itertools.groupby(row):
			adjacents[key][len(list(group)) - 1] += 1
	print("\n".join(["{0} {1}".format(adjacents[0][index], adjacents[1][index]) for index in range(1, 4)]))
	
			

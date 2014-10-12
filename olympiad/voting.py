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

# Voting program, solution for problem A5.
# TODO: explain what it does
if __name__ == "__main__":
	k, d = [int(input()) for i in range(0, 2)]
	preferences = list([list(map(lambda x: int(x), input().split())) for i in range(0, d)])
	voting = [0] * k
	while True:
		for preference in preferences:
			preference = [choice for choice in preference if voting[choice - 1] >= 0]
			voting[preference[0] - 1] += (-1, 1)[voting[preference[0] - 1] >= 0]
		winners = list(filter(lambda x: x > d // 2, voting))
		if (len(winners) > 0):
			break
		indices = [i for i, x in enumerate(voting) if x == min(filter(lambda x: x >= 0, voting))]
		for index in (indices, indices[1:])[len(indices) > 1]:
			voting[index] = -1
		winners = list(filter(lambda x: x[1] > 0, enumerate(voting)))
		if (len(winners) <= 1):
			break
		voting = list(map(lambda x: (x, 0)[x >= 0], voting))
	print(winners[0][0] + 1)

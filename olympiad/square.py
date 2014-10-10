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

# Read postive integers from stdin until zero occurs and print the
# 	square of the odd integers.
if __name__ == "__main__":
	numbers = []
	while True:
		read = int(input())
		if read is 0: break
		numbers.append(read)
	print(sum(map(lambda x: x*x, filter(lambda x: x & 0x1, numbers))))
            

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

# Read name from standard input and print the length, the amount of 
#	uppercase characters, the amount of unique characters and
#	the reversed name.
if __name__ == "__main__":
	name = input()
	print(len(name))
	print(len(list(filter(lambda x: x.isupper(), name))))
	print(len(set(name)))
	print("".join(reversed(name)))

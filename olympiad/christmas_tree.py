#!/usr/bin/env python3
# -*- coding: utf-8 -*- 

#   Copyright 2014 Fabian M.
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#       http://www.apache.org/licenses/LICENSE-2.0
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.

# Generate a christmas tree in ASCII.
#
# Keyword arguments:
# z -- the height of the christmas tree minus the base (height = z + 1)
# Returns a multiple line string containing the ASCII christmas tree.
def tree(z):
	# This is evil!
	return "\n".join("".join(l) \
		for l in map((lambda y: list(("*", "-")[x < y or x > 2*z-y-2] \
		for x in range(0, 2*z-1))), reversed([z-1]+list(range(0, z)))))

if __name__ == "__main__":
    	print(tree(int(input())))

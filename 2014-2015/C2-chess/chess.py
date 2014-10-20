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
import copy, time

def traverse(matrix, size, point, step, steps = []):
	res = []
	x, y = point
	xb, yb = size
	xs, ys = step
	matrix = copy.deepcopy(matrix)
	matrix[y][x] += 1
	steps = copy.deepcopy(steps)
	steps.append(point)

	print("point={0}, steps={1}".format(point, len(steps)))
	for row in matrix:
		print(row)
	time.sleep(0.05)
	test = [x for y in matrix for x in filter(lambda x: x == 0, y)]
	if len(test) <= 0:
		return [(True, len(steps), steps)]
	if 0 <= x + xs < xb and 0 <= y + ys < yb and matrix[y + ys][x + xs] == 0:
		res.extend(traverse(matrix, size, (x + xs, y + ys), step, steps))
	if 0 <= x - xs < xb and 0 <= y + ys < yb and matrix[y + ys][x - xs] == 0:
		res.extend(traverse(matrix, size, (x - xs, y + ys), step, steps))
	if 0 <= x + ys < xb and 0 <= y - xs < yb and matrix[y - xs][x + ys] == 0:
		res.extend(traverse(matrix, size, (x + ys, y - xs), step, steps)) 
	if 0 <= x - xs < xb and 0 <= y - ys < yb and matrix[y - ys][x - xs] == 0:
		res.extend(traverse(matrix, size, (x - xs, y - ys), step, steps))
	if 0 <= x + xs < xb and 0 <= y - ys < yb and matrix[y - ys][x + xs] == 0:
		res.extend(traverse(matrix, size, (x + xs, y - ys), step, steps))
 
	if 0 <= x + ys < xb and 0 <= y + xs < yb and matrix[y + xs][x + ys] == 0:
		res.extend(traverse(matrix, size, (x + ys, y + xs), step, steps))
	if 0 <= x - ys < xb and 0 <= y + xs < yb and matrix[y + xs][x - ys] == 0:
		res.extend(traverse(matrix, size, (x - ys, y + xs), step, steps))
	if 0 <= x - ys < xb and 0 <= y - xs < yb and matrix[y - xs][x - ys] == 0:
		res.extend(traverse(matrix, size, (x - ys, y - xs), step, steps))
	if 0 <= x + ys < xb and 0 <= y - xs < yb and matrix[y - xs][x + ys] == 0:
		res.extend(traverse(matrix, size, (x + ys, y - xs), step, steps))
	return res

if __name__ == "__main__":
	size = list(map(lambda x: int(x), input().split()))
	step = list(map(lambda x: int(x), input().split()))
	w, h = size
	matrix = [[0] * w for _ in range(h)]
	for row in matrix:
		print(row)
	print(traverse(matrix, size, (0, 0), step))
	
	

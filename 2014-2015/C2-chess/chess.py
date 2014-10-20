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

def traverse(matrix, size, point, step, path = []):
	x, y = point
	w, l = size
	xs, ys = step
	res = []

	# Clone path. Faster than copy.copy()
	path = path[:]
	path.append(point)

	# Clone matrix. Faster than copy.deepcopy()
	matrix = [row[:] for row in matrix]
	matrix[y][x] = len(path)

	# Test if all points have been visited.
	if matrix[y][x] == w * l:
		return [(0, path)]
	# All path we can take, ignoring the boundaries of the matrix
	#	and the values of the points.
	points = set([
		(x - xs, y - ys),
		(x - ys, y - xs),

		(x + xs, y - ys),
		(x + ys, y - xs),

		(x - xs, y + ys),
		(x - ys, y + xs),
		
		(x + xs, y + ys),
		(x + ys, y + xs)	
	])
	# Remove points that lie beyond the boundaries of the matrix.
	points = [(m, n) for m, n in points if 0 <= m < w and 0 <= n < l]
	# Remove points that have already been visited.
	points = [(m, n) for m, n in points if matrix[n][m] == 0]
	# Check if there are still points available to visit.
	if not points:
		# Yep, we're fucked now.
		return [(w * l - matrix[y][x], path)]
	# Get the distance from the points to the boundaries.
	ds = [min(m, w - m - 1) + min(n, l - n - 1) for m, n in points]
	# Shortest distance from a point to the boundaries of the matrix found.
	minimum = min(ds)
	points = [points[i] for i, d in enumerate(ds) if d == minimum]
	ds = [min(m, w - m - 1, n, l - n - 1) for m, n in points]
	minimum = min(ds)
	points = [points[i] for i, d in enumerate(ds) if d == minimum]  
	# Visit the best points available.
	for p in points:
		res.extend(traverse(matrix, size, p, step, path))
	return res

if __name__ == "__main__":
	size, step = [list(map(lambda x: int(x), input().split())) for _ in range(2)]
	w, h = size
	matrix = [[0] * w for _ in range(h)]
	results = traverse(matrix, size, (0, 0), step)
	results = sorted(results, key=lambda k: k[0])
	if len(results) > 0:
		unvisited, path = results[0]
		print(len(path))
		for x, y in path:
			print("{0}{1}".format(chr(x + ord('a')), h - y))

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
	points = filter(lambda point: 0 <= point[0] < w and 0 <= point[1] < l, points) 
	# Remove points that have already been visited.
	points = filter(lambda point: matrix[point[1]][point[0]] == 0, points)
	points = list(points)
	# Check if there are still points available to visit.
	if not points:
		# Yep, you're fucked now.
		return [(w * l - matrix[y][x], path)]
	# Get the distance from the points to the boundaries.
	dp = map(lambda point: (min(min(point[0], w - point[0] - 1),  min(point[1], l - point[1] - 1)), point), points)
	dp = list(dp)
	# Shortest distance from a point to the boundaries of the matrix found.
	minimum = min(dp)[0]
	# The points which have the shortest distance to the edges.
	#points = [points[i] for i, m in enumerate(db) if m == minimum] 
	#dp = map(lambda point: (min(min(point[0], w - point[0] - 1), min(point[1], l - point[1] - 1)), point), points)
	dp = sorted(dp, key=lambda k: k[0])
	#minimum = min(dp)[0]
	#points = filter(lambda x: x[0] == minimum, dp)
	#points = map(lambda x: x[1], dp)
	# Visit the best points available.
	for d, p in dp:
		print(p)
		if d > minimum: break
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

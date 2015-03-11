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
	m, n = size
	dx, dy = step
	res = []

	# Clone path. Faster than copy.copy()
	path = path[:]
	path.append(point)

	# Clone matrix. Faster than copy.deepcopy()
	matrix = [row[:] for row in matrix]
	matrix[y][x] = len(path)

	# Test if all points have been visited.
	if matrix[y][x] == m * n:
		return [(0, path)]
	# All path we can take, ignoring the boundaries of the matrix
	#	and the values of the points.
	points = set([
		(x - dx, y - dy),
		(x - dy, y - dx),

		(x + dx, y - dy),
		(x + dy, y - dx),

		(x - dx, y + dy),
		(x - dy, y + dx),
		
		(x + dx, y + dy),
		(x + dy, y + dx)	
	])
	# Remove points that lie beyond the boundaries of the matrix.
	points = [(x, y) for x, y in points if 0 <= x < m and 0 <= y < n]
	# Remove points that have already been visited.
	points = [(x, y) for x, y in points if matrix[y][x] == 0]
	# Check if there are still points available to visit.
	if not points:
		# Yep, we're fucked now.
		return [(m * n - matrix[y][x], path)]
	# Refine the points we should visit.
	# Get the points with the shortest distance to the boundaries where
	#	distance is the distance to the m boundaries plus the 
	#	distance to the n boundaries:
	#
	#	d = min(x, m - x - 1) + min(y, n - y - 1)
	distances = [min(x, m - x - 1) + min(y, n - y - 1) for x, y in points]
	minimum = min(distances)
	points = [points[i] for i, d in enumerate(distances) if d == minimum]
	# Refine the points we should visit for a final time.
	# We should only visit the points with
	# 	the shortest distance to the boundaries where distance
	#	is either the distance to the x boundaries or the y boundaries:
	#
	#	d = min(x, m - x - 1, y, n - y - 1)
	distances = [min(x, m - x - 1, y, n - y - 1) for x, y in points]
	minimum = min(distances)
	points = [points[i] for i, d in enumerate(distances) if d == minimum]  
	# Try the points that are available and collect the results.
	for p in points:
		res += traverse(matrix, size, p, step, path)
	return res

if __name__ == "__main__":
	size, step = [list(map(lambda x: int(x), input().split())) for _ in range(2)]
	m, n = size
	matrix = [[0] * m for _ in range(n)]
	results = traverse(matrix, size, (0, 0), step)
	results = sorted(results, key=lambda k: k[0])
	if len(results) > 0:
		unvisited, path = results[0]
		print(len(path))
		for x, y in path:
			print("{0}{1}".format(chr(x + ord('a')), n - y))

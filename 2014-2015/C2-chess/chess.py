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
	x, y = point
	xb, yb = size
	xs, ys = step
	res = []

	matrix = copy.deepcopy(matrix)
	matrix[y][x] += 1

	steps = copy.deepcopy(steps)
	steps.append(point)

	print("point={0}, steps={1}".format(point, len(steps)))
	for row in matrix:
		print(row)
	time.sleep(0.05)

	# Get all unvisited points in the matrix.
	test = [x for y in matrix for x in filter(lambda x: x == 0, y)]
	if not len(test):
		# Return if all points in the matrix have been visited.
		return [(True, steps)]
	# All path we can take, ignoring the boundaries of the matrix
	#	and the values of the points.
	points = [
		(x - xs, y - ys),
		(x + xs, y - ys),
		(x - xs, y + ys),
		(x + xs, y + ys),
		(x - ys, y - xs),
		(x + ys, y - xs),
		(x - ys, y + xs),
		(x + ys, y + xs)
	]
	# Remove points that lie beyond the boundaries of the matrix.
	points = filter(lambda point: 0 <= point[0] < xb and 0 <= point[1] < yb, points)
	# Remove points that have already been visited.
	points = filter(lambda point: matrix[point[1]][point[0]] == 0, points)
	points = list(points)
	# Check if there are still points available to visit.
	if not points:
		# Yep, we're fucked now.
		# You'll enter a infinite loop now.
		return [(False, steps)]
	# Get the distance from the points to the boundaries.
	db = map(lambda point: min([point[0], xb - point[0]]) + min([point[1], yb - point[1]]), points)
	db = list(db)
	# Shortest distance from a point to the boundaries of the matrix found.
	minimum = min(db)
	# Try all points that share the minimum.
	while minimum in db:
		index = db.index(minimum)
		best = points[index]
		res.extend(traverse(matrix, size, best, step, steps))
		db.pop(index)
	return res 

if __name__ == "__main__":
	size = list(map(lambda x: int(x), input().split()))
	step = list(map(lambda x: int(x), input().split()))
	w, h = size
	matrix = [[0] * w for _ in range(h)]
	for row in matrix:
		print(row)
	print(traverse(matrix, size, (0, 0), step))
	
	

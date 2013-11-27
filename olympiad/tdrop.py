#!/usr/bin/env python
# -*- coding: utf-8 -*- 

#   Copyright 2013 Fabian M.
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
#

import sys

# This module contains an implementation of a Tetris-like game called T-Drop.

# Example of the usage of this module:
#
#	import tdrop
#	# create board
#	board = tdrop.Board(6, 9) # width, height
#	# create a block.
#	block = tdrop.TBlock(1, 1, 1) # x, y, direction
#	board.drop(tdrop)
#

# This module is made as a solution to problem 2 of the Dutch 
# Olympiad in Informatics. 
#
# See http://www.informaticaolympiade.nl for more information.

class Board(object):
	"""
	The Board class represents a grid which contains blocks.
	"""
	def __init__(self, width = 6, height = 9):
		"""
		Initializes a new Board instance.
		
		Keyword arguments:
		width -- the width of the grid (default = 6)
		height -- the height of the grid (default = 9)
		"""
		self.width = width
		self.height = height
		self.blocks = []
		
	def is_valid(self, block):
		for rectangle in block.points:
			x, y = rectangle
			if x < 0 or x >= self.width or y < 0 or y >= self.height:
				return False
		for other_block in self.blocks:
			if other_block != block and block.conflict(other_block):
				return False
		return True
		
	def add_block(self, block):
		if not self.is_valid(block):
			return False
		self.blocks.append(block)
		return True
		
	def drop(self, block):
		while True:
			block.y -= 1
			if not self.is_valid(block):
				block.y += 1
				if not block in self.blocks:
					self.blocks.append(block)
				break
	
	def __str__(self):
		"""
		Returns a string representation of this Board.
		"""
		string = "-" * self.width * 2
		string += "\n"
		for line in reversed(range(0, self.height)):
			string += "|"
			for column in range(0, self.width):
				if not self.blocks:
					string += " "
				else:
					empty = True
					for block in self.blocks:
						if block.uses(column, line):
							string += "#"
							empty = False
							break
					if empty:
						string += " "
				string += "|"
			string += "\n"
			string += "-" * self.width * 2
			string += "\n"
		return string
		
class Block(object):
	def __init__(self, x, y):
		"""
		Initializes a new Block object.
		"""
		self._x = x
		self._y = y
		self.points = [(x, y)]	
		
	def calc_max(board):
		"""
		Calculates the max amount of this block in the given board.
		"""
		return board.width * board.height
		
		
	def uses(self, x, y = 0):
		for rectangle in self.points:
			if rectangle[0] == x and rectangle[1] == y:
				return True
		return False
		
	def conflict(self, block):
		for rectangle in self.points:
			x, y = rectangle
			if block.uses(x, y):
				return True
		return False
		
	@property
	def x(self):
		return self._x
	
	@property	
	def y(self):
		return self._y
		
	@x.setter
	def x(self, x):
		self._x = x
		deltaX = x - self.points[0][0]
		for index, rectangle in enumerate(self.points):
			self.points[index] = (rectangle[0] + deltaX, rectangle[1])
		
	@y.setter
	def y(self, y):
		self._y = y
		deltaY = y - self.points[0][1]
		for index, rectangle in enumerate(self.points):
			self.points[index] = (rectangle[0], rectangle[1] + deltaY)
			
	def __str__(self):
		return str(self.points)
			
class TBlock(Block):
	def __init__(self, x, y, direction):
		"""
		Initializes a new TBlock object.
		"""
		Block.__init__(self, x, y)
		self.points = [(x, y), (x + 1, y), (x + 2, y), (x + 1, y + 1)]
		self.direction = direction
		
	@property
	def direction(self):
		return self._direction
		
	@direction.setter
	def direction(self, direction):
		self._direction = direction
		x, y = self.points[0]
		if direction == 1:
			self.points = [(x, y), (x + 1, y), (x + 2, y), (x + 1, y + 1)]
		elif direction == 2:
			self.points = [(x, y), (x, y + 1), (x, y + 2), (x + 1, y + 1)]
		elif direction == 3:
			self.points = [(x, y), (x + 1, y), (x + 2, y), (x + 1, y - 1)]
		elif direction == 4:
			self.points = [(x, y), (x + 1, y + 1), (x + 1, y - 1), (x + 1, y)]
	
def a():
	input = open('in.txt')
	width = int(input.readline())
	height = int(input.readline()) + 1
	board = Board(width, height + 5)
	
	for line in input:
		x = ord(line[0].lower()) - 97
		direction = int(line[1])
		block = TBlock(x, height, direction)
		if not board.add_block(block):
			#print(board)
			#raise Exception("What? Block is not valid at spawn? Block: {0}".format(block)))
			# Suppress this warning, just continue. 
			continue
		while True:
			block.y -= 1
			if not board.is_valid(block):
				block.y += 1
				break
	output = open('out.txt', "w")
	for block in board.blocks:
		output.write("{0}\n".format(block.y + 1))
		
def calc_max(width, height):
	"""
	Calculates the maximum amount of this block in the given board.
	"""
	if width < 2 or height < 2:
		return 0
	elif width * height < 10:
		return 1
	elif width  % 4 == height % 4 == 0:
		return int(width * height / 4)
	elif width  % 4 == height % 4 == 2 or width % 4 == 0 or height % 4 == 0:
		return int((width * height - 4) / 4)
	elif width % 4 == 0 and height % 4 == 2:
		return int((width * height - (height % 4 + 2)) / 4)
	elif width % 4 == 2 and height % 4 == 0:
		return int((width * height - (width % 4 + 2)) / 4)
	elif width == 3 and height != 3:
		if height % 3 == 0:
			return int((width * height - (height / 3 + 1)) / 4) + 1
		elif height % 3 == 1:
			return int((width * height - (((height + 2) / 3) + 1)) / 4)
		elif height % 3 == 2:
			return int((width * height - (((height + 1) / 3) + 1)) / 4)
	elif height == 3 and width != 3:
		if width % 3 == 0:
			return int((width * height - (width / 3 + 1)) / 4) + 1
		elif width % 3 == 1:
			return int((width * height - (((width + 2) / 3) + 1)) / 4)
		elif width % 3 == 2:
			return int((width * height - (((width + 1) / 3) + 1)) / 4)
	elif width == 5 or height == 5:
		if width * height % 2 == 0 and width * height % 4 != 0:
			return int((width * height - 2) / 4)
		elif width * height % 3 == 0:
			return int((width * height - 3) / 4)
		elif width * height % 4 == 0:
			return int((width * height - 4) / 4)
		elif width * height % 5 == 0:
			return int((width * height - 5) / 4)
	elif width == 6 and height != 6:
		if height % 6 == 0:
			return int((width * height - 4) / 4)
		elif height % 3 == 0:
			return int((width * height - 2) / 4)
		elif (height + 1) % 6 == 0:
			return int((width * height - 6) / 4)
		elif (height + 1) % 3 == 0:
			return int((width * height - 4)	/ 4)
	elif height == 6 and width != 6:
		if width % 6 == 0:
			return int((width * height - 4) / 4)
		elif width % 3 == 0:
			return int((width * height - 2) / 4)
		elif (width + 1) % 6 == 0:
			return int((width * height - 6) / 4)
		elif (width + 1) % 3 == 0:
			return int((width * height - 4) / 4)
	elif width == 7 and height != 7:
		if height % 4 == 0:
			return int((width * height - 4) / 4)
		elif height % 4 == 1:
			return int((width * height - 3) / 4)
		elif height % 4 == 2:
			return int((width * height - 2) / 4)
		elif height % 4 == 3:
			return int((width * height - 5) / 4)
	elif height == 7 and width != 7:
		if width % 4 == 0:
			return int((width * height - 4) / 4)
		elif width % 4 == 1:
			return int((width * height - 3) / 4)
		elif width % 4 == 2:
			return int((width * height - 2) / 4)
		elif width % 4 == 3:
			return int((width * height - 5) / 4)
			
def b():
	width = int(input())
	height = int(input())
	print(calc_max(width, height))

if __name__ == "__main__":
	if len(sys.argv) > 1 and sys.argv[1] == "b":
		b()
	else:
		a()

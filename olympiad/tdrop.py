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
#	board = tdrop.Board(6, 9) # width, length
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
	def __init__(self, width = 6, length = 9):
		"""
		Initializes a new Board instance.
		
		Keyword arguments:
		width -- the width of the grid (default = 6)
		length -- the length of the grid (default = 9)
		"""
		self.width = width
		self.length = length
		self.blocks = []
		
	def is_valid(self, block):
		for rectangle in block.points:
			x, y = rectangle
			if x < 0 or x >= self.width or y < 0 or y >= self.length:
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
		for line in reversed(range(0, self.length)):
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
		return board.width * board.length
		
		
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
			
def calc_max(length, width):
	"""
	Calculates the maximum amount of T-shaped objects in a rectangle of the
	  given size.

	Keyword arguments:
	length -- the length of the rectangle.
	width -- the width of the rectangle.
	
	Returns the maximum amount of T-shaped objects in a rectangle of the given
	  size.
	"""
	surface = length * width
	if length < 2 or width < 2 or surface < 6:
		return 0
	elif surface < 10:
		return 1
	elif length  % 4 == width % 4 == 0:
		return int(surface / 4)
	elif length  % 4 == width % 4 == 2 or length % 4 == 0 or width % 4 == 0:
		return int((surface - 4) / 4)
	elif length % 4 == 0 and width  % 4 == 2:
		return int((surface - (length % 4 + 2)) / 4)
	elif length % 4 == 2 and width % 4 == 0:
		return int((surface - (width % 4 + 2)) / 4)
	elif length == 3 or width == 3:
		return length == 3 and calc_max_three(width) or calc_max_three(length)
	elif length == 5 or width == 5:
		return length == 5 and calc_max_five(width) or calc_max_five(length)
	elif length == 6 or width == 6:
		return length == 6 and calc_max_six(width) or calc_max_six(length)
	elif length == 7 or width == 7:
		return length == 7 and calc_max_seven(width) or calc_max_seven(length)

def calc_max_three(length):
	"""
	Assumes that the width equals three and calculates the maximum amount of 
	  T-shaped objects in a rectangle with size __3x__.

	Keyword arguments:
	length -- the length of the rectangle.

	Returns the maximum amount of T-shaped objects in a rectangle with the
	  size __3x__
	"""
	surface = 3 * length
	if length == 3:
		return 1
	elif length % 3 == 0:
		return int((surface - (length / 3 + 1)) / 4) + 1
	elif length % 3 == 1:
		return int((surface - (((length + 2) / 3) + 1)) / 4)
	elif length % 3 == 2:
		return int((surface - (((length + 1) / 3) + 1)) / 4)

def calc_max_five(length):
	"""
	Assumes that the width equals five and calculates the maximum amount of
	  T-shaped objects in a rectangle with size __5x__.

	Keyword arguments:
	length -- the length of the rectangle

	Returns the maximum amount of T-shaped objects in a rectangle with the 
	  size __5x__
	"""
	surface = 5 * length
	if surface % 2 == 0 != surface % 4:
		return int((surface - 2) / 4)
	elif surface % 3 == 0:
		return int((surface - 3) / 4)
	elif surface % 4 == 0:
		return int((surface - 4) / 4)
	elif surface % 5 == 0:
		return int((surface - 5) / 4)

def calc_max_six(length):
	"""
	Assumes that the width equals six and calculates the maximum amount of
	  T-shaped objects in a rectangle with size __6x__.

	Keyword arguments:
	l -- the length of the rectangle

	Returns the maximum amount of T-shaped objects in a rectangle with the 
	  size __6x__
	"""
	surface = 6 * length
	if length % 6 == 0:
		return int((surface - 4) / 4)
	elif length % 3 == 0:
		return int((surface - 2) / 4)
	elif (length + 1) % 6 == 0:
		return int((surface - 6) / 4)
	elif (length + 1) % 3 == 0:
		return int((surface - 4) / 4)

def calc_max_seven(length):
	"""
	Assumes that the width equals seven and calculates the maximum amount of
	  T-shaped objects in a rectangle with size __7x__.

	Keyword arguments:
	length -- the length of the rectangle

	Returns the maximum amount of T-shaped objects in a rectangle with the 
	  size __7x__
	"""
	surface = 7 * length
	if length % 4 == 0:
		return int((surface - 4) / 4)
	elif length % 4 == 1:
		return int((surface - 3) / 4)
	elif length % 4 == 2:
		return int((surface - 2) / 4)
	elif length % 4 == 3:
		return int((surface - 5) / 4)

if __name__ == "__main__":
	"""
	Main entry point for this program.
	"""
	if len(sys.argv) <= 1: 
		input = open('in.txt')
		width = int(input.readline())
		length = int(input.readline()) + 1
		board = Board(width, length + 5)
	
		for line in input:
			x = ord(line[0].lower()) - 97
			direction = int(line[1])
			block = TBlock(x, length, direction)
			if not board.add_block(block):
				continue
			while True:
				block.y -= 1
				if not board.is_valid(block):
					block.y += 1
					break
		output = open('out.txt', "w")
		for block in board.blocks:
			output.write("{0}\n".format(block.y + 1))
	elif sys.argv[1] == "b":
		width = int(input())
		length = int(input())
		print(calc_max(length, width))

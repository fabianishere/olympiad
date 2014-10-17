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

# This module is made as a solution for problem 2 of the Dutch 
# Olympiad in Informatics 2013-2014.
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

if __name__ == "__main__":
	"""
	Main entry point for this program.
	"""
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
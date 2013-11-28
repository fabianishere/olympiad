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
import unittest
import olympiad.tdrop

class TestTDrop(unittest.TestCase):
	"""
	Test case for the TDrop module.
	"""
	def setUp(self):
		self.results = [
			[0, 0, 0, 0, 0, 0, 0, 0],
			[0, 0, 0, 0, 0, 0, 0, 0],
			[0, 0, 0, 1, 1, 2, 2, 3, 3],
			[0, 0, 1, 1, 2, 3, 4, 4, 5],
			[0, 0, 1, 2, 4, 4, 5, 6, 8],
			[0, 0, 2, 3, 4, 5, 7, 8, 9],
			[0, 0, 2, 4, 5, 7, 8, 10, 11],
			[0, 0, 3, 4, 6, 8, 9, 11, 13],
			[0, 0, 3, 5, 8, 9, 11, 12, 16],
			[0, 0, 4, 6, 8, 10, 13, 15, 17],
			[0, 0, 4, 6, 9, 11, 14, 17, 19],
			[0, 0, 5, 7, 10, 12],
			[0, 0, 5, 8, 12, 15],
			[0, 0, 6, 8, 12],
			[0, 0, 6, 9, 13],
			[0, 0, 7, 10, 14],
			[0, 0, 7, 10, 16],
			[0, 0, 8, 11, 16],
			[0, 0, 8, 12, 17],
			[0, 0, 9, 12, 18],
			[0, 0, 9, 13, 20],
			[0, 0, 10, 14, 20],
			[0, 0, 10, 14, 21],
			[0, 0, 11, 15, 22],
			[0, 0, 11, 16, 24],
			[0, 0, 12, 16, 24],
			[0, 0, 12, 17, 25],
			[0, 0, 13, 18, 26],
			[0, 0, 13, 18, 28],
			[0, 0, 14, 19, 28],
			[0, 0, 14, 20, 29]
		]
	
	def test_calc_max_default(self):
		for length, widths in enumerate(self.results):
			for width, result in enumerate(widths):
				print("{0} must equal {1} ({2}x{3})".format(olympiad.tdrop.calc_max(length, width), result, length, width))
				self.assertEqual(olympiad.tdrop.calc_max(length, width), result) 
			
if __name__ == "__main__":
	unittest.main()
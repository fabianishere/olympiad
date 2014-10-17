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
import unittest
import num2word

class TestDutchConverter(unittest.TestCase):
	"""
	Test case for the Dutch number to word converter.
	"""
	def setUp(self):
		self.numbers = {
			0 : "nul",
			10 : "tien",
			13 : "dertien",
			83 : "drie&ntachtig",
			3943 : "drieduizend negenhonderddrie&nveertig",
			9999 : "negenduizend negenhonderdnegenennegentig",
			193920 : "honderddrie&nnegentigduizend negenhonderdtwintig",
			134 : "honderdvierendertig",
			32 : "twee&ndertig"
		}
		self.converter = olympiad.num2word.DutchConverter()
	
	def test_convert(self):
		for number in self.numbers:
			print("Number {0} must equal {1}".format(number, self.numbers[number]))
			self.assertEqual(self.converter.convert(number), self.numbers[number])
			
if __name__ == "__main__":
	unittest.main()
#!/usr/bin/env python3
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

import abc

# This module contains an abstract number to word converter and an 
# implementation of the converter for the Dutch language.
#
# Please note that this module requires the abc module in order to run.

# Example of the usage of this module:
#
#	import num2word
#	# select the language you want to convert the number to. (I chose Dutch)
#	converter = num2word.DutchConverter()
#	print(converter.convert(10)) # prints "tien"
#

# This module is made as a solution to problem 1 of the Dutch 
# Olympiad if Informatics. 
#
# See http://www.informaticaolympiade.nl for more information.

class BaseConverter(object):
	"""Base class of a number to word converter.
	"""
	__metaclass__ = abc.ABCMeta
	
	@abc.abstractproperty
	def name(self):
		"""
		Returns the name of the language this converter converts a number to.
	
		For example, the Dutch implementation of this class would return a 
		string containing "dutch".
		"""
		pass
		
	@abc.abstractproperty
	def code(self):
		"""
		Returns the language code (ISO 639-1) of the language this converter 
		converts a number to.
	
		For example, the Dutch implementation of this class would return a 
		string containing "nl".
		"""
		pass
		
	def convert(self, number):
		"""
		Converts a number to a word.
	
		Keyword arguments:
		number -- the number to convert to a word. This number could be an
		integer, long, float or a numeric string.
	
		Example:
	
			DutchConverter().convert_number("10") => "tien"
			
		Raises a TypeError when an invalid argument is given.
		"""
		pass
		
class DutchConverter(BaseConverter):
	"""Implementation of the BaseConverter class for the Dutch language.
	"""
	def __init__(self):
		"""Intializes this class.
		"""
		self.units = [
			"nul", "een", "twee", "drie", "vier", "vijf", "zes", "zeven",
			"acht", "negen"
		]
		self.tens = [
			"", "tien", "twintig", "dertig", 
			"veertig", "vijfig","zestig", 
		    "zeventig", "tachtig", "negentig"
		]
		self.high = ["honderd", "duizend", "miljoen", "miljard"]
		self.exceptions = [
			"tien", "elf", "twaalf", "dertien", "veertien", "vijftien",
			"zestien", "zeventien", "achtien", "negentien"
		]
	
	@property
	def name(self):
		"""
		Returns the name of the language this converter converts a number to.
	
		For example, the Dutch implementation of this class would return a 
		string containing "dutch".
		"""
		return "dutch"
		
	@property
	def code(self):
		"""
		Returns the language code (ISO 639-1) of the language this converter 
		converts a number to.
	
		For example, the Dutch implementation of this class would return a 
		string containing "nl".
		"""
		return "nl"
		
	def convert(self, number):
		"""
		Converts a number to a word.
	
		Keyword arguments:
		number -- the number to convert to a word. This number could be an
		integer, long, float or a numeric string.
	
		Example:
	
			DutchConverter().convert_number("10") => "tien"
			
		Raises a TypeError when an invalid argument is given.
		"""
		string = number
		if not isinstance(number, str):
			string = str(number)
		
		# This implementation of a number to word converter reads the number as
		# a string from back to front.
		#
		# The index is set to zero, since the number string is reversed.
		index = 0
		
		# The length of the number to check if there are more characters
		# coming.
		length = len(string)
		
		result = ""
		# Return if the length of the string is lower than one.
		if length < 1:
			return "nul"
			
		# Reverse the string so we can read from the back to the font.
		reverse_string = string[::-1]
		
		# The character of a reversed numeric string must be a unit.
		# This means the word that represents the number is looked up and is
		# added to the result.
		#
		# Example:
		#	
		#	3 (self.units[3]) => drie
		#
		# One exception, in Dutch, when we have the number 29, instead of saying
		# "twintig negen" (which means twenty nine), we say "negentwintig".
		# This means the length of the number string should be checked, so we
		# can see if this character is followed by another.
		#
		# So:
		#
		# 	29 => negenentwintig
		# 
		# But:
		#
		#	22 => tweeëntwintig
		#	23 => drieëntwintig
		#
		# Be aware that we will return "twee&ntwintig" instead of 
		# "tweeëntwintig" due to encoding issues.
		#
		#
		# This if construction basically means that if the integer value of the
		# first character is zero and the length of the number string is one,
		# or the value of the first character is not zero, then the result will
		# be set to the appropriate unit.
		#
		# This means:
		#
		#	0 => "nul"
		#   1 => "een"
		# 
		# But:
		#
		#   10 => "ten"
		# 
		if (int(reverse_string[index]) == 0 and length == 1) or int(reverse_string[index]) != 0:
			result = self.units[int(reverse_string[index])]
			
		# Jump to next number or return the result if there isn't a next number.
		index += 1
		if index >= length:
			return result
		
		# Exception: 11 to 19 are written as "elf", "twaalf", "dertien", etc.
		# This if construction will check if the integer value of the current
		# character equals one. If so, use words from the exception list.
		if int(reverse_string[index]) == 1:
			result = self.exceptions[int(reverse_string[index - 1])]
		elif int(reverse_string[index]) != 0:
			if int(reverse_string[index - 1]) != 0:
				# As already mentioned, "drie" comes in front of "twintig" when 
				# the number is 23.
				result = self.units[int(reverse_string[index - 1])]
				# Add "&n" when the first character is 2 or 3, otherwise "en".
				if int(reverse_string[index - 1]) in [2, 3]:  
					result += "&n"
				else:
					result += "en"
			# Add "twintig" when the numer is 22.
			result += self.tens[int(reverse_string[index])]
		
		# Jump to next number or return the result if there isn't a next number.
		index += 1
		if index >= length:
			return result
		
		# Exception: don't add a unit in front of "honderd" when the integer
		# value of the current character equals one.
		# If the integer value of the current character equals one, then 
		# the character should be ignored.
		#
		# Example:
		#
		#	100 => "honderd" not 100 => "eenhonderd"
		#   200 => "tweehonderd"
		#   1100 => "duizend honderd"
		#   1200 => "duizend tweehonderd"
		#   1000 => "duizend" not 1000 => "duizend nulhonderd"
		#
		if int(reverse_string[index]) == 1:
			result = self.high[0] + result
		elif int(reverse_string[index]) != 0:
			result = self.units[int(reverse_string[index])] + self.high[0] + result
		
		# Jump to next number or return the result if there isn't a next number.
		index += 1
		if index >= length:
			return result
		
		append = ""
		# Exception: don't add a unit in front of "duizend" when the integer
		# value of the current character equals one.
		# If the integer value of the current character equals one, then 
		# the character should be ignored.
		#
		# Example:
		#
		#	1000 => "duizend" not 1000 => "eenduizend"
		#   2000 => "tweeduizend"
		#   1001000 => "een miljoen duizend"
		#   1200000 => "een miljoen tweehonderdduizend"
		#   1000000 => "een miljoen"
		#
		if int(reverse_string[index]) == 1:
			append = self.high[1]
		elif int(reverse_string[index + 2:index - 1:-1]) != 0:
			append = self.convert(int(reverse_string[index + 2:index - 1:-1])) + self.high[1]
		if result:
			result = " " + result
		result = append + result
			
		# Increase the index by three since we already covered those characters.
 	    # If there aren't any more characters, return the result. 
		index += 3
		if index >= length:
			return result

		for count in range(2, 4):
			append = ""
			# Exception: add spaces between both sides of the "miljoen" 
			# or "miljard" word, but only if words follow, otherwise not.
			#
			# If the integer value of the current character equals one, then 
			# the character should be ignored.
			#
			# Example:
			#
			#	1000000 => "een miljoen" not 1000000 => "miljoen"
			#   10000000 => "tien miljoen"
			#   1002000 => "een miljoen tweeduizend"
			#	1000000000 => "een miljard" not 1000000000 => "miljard"
			#   10000000000 => "tien miljard"
			#   10020000000 => "tien miljard tweehonderd miljoen"
			#
			if int(reverse_string[index + 2:index - 1:-1]) != 0:
				append = self.convert(int(reverse_string[index + 2:index - 1:-1])) + " " + self.high[count]
				if result: 
					result = " " + result
				result = append + result
		
			# Increase the index by three since we already covered 
			# those characters.
 	    	# If there aren't any more characters, return the result.		
			index += 3
			if index >= length:
				return result

		return result

if __name__ == "__main__":
	"""
	Maint entry point for this program.
	"""
	converter = DutchConverter()
	while True:
		print(converter.convert(raw_input()))

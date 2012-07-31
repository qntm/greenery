# Copyright (C) 2010 by Sam Hughes

# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:

# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.

# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.

# http://qntm.org/lego
# http://qntm.org/greenery

import copy

escapes = {
	"\t" : "\\t", # tab
	"\n" : "\\n", # line feed
	"\v" : "\\v", # vertical tab
	"\f" : "\\f", # form feed
	"\r" : "\\r", # carriage return
}

# these are the characters carrying special meanings when they appear "outdoors"
# within a regular expression. To be interpreted literally, they must be
# escaped with a backslash.
allSpecial = set("\\[]|().?*+{}")

# these are the characters carrying special meanings when they appear INSIDE a
# character class (delimited by square brackets) within a regular expression.
# To be interpreted literally, they must be escaped with a backslash.
# Notice how much smaller this class is than the one above; note also that the
# hyphen does NOT appear above.
classSpecial = set("\\[]^-")

# these are the character ranges which can be used inside square brackets e.g.
# "[a-z]", "[F-J]". These ranges should be disjoint, and sorted in order.
allowableRanges = {
	"ABCDEFGHIJKLMNOPQRSTUVWXYZ",
	"abcdefghijklmnopqrstuvwxyz",
	"0123456789",
}

class ParseFailureException(Exception):
	pass

class lego:
	''' Parent class for all lego pieces '''

	# It causes some pretty severe potential problems when
	# these things are mutable
	def __setattr__(self, name, value):
		raise Exception("Can't set " + str(self) + " attribute " + str(name) + " to " + str(value))

	@classmethod
	def parse(cls, string):
		'''Parse a full string, fail if the whole string wasn't parsed'''
		result, i = cls.match(string, 0)
		if i != len(string):
			raise ParseFailureException("Could not parse '" + string + "' beyond index " + str(i))
		return result

	@classmethod
	def parseStatic(cls, string, i, static):
		if string[i:len(static)+i] == static:
			return i+len(static)
		raise ParseFailureException(
			"Can't find '" + static + "' at index " + str(i) + " in '" + string + "'"
		)

	@classmethod
	def parseAny(cls, string, i, collection=None):
		if collection is None:
			if i >= len(string):
				raise ParseFailureException
			return string[i], i+1
		else:
			for char in collection:
				try:
					return char, cls.parseStatic(string, i, char)
				except ParseFailureException:
					pass
			raise ParseFailureException("Can't find any of '" + str(collection) + "' at index " + str(i) + " in '" + string + "'")

class charclass(lego):
	'''
		A charclass is basically a frozenset of symbols. The reason for the
		charclass object instead of using frozenset directly is to allow us to
		set a "negated" flag. A charclass with the negation flag set is assumed
		to contain every symbol that is in the alphabet of all symbols but not
		explicitly listed inside the frozenset. e.g. [^a]. This is very handy
		if the full alphabet is extremely large, but also requires dedicated
		combination functions like classunion and classissubset.

		Important note: while symbols are characters in almost every example and
		in unit tests, a symbol can be any hashable object.
		However, it's only practical to print a charclass
		out if its symbols are all individual characters.
	'''

	def __init__(self, chars=set(), negateMe=False):
		self.__dict__["chars"]   = frozenset(chars)
		self.__dict__["negated"] = negateMe

	def __eq__(self, other):
		return type(other) == type(self) and \
		self.chars == other.chars and \
		self.negated == other.negated

	def __hash__(self):
		return tuple.__hash__((self.chars, self.negated))

	def __mul__(self, multiplier):
		if multiplier == one:
			return self
		return mult(self, multiplier)

	def regex(self):
		'''Render this charclass in string format'''

		# e.g. \w
		if self in shorthand.keys():
			return shorthand[self]

		# e.g. [^a]
		if self.negated:
			return "[^" + self.escape() + "]"

		# single character, not contained inside square brackets.
		if len(self.chars) == 1:
			# Python lacks the Axiom of Choice
			char = "".join(self.chars)

			# e.g. if char is "\t", return "\\t"
			if char in escapes.keys():
				return escapes[char]

			if char in allSpecial:
				return "\\" + char

			return char

		# multiple characters
		return "[" + self.escape() + "]"

	def escape(self):

		def escapeChar(char):
			if char in classSpecial:
				return "\\" + char
			if char in escapes.keys():
				return escapes[char]
			return char

		def recordRange():
			nonlocal currentRange
			nonlocal output

			# there's no point in putting a range when the whole thing is
			# 3 characters or fewer.
			if len(currentRange) in {0, 1, 2, 3}:
				output += "".join(escapeChar(char) for char in currentRange)
			else:
				output += escapeChar(currentRange[0]) + "-" + \
				escapeChar(currentRange[-1])

			currentRange = ""

		output = ""

		# use shorthand for known character ranges
		# note the nested processing order. DO NOT process \d before processing
		# \w. if more character class constants arise which do not nest nicely,
		# a problem will arise because there is no clear ordering to use...

		# look for ranges
		currentRange = ""
		for char in sorted(self.chars):

			# range is not empty: new char must fit after previous one
			if len(currentRange) > 0:

				# find out if this character appears in any of the
				# allowableRanges listed above.
				superRange = None
				for allowableRange in allowableRanges:
					if char in allowableRange:
						superRange = allowableRange
						break

				if superRange is None:
					# if this character doesn't appear above, then any existing
					# currentRange should be sorted and filed now
					# if there is one
					recordRange()

				else:
					i = superRange.i(char)

					# char doesn't fit old range: restart
					if i == 0 or superRange[i-1] != currentRange[-1]:
						recordRange()

			currentRange += char

		recordRange()

		return output

	def __repr__(self):
		string = "charclass("
		stuff = []
		if len(self.chars) > 0:
			stuff.append("".join(str(char) for char in sorted(self.chars, key=str)))
		if self.negated:
			stuff.append("negateMe=True")
		string += ", ".join(stuff)
		string += ")"
		return string

	def reduce(self):
		# Charclasses cannot be reduced()
		return self

	def __add__(self, other):
		'''Concatenation function'''
		return mult(self, one) + other

	@classmethod
	def match(cls, string, i):
		'''Parse and return a new charclass object starting at the supplied
		index in the supplied string, or throw an exception on failure'''

		# wildcard ".", "\\w", "\\d", etc.
		for key in shorthand.keys():
			try:
				return key, cls.parseStatic(string, i, shorthand[key])
			except ParseFailureException:
				pass

		# "[^dsgsdg]"
		try:
			return cls.parseNegatedRange(string, i)
		except ParseFailureException:
			pass

		# "[sdfsf]"
		try:
			return cls.parseRange(string, i)
		except ParseFailureException:
			pass

		# e.g. if seeing "\\t", return "\t"
		for key in escapes.keys():
			try:
				return charclass(key), cls.parseStatic(string, i, escapes[key])
			except ParseFailureException:
				pass

		# e.g. if seeing "\\{", return "{"
		for char in allSpecial:
			try:
				return charclass(char), cls.parseStatic(string, i, "\\" + char)
			except ParseFailureException:
				pass

		# single non-special character, not contained inside square brackets
		char, i = cls.parseAny(string, i)
		if char in allSpecial:
			raise ParseFailureException

		return charclass(char), i

	@classmethod
	def parseNegatedRange(cls, string, i):
		i = cls.parseStatic(string, i, "[^")
		chars, i = cls.parseRangeInterior(string, i)
		i = cls.parseStatic(string, i, "]")
		return charclass(chars, negateMe=True), i

	@classmethod
	def parseRange(cls, string, i):
		i = cls.parseStatic(string, i, "[")
		chars, i = cls.parseRangeInterior(string, i)
		i = cls.parseStatic(string, i, "]")
		return charclass(chars), i

	@classmethod
	def parseRangeInterior(cls, string, i):
		internals = ""
		try:
			while True:
				internal, i = cls.parseRangeInternal(string, i)
				internals += internal
		except ParseFailureException:
			pass
		return internals, i

	@classmethod
	def parseRangeInternal(cls, string, i):
		firstChar, i = cls.parseInternalChar(string, i)
		try:
			j = cls.parseStatic(string, i, "-")
			lastChar, j = cls.parseInternalChar(string, j)

			charRange = None
			for allowableRange in allowableRanges:
				if firstChar in allowableRange:
					# first and last must be in the same character range
					if lastChar not in allowableRange:
						raise ParseFailureException("Char '" + lastChar + "' not allowed as end of range")

					firstIndex = allowableRange.index(firstChar)
					lastIndex = allowableRange.index(lastChar)

					# and in order i.e. a < b
					if firstIndex >= lastIndex:
						raise ParseFailureException(
							"Disordered range ('" + firstChar + "' !< '" + lastChar + "')"
						)

					# OK
					return allowableRange[firstIndex:lastIndex + 1], j

			raise ParseFailureException("Char '" + firstChar + "' not allowed as start of a range")
		except ParseFailureException:
			return firstChar, i

	@classmethod
	def parseInternalChar(cls, string, i):

		# e.g. if we see "\\t", return "\t"
		for key in escapes.keys():
			try:
				return key, cls.parseStatic(string, i, escapes[key])
			except ParseFailureException:
				pass

		# special chars e.g. "\\-" returns "-"
		for char in classSpecial:
			try:
				return char, cls.parseStatic(string, i, "\\" + char)
			except ParseFailureException:
				pass

		# single non-special character, not contained
		# inside square brackets
		char, j = cls.parseAny(string, i)
		if char in classSpecial:
			raise ParseFailureException

		return char, j

	# self output methods:

	def escape(self):

		def escapeChar(char):
			if char in classSpecial:
				return "\\" + char
			if char in escapes.keys():
				return escapes[char]
			return char

		def recordRange():
			nonlocal currentRange
			nonlocal output

			# there's no point in putting a range when the whole thing is
			# 3 characters or fewer.
			if len(currentRange) in {0, 1, 2, 3}:
				output += "".join(escapeChar(char) for char in currentRange)
			else :
				output += escapeChar(currentRange[0]) + "-" + \
				escapeChar(currentRange[-1])

			currentRange = ""

		output = ""

		# use shorthand for known character ranges
		# note the nested processing order. DO NOT process \d before processing
		# \w. if more character class constants arise which do not nest nicely,
		# a problem will arise because there is no clear ordering to use...

		# look for ranges
		currentRange = ""
		for char in sorted(self.chars):

			# range is not empty: new char must fit after previous one
			if len(currentRange) > 0:

				# find out if this character appears in any of the
				# allowableRanges listed above.
				superRange = None
				for allowableRange in allowableRanges:
					if char in allowableRange:
						superRange = allowableRange
						break

				if superRange is None:
					# if this character doesn't appear above, then any existing
					# currentRange should be sorted and filed now
					# if there is one
					recordRange()

				else:
					i = superRange.index(char)

					# char doesn't fit old range: restart
					if i == 0 or superRange[i-1] != currentRange[-1]:
						recordRange()

			currentRange += char

		recordRange()

		return output

	# set operations

	def negate(self):
		'''Replace a charclass with its negation'''
		return charclass(self.chars, negateMe=not self.negated)

	def union(self, other):
		'''Find the classunion of charclasses, returning a charclass.'''

		# two regular classes unify in the regular sense.
		if not self.negated and not other.negated:
			return charclass(self.chars.union(other.chars))

		# classunioning a charclass and a negated one is a little more complex
		# A OR ¬B = ¬(B - A)
		if not self.negated and other.negated:
			return charclass(other.chars.difference(self.chars), negateMe=True)

		# ¬A OR B = ¬(A - B)
		if self.negated and not other.negated:
			return charclass(self.chars.difference(other.chars), negateMe=True)

		# ¬A OR ¬B = ¬(A AND B)
		if self.negated and other.negated:
			return charclass(self.chars.intersection(other.chars), negateMe=True)

		raise Exception("What")

	def difference(self, other):
		'''Subtract B from A'''

		# A - B = A - B
		if not self.negated and not other.negated:
			return charclass(self.chars.difference(other.chars))

		# A - ¬B = A AND B
		if not self.negated and other.negated:
			return charclass(self.chars.intersection(other.chars))

		# ¬A - B = ¬(A OR B)
		if self.negated and not other.negated:
			return charclass(self.chars.union(other.chars), negateMe=True)

		# ¬A - ¬B = B - A
		if self.negated and other.negated:
			return charclass(other.chars.difference(self.chars))

		raise Exception("What")

	def intersection(self, other):
		'''Find the classintersection of two charclasses, returning a charclass.'''

		# two regular classes intersect in the regular sense.
		if not self.negated and not other.negated:
			return charclass(self.chars.intersection(other.chars))

		# other combinations are more complex
		# A AND ¬B = A - B
		if not self.negated and other.negated:
			return charclass(self.chars.difference(other.chars))

		# ¬A AND B = B - A
		if self.negated and not other.negated:
			return charclass(other.chars.difference(self.chars))

		# ¬A AND ¬B = ¬(A OR B)
		if self.negated and other.negated:
			return charclass(self.chars.union(other.chars), negateMe=True)

		raise Exception("What")

	def issubset(self, other):
		'''Find out if A is a subset of B'''

		# A < B
		if not self.negated and not other.negated:
			return self.chars.issubset(other.chars)

		# A < ¬B if A n B = 0
		elif not self.negated and other.negated:
			return self.chars.isdisjoint(other.chars)

		# ¬A < B is impossible
		elif self.negated and not other.negated:
			return False

		# ¬A < ¬B if B < A
		elif self.negated and other.negated:
			return other.chars.issubset(self.chars)

		raise Exception("Whaaa")

# some useful constants
w = charclass("0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz")
d = charclass("0123456789")
s = charclass("\t\n\v\f\r ")
W = charclass("0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz", negateMe=True)
D = charclass("0123456789", negateMe=True)
S = charclass("\t\n\v\f\r ", negateMe=True)
dot = charclass(negateMe=True)

shorthand = {
	w : "\\w", d : "\\d", s : "\\s",
	W : "\\W", D : "\\D", S : "\\S",
	dot : ".",
}

class multiplier(lego):
	'''
		A min and a max. The vast majority of characters in regular
		expressions occur without a specific multiplier, which is implicitly
		equivalent to a min of 1 and a max of 1, but many more have explicit
		multipliers like "*" (min = 0, max = infinity) and so on. We use None to
		stand for infinity in the value of max.
	'''
	def __init__(self, min, max):
		if min is None:
			if max is None:
				pass
			else:
				raise Exception("max must match or exceed min")
		else:
			if type(min) != int:
				raise Exception("min must be an integer")
			if min < 0:
				raise Exception("min must be >= 0")
			if max is None:
				pass
			else:
				if type(max) != int:
					raise Exception("max must be None or an integer")
				if max < min or min is None:
					raise Exception("max '" + str(max) + "' must match or exceed min '" + str(min) + "'")

		self.__dict__['min'] = min
		self.__dict__['max'] = max

	def __eq__(self, other):
		return type(self) == type(other) \
		and self.min == other.min \
		and self.max == other.max

	def __hash__(self):
		return tuple.__hash__((self.min, self.max))

	def __repr__(self):
		return "multiplier(" + str(self.min) + ", " + str(self.max) + ")"

	def regex(self):
		if self.max == 0 \
		or self.min is None:
			raise Exception("No regex available for " + str(self))
		if self in symbolic.keys():
			return symbolic[self]
		if self.max is None:
			return "{" + str(self.min) + ",}"
		if self.min == self.max:
			return "{" + str(self.min) + "}"
		return "{" + str(self.min) + "," + str(self.max) + "}"

	@classmethod
	def match(cls, string, i):
		# {2,3}
		try:
			j = cls.parseStatic(string, i, "{")
			min, j = cls.parseInteger(string, j)
			j = cls.parseStatic(string, j, ",")
			max, j = cls.parseInteger(string, j)
			j = cls.parseStatic(string, j, "}")
			return multiplier(min, max), j
		except ParseFailureException:
			pass

		# {2,}
		try:
			j = cls.parseStatic(string, i, "{")
			min, j = cls.parseInteger(string, j)
			j = cls.parseStatic(string, j, ",}")
			return multiplier(min, None), j
		except ParseFailureException:
			pass

		# {2}
		try:
			j = cls.parseStatic(string, i, "{")
			min, j = cls.parseInteger(string, j)
			j = cls.parseStatic(string, j, "}")
			return multiplier(min, min), j
		except ParseFailureException:
			pass

		# "?"/"*"/"+"/""
		# we do these in reverse order of symbol length, because
		# that forces "" to be done last
		for key in sorted(symbolic, key=lambda key: -len(symbolic[key])):
			try:
				return key, cls.parseStatic(string, i, symbolic[key])
			except ParseFailureException:
				pass

		raise ParseFailureException

	@classmethod
	def parseInteger(cls, string, i):
		try:
			return 0, cls.parseStatic(string, i, "0")
		except ParseFailureException:
			pass

		digit, i = cls.parseAny(string, i, "123456789")
		integer = int(digit)
		try:
			while True:
				digit, i = cls.parseAny(string, i, "0123456789")
				integer *= 10
				integer += int(digit)
		except ParseFailureException:
			return integer, i

	def __mul__(self, other):
		'''Multiply this multiplier by another'''
		if self.min is None or other.min is None:
			min = None
		else:
			min = self.min * other.min
		if self.max is None or other.max is None:
			max = None
		else:
			max = self.max * other.max
		return multiplier(min, max)

	def __add__(self, other):
		'''Add another multiplier to this one'''
		if self.min is None or other.min is None:
			min = None
		else:
			min = self.min + other.min
		if self.max is None or other.max is None:
			max = None
		else:
			max = self.max + other.max
		return multiplier(min, max)

	def getMO(self):
		'''
			More useful than "min" and "max" in many situations
			are "mandatory" and "optional"
		'''
		if self.min is None:
			# infinite mandatory, infinite optional (this is odd)
			if self.max is None:
				return None, None
			raise Exception("wha")
		if self.max is None:
			return self.min, None
		return self.min, self.max - self.min

	def __sub__(self, other):
		'''
			Subtract another multiplier from this one.
			This is a bit of a nightmare.
			Caution: multipliers are not totally ordered.
			This operation is not meaningful for all pairs of multipliers.
		'''
		# get that info from self
		selfMandatory, selfOptional = self.getMO()

		# get that info from other
		otherMandatory, otherOptional = other.getMO()

		# subtract other from self
		if otherMandatory is None:
			if selfMandatory is None:
				newMandatory = 0 # None minus None
			else:
				raise Exception("Can't subtract " + str(other) + " from " + str(self))
		else:
			if selfMandatory is None:
				newMandatory = None
			else:
				newMandatory = selfMandatory - otherMandatory

		if otherOptional is None:
			if selfOptional is None:
				newOptional = 0 # None minus None
			else:
				raise Exception("Can't subtract " + str(other) + " from " + str(self))
		else:
			if selfOptional is None:
				newOptional = None
			else:
				newOptional = selfOptional - otherOptional

		# turn info back into min/max
		if newMandatory is None:
			if newOptional is None:
				newMin, newMax = None, None
			else:
				raise Exception("Can't subtract " + str(other) + " from " + str(self))
		else:
			if newOptional is None:
				newMin, newMax = newMandatory, None
			else:
				newMin, newMax = newMandatory, newMandatory + newOptional

		return multiplier(newMin, newMax)

	def __and__(self, other):
		'''
			Find the shared part of two multipliers.
			This is a bit of a nightmare as well
		'''
		selfMandatory , selfOptional  = self.getMO()
		otherMandatory, otherOptional = other.getMO()

		if selfMandatory is None:
			if otherMandatory is None:
				commonMandatory = None
			else:
				commonMandatory = otherMandatory
		else:
			if otherMandatory is None:
				commonMandatory = selfMandatory
			else:
				commonMandatory = min(selfMandatory, otherMandatory)

		if selfOptional is None:
			if otherOptional is None:
				commonOptional = None
			else:
				commonOptional = otherOptional
		else:
			if otherOptional is None:
				commonOptional = selfOptional
			else:
				commonOptional = min(selfOptional, otherOptional)

		# turn info back into min/max
		if commonMandatory is None:
			if commonOptional is None:
				newMin, newMax = None, None
			else:
				raise Exception("Can't common " + str(other) + " from " + str(self))
		else:
			if commonOptional is None:
				newMin, newMax = commonMandatory, None
			else:
				newMin, newMax = commonMandatory, commonMandatory + commonOptional

		return multiplier(newMin, newMax)

zero = multiplier(0, 0) # has some occasional uses
qm   = multiplier(0, 1)
one  = multiplier(1, 1)
star = multiplier(0, None)
plus = multiplier(1, None)
inf  = multiplier(None, None) # has some very occasional uses

symbolic = {
	qm   : "?",
	one  : "" ,
	star : "*",
	plus : "+",
}

class NoCommonMultiplicandException(Exception):
	'''
		This happens when you try to intersect or subtract two mults which
		don't have a common multiplicand.
	'''

class NoCommonPrefixException(Exception):
	'''
		This happens when you call pattern.commonprefix() on a pattern whose
		concs don't have a common prefix.
	'''

class mult(lego):
	'''
		A mult is a combination of a multiplicand (a charclass or subpattern) with
		a multiplier (a min and a max). The vast majority of characters in regular
		expressions occur without a specific multiplier, which is implicitly
		equivalent to a min of 1 and a max of 1, but many more have explicit
		multipliers like "*" (min = 0, max = infinity) and so on. We use None to
		stand for infinity in the value of max.

		e.g. a, b{2}, c?, d*, [efg]{2,5}, f{2,}, (anysubpattern)+, .*, and so on
	'''

	def __init__(self, multiplicand, x):
		if type(multiplicand) not in {charclass, pattern}:
			raise Exception("Wrong type '" + str(type(multiplicand)) + "' for multiplicand")
		if type(x) != multiplier:
			raise Exception("Wrong type '" + str(type(x)) + "' for multiplier")

		self.__dict__["multiplicand"] = multiplicand
		self.__dict__["multiplier"]   = x

	def __eq__(self, other):
		return type(self) == type(other) and \
		type(self.multiplicand) == type(other.multiplicand) and \
		self.multiplicand == other.multiplicand and \
		self.multiplier == other.multiplier

	def __hash__(self):
		return tuple.__hash__((self.multiplicand, self.multiplier))

	def __repr__(self):
		string = "mult("
		string += ", ".join([str(self.multiplicand), str(self.multiplier)])
		string += ")"
		return string

	def __mul__(self, multiplier):
		if multiplier == one:
			return self
		return mult(self.multiplicand, self.multiplier * multiplier)

	def __add__(self, other):
		'''Concatenation'''
		return conc(self) + other

	def __sub__(self, other):
		'''
			Subtract another mult from this one and return the result.
			The reverse of concatenation. This is a lot trickier.
			e.g. a{4,5} - a{3} = a{1,2}
		'''
		if other.multiplicand != self.multiplicand:
			raise NoCommonMultiplicandException("Can't subtract " + str(other) + " from " + str(self))

		return mult(self.multiplicand, self.multiplier - other.multiplier)

	def __and__(self, other):
		'''Find the shared part of two mults.'''
		if other.multiplicand != self.multiplicand:
			raise NoCommonMultiplicandException("Can't find intersection of " + str(other) + " with " + str(self))

		return mult(self.multiplicand, self.multiplier & other.multiplier)

	def reduce(self):
		'''
			Return a lego piece with the same matching power but
			potentially reduced complexity.
		'''
		# no point multiplying in the singular
		if self.multiplier == one:
			return self.multiplicand.reduce()
	
		# Try recursively reducing our internals
		reducedMultiplicand = self.multiplicand.reduce()
		# "bulk up" smaller lego pieces to pattern if need be
		if type(reducedMultiplicand) == mult:
			reducedMultiplicand = conc(reducedMultiplicand)
		if type(reducedMultiplicand) == conc:
			reducedMultiplicand = pattern(reducedMultiplicand)
		if reducedMultiplicand != self.multiplicand:
			return mult(reducedMultiplicand, self.multiplier).reduce()

		# If our multiplicand is a pattern containing an empty conc()
		# (and at least one other conc), we can pull that "optional"
		# bit out into our own multiplier instead.
		# e.g. (A|B|C|)D -> (A|B|C)?D
		# e.g. (A|B|C|){2} -> (A|B|C){0,2}
		if type(self.multiplicand) == pattern \
		and emptystring in self.multiplicand.concs \
		and len(self.multiplicand.concs) > 1:
			return mult(
				pattern(
					*self.multiplicand.concs.difference({emptystring})
				),
				self.multiplier * qm,
			).reduce()

		# If our multiplicand is a pattern containing a single conc
		# containing a single mult, we can separate that out a lot
		# e.g. ([ab])* -> [ab]*
		if type(self.multiplicand) == pattern \
		and len(self.multiplicand.concs) == 1:
			singleton = [x for x in self.multiplicand.concs][0]
			if len(singleton.mults) == 1:
				return mult(
					singleton.mults[0].multiplicand,
					singleton.mults[0].multiplier * self.multiplier
				).reduce()

		return self

	def regex(self):
		'''Return a string representation of the mult represented here.'''

		output = ""

		# recurse into subpattern
		if type(self.multiplicand) is pattern:
			output += "(" + self.multiplicand.regex() + ")"

		else: 
			output += self.multiplicand.regex()

		# try this with "a?b*c+d{1}e{1,2}f{3,}g{,5}h{8,8}"
		suffix = self.multiplier.regex()

		# Pick whatever is shorter/more comprehensible.
		# e.g. "aa" beats "a{2}", "ababab" beats "(ab){3}"
		if self.multiplier.min == self.multiplier.max and \
		len(output) * self.multiplier.min <= len(output) + len(suffix):
			output += str(output) * (self.multiplier.min - 1) # because it has one already
		else:
			output += suffix

		return output

	@classmethod
	def match(cls, string, i):
		try:
			j = cls.parseStatic(string, i, "(")
			multiplicand, j = pattern.match(string, j)
			j = cls.parseStatic(string, j, ")")
		except ParseFailureException:
			multiplicand, j = charclass.match(string, i)

		x, j = multiplier.match(string, j)
		return mult(multiplicand, x), j

class conc(lego):
	'''
		A conc (short for "concatenation") is a tuple of mults i.e. an unbroken
		string of mults occurring one after the other.
		e.g. abcde[^fg]*h{4}[a-z]+(subpattern)(subpattern2)
		To express the empty string, use an empty conc, conc(()).
	'''

	def __init__(self, *mults):
		for x in mults:
			if type(x) is not mult:
				raise Exception(str(x) + " is not a mult")
		self.__dict__["mults"] = tuple(mults)

	def __eq__(self, other):
		return type(self) == type(other) and \
		self.mults == other.mults

	def __hash__(self):
		return tuple.__hash__(self.mults)

	def __repr__(self):
		string = "conc("
		string += ", ".join(str(x) for x in self.mults)
		string += ")"
		return string

	def __mul__(self, multiplier):
		if multiplier == one:
			return self
		# Have to replace self with a pattern unfortunately
		return pattern(self) * multiplier

	def __add__(self, other):
		'''
			Magical function for the concatenation of any two pieces of lego. All
			calls are redirected into the specific (conc, conc) case and then reduced
			afterwards if possible.
		'''

		# TODO: this would be so sweet
		# return conc(*(self.mults + other.mults)).reduce()

		# other must be a conc too
		if type(other) in {charclass, pattern}:
			other = mult(other, one)
		if type(other) is mult:
			other = conc(other)

		# conc versus conc
		if len(self.mults) == 0:
			if len(other.mults) == 0:
				result = emptystring
			else:
				result = other
		else:
			if len(other.mults) == 0:
				result = self
			else:
				multsA = list(self.mults)
				multsB = list(other.mults)

				# see if we can combine the last mult in self with
				# the first mult in other
				lastMultA  = multsA.pop()
				firstMultB = multsB.pop(0)

				# if the multiplicands in each mult are identical, then
				# we can just add the multiplicands
				if lastMultA.multiplicand == firstMultB.multiplicand:
					multC = mult(lastMultA.multiplicand, lastMultA.multiplier + firstMultB.multiplier)
					multsC = [multC]
				else:
					multsC = [lastMultA, firstMultB]

				# now multsC is a list just like multsA and newB

				# concatenate the lists
				result = conc(*(multsA + multsC + multsB))

		return result.reduce()

	def reduce(self):
		'''
			Return a possibly-simpler lego piece.
			It is critically important to (1) always call reduce()
			on whatever you're returning before you return it and
			therefore (2) always return something STRICTLY SIMPLER
			than the current object. Otherwise infinite loops become
			possible in reduce() calls
		'''

		# no point concatenating one thing (note: concatenating nothing is
		# entirely valid)
		if len(self.mults) == 1:
			return self.mults[0].reduce()

		# Try recursively reducing our internals
		reducedMults = [x.reduce() for x in self.mults]
		# "bulk up" smaller lego pieces to concs if need be
		reducedMults = [pattern(x) if type(x) == conc else x for x in reducedMults]
		reducedMults = [mult(x, one) if type(x) in {charclass, pattern} else x for x in reducedMults]
		reducedMults = tuple(reducedMults)
		if reducedMults != self.mults:
			return conc(*reducedMults).reduce()

		# multiple mults with identical multiplicands in a row?
		# squish those together
		# e.g. ab?b?c -> ab{1,2}c
		if len(self.mults) > 1:
			for i in range(len(self.mults)-1):
				if self.mults[i].multiplicand == self.mults[i+1].multiplicand:
					squished = mult(
						self.mults[i].multiplicand,
						self.mults[i].multiplier + self.mults[i+1].multiplier
					)
					newMults = self.mults[:i] + (squished,) + self.mults[i+2:]
					return conc(*newMults).reduce()

		# TODO:
		# Conc contains a *singleton* mult containing a pattern with only
		# one internal conc? Flatten out.
		# e.g. "a(a(ab|a*c))" -> "aa(ab|a*c)"
		# BUT NOT "a(a(ab|a*c)){2,}"
		# AND NOT "a(a(ab|a*c)|y)"

		return self

	def regex(self):
		'''Return a string representation of regex which this conc
		represents.'''
		return "".join(mult.regex() for mult in self.mults)

	@classmethod
	def match(cls, string, i):
		mults = list()
		try:
			while True:
				x, i = mult.match(string, i)
				mults.append(x)
		except ParseFailureException:
			pass
		return conc(*mults), i

emptystring = conc()

class pattern(lego):
	'''
		A pattern (also known as an "alt", short for "alternation") is a
		set of concs. The simplest pattern contains a single conc, but it
		is also possible for a pattern to contain multiple alternate possibilities.
		When written out as a regex, these would separated by pipes. A pattern
		containing no possibilities should be impossible.
		
		e.g. "abc|def(ghi|jkl)" is an alt containing two concs: "abc" and
		"def(ghi|jkl)". The latter is a conc containing four mults: "d", "e", "f"
		and "(ghi|jkl)". The latter in turn is a mult consisting of an upper bound
		1, a lower bound 1, and a multiplicand which is a new subpattern, "ghi|jkl".
		This new subpattern again consists of two concs: "ghi" and "jkl".
	'''
	def __init__(self, *concs):
		if len(concs) < 1:
			raise Exception("Can't take alternation of nothing")
		for x in concs:
			if type(x) != conc:
				raise Exception("Can't put a " + str(type(x)) + " in a pattern")
		self.__dict__["concs"] = frozenset(concs)

	def __eq__(self, other):
		return type(self) == type(other) \
		and self.concs == other.concs

	def __hash__(self):
		return frozenset.__hash__(self.concs)

	def __repr__(self):
		string = "pattern("
		string += ", ".join(str(x) for x in self.concs)
		string += ")"
		return string

	def __mul__(self, multiplier):
		if multiplier == one:
			return self
		return mult(self, multiplier)

	def __add__(self, other):
		'''Concatenation function. Turn self into an equivalent conc'''
		return self._concsuffix() + other

	def regex(self):
		'''Return the string representation of the regex which this pattern
		represents.'''

		# take the alternation of the input collection of regular expressions.
		# i.e. jam "|" between each element

		# 1+ elements.
		return "|".join(sorted(conc.regex() for conc in self.concs))

	def reduce(self):
		'''Return a possibly-simplified self'''

		# no point alternating among one possibility
		if len(self.concs) == 1:
			return [e for e in self.concs][0].reduce()

		# Try recursively reducing our internals first.
		reducedConcs = [x.reduce() for x in self.concs]
		# "bulk up" smaller lego pieces to concs if need be
		reducedConcs = [mult(x, one) if type(x) in {charclass, pattern} else x for x in reducedConcs]
		reducedConcs = [conc(x) if type(x) == mult else x for x in reducedConcs]
		reducedConcs = frozenset(reducedConcs)
		if reducedConcs != self.concs:
			return pattern(*reducedConcs).reduce()

		# If this pattern contains several concs each containing just 1 mult
		# each containing just a charclass, with identical multipliers,
		# then we can merge those branches together.
		# e.g.

		# pattern(
		# 	conc(mult(charclass("0"), one)),
		# 	conc(mult(charclass("123456789"), one)),
		#	)
		# "0|[1-9]"

		# becomes
		# pattern(
		#		conc(mult(charclass("0123456789"), one)),
		#	)
		# "[0-9]"

		# Do this for all distinct multipliers.
		# Keep track of whether anything actually changed. If not,
		# don't actually try to change anything, or we'll end up
		# recursing forever due to that final "reduce()" call
		isChanged = False
		merged = {} # key is multiplier, value is all merged charclasses at that multiplier
		rest = []
		for x in self.concs:
			if len(x.mults) == 1 \
			and type(x.mults[0].multiplicand) == charclass:
				key = x.mults[0].multiplier
				if key not in merged:
					merged[key] = x.mults[0].multiplicand
				else:
					merged[key] = merged[key].union(x.mults[0].multiplicand)
					isChanged = True
			else:
				rest.append(x)
		if isChanged == True:
			for key in merged:
				rest.append(conc(mult(merged[key], key)))
			return pattern(*rest).reduce()

#		# If the present pattern's concs all have a common prefix, split
#		# that out. This increases the depth of the object
#		# but it is still arguably simpler/ripe for further reduction
#
#		# Stage 1: Check the first mult in each conc to see if they have anything
#		# in common.
#			try:
#				common = self.commonprefix()
#			except NoCommonPrefixException:
#				pass
#
#			leftovers = [x - common for x in firstMults]
#
#			# Success! The first mults have a common part.
#			# Slash that common part from all of the mults and, if appropriate,
#			# try again
#			continueFlag = True
#			newMults.append(common)
#			for j in range(len(leftovers)):
#				if leftovers[j].multiplier == zero:
#					multListList[j].pop(0)
#				else:
#					# partial success, stop now
#					multListList[j][0] = leftovers[j]
#					continueFlag = False
#
#			if continueFlag is False:
#				break
#
#		# now what is left of multListList is going to form the first mult
#		# in the new conc
#		concList = [conc(*multList) for multList in multListList]

		return self

	@classmethod
	def match(cls, string, i):
		concs = list()

		# first one
		x, i = conc.match(string, i)
		concs.append(x)

		# the rest
		try:
			while True:
				i = cls.parseStatic(string, i, "|")
				x, i = conc.match(string, i)
				concs.append(x)
		except ParseFailureException:
			pass

		return pattern(*concs), i

	def _concprefix(self):
		'''
			"ZA|ZB|ZC"   -> "Z(A|B|C)"

			Outputs: a conc with exactly equivalent matching capability to the
			present pattern.
			The output conc's leading internal mults will form the common prefix,
			if there was one. After that, the last mult will contain a pattern,
			itself containing all the concs which are left over once the
			common prefix has been truncated from the front of each conc. In the
			event that nothing is left of an input conc after this truncation,
			the corresponding output conc will be empty.

			conc
				mult
					common suffix to concs 1, 2, etc.
				mult
					common suffix to concs 1, 2, etc.
				etc.
				mult
					pattern
						conc: non-shared tail from conc 1 (possibly empty)
						conc: non-shared tail from conc 2 (possibly empty)
						etc.

			Usually the multiplier on that last mult will be (1, 1). But if
			any of its internal concs are empty, they'll be stripped out and
			the multiplier will be 0, 1 instead
			"ZA|ZB|ZC|Z" -> "Z(A|B|C|)" -> "Z(A|B|C)?"

			If the prefix encompasses the whole deal, there will be no first
			element
			"CZ|CZ" -> "CZ()" -> "CZ"

			If no common prefix mult can be found, an exception is thrown.
			This operates recursively.
		'''

		# Try to find just one mult to put into that commonprefix.
		# Check the first mult in each conc to see if they have anything in common.
		commonMult = None
		for x in self.concs:

			# No common prefix here
			if len(x.mults) == 0:
				return [], self

			if commonMult is None:
				commonMult = x.mults[0]
			else:
				try:
					commonMult &= x.mults[0]
				except NoCommonMultiplicandException:
					return [], self

		# If we're here then a common mult was found.
		prefix = [commonMult]

		# Now construct a new pattern from the leftovers.

		# First, subtract common from the front to give a remainder
		recurseFlag = True
		concList = []
		for x in self.concs:
			newMult1 = x.mults[0] - commonMult
			if newMult1.multiplier == zero:
				# omit that mult entirely since it has been factored out
				newConc = conc(*x.mults[1:])
			else:
				# partial success, end now
				recurseFlag = False
				newConc = conc(newMult1, *x.mults[1:])
			concList.append(newConc)

		# rebuild
		leftover = pattern(*concList)

		# So at the very least we could "return prefix, leftover"
		# right now. But maybe leftover has more to give up.

		if recurseFlag is True:
			innerPrefix, leftover = leftover._concprefix()
			prefix.extend(innerPrefix)

		return prefix, leftover

	def _concsuffix(self):
		'''
			"AZ|BZ|CZ"   -> "(A|B|C)Z"

			Input: a collection of concs, such as those given by pattern.concs,
			to be alternated between.

			conc 1
			conc 2
			etc.

			Outputs: a conc with exactly equivalent matching capability.
			The output conc's trailing internal mults will form the common suffix,
			if there was one. Before that, the first mult will contain a pattern,
			itself containing all the concs which are left over once the
			common suffix has been truncated from the end of each conc. In the
			event that nothing is left of an input conc after this truncation,
			the output conc will be empty.
			
			conc
				mult
					pattern
						conc: non-shared initial from conc 1 (possibly empty)
						conc: non-shared initial from conc 2 (possibly empty)
						etc.
				mult
					common suffix to concs 1, 2, etc.
				mult
					common suffix to concs 1, 2, etc.
				etc.

			Usually the multiplier on that first mult will be (1, 1). But if
			any of its internal concs are empty, they'll be stripped out and
			the multiplier will be 0, 1 instead
			"AZ|BZ|CZ|Z" -> "(A|B|C)?Z"

			If the suffix encompasses the whole deal, there will be no first
			element
			"CZ|CZ" -> "()CZ" -> "CZ"
		'''

		# need to get those immutable objects out where we can mute them.
		multListList = [
			[
				copy.deepcopy(y) for y in x.mults
			] for x in self.concs
		]

		# Stage 1: Check the final mult in each conc to see if they have anything
		# in common.
		newMults = list()
		while min(len(multList) for multList in multListList) > 0:

			# compare the mults at last position in the list.
			finalMults = [multList[-1] for multList in multListList]

			try:
				common = mult(finalMults[0].multiplicand, inf)
				for x in finalMults:
					common &= x

			except NoCommonMultiplicandException:
				# Failure: the final mults have nothing in common. No truncation can
				# be achieved today
				break

			leftovers = [x - common for x in finalMults]

			# Success! The final mults have a common part.
			# Slash that common part from all of the mults and, if appropriate,
			# try again
			continueFlag = True
			newMults.insert(0, common)
			for j in range(len(leftovers)):
				if leftovers[j].multiplier == zero:
					multListList[j].pop()
				else:
					# partial success, stop now
					multListList[j][-1] = leftovers[j]
					continueFlag = False

			if continueFlag is False:
				break

		# now what is left of multListList is going to form the first mult
		# in the new conc
		concList = [conc(*multList) for multList in multListList]

		# question mark...
		if emptystring in concList:
			isqm = True
			concList.remove(emptystring)
		else:
			isqm = False

		# If there is nothing left, don't bother inserting it.
		# You'll get a "can't alternate nothing" error from the pattern
		# constructor anyway.
		if len(concList) > 0:
			if isqm:
				newMults.insert(0, mult(pattern(*concList), qm))
			else:
				newMults.insert(0, mult(pattern(*concList), one))

		return conc(*newMults)

# unit tests
if __name__ == '__main__':
	# make sure recursion problem in reduce()
	# has gone away
	emptystring + mult(
		pattern(
			conc(mult(charclass("123456789"), one)),
			conc(mult(charclass("0"), one))
		),
		one
	)

	# charclass equality
	assert charclass("a") == charclass("a")
	assert charclass("a", negateMe=True) == charclass("a", negateMe=True)
	assert charclass("a", negateMe=True) != charclass("a")
	assert charclass("ab") == charclass("ba")

	# charclass.regex()
	assert w.regex() == "\\w"
	assert d.regex() == "\\d"
	assert s.regex() == "\\s"
	assert charclass("a").regex() == "a"
	assert charclass("{").regex() == "\\{"
	assert charclass("\t").regex() == "\\t"
	assert charclass("ab").regex() == "[ab]"
	assert charclass("a{").regex() == "[a{]"
	assert charclass("a\t").regex() == "[\\ta]"
	assert charclass("a-").regex() == "[\\-a]"
	assert charclass("a[").regex() == "[\\[a]"
	assert charclass("a]").regex() == "[\\]a]"
	assert charclass("ab").regex() == "[ab]"
	assert charclass("abc").regex() == "[abc]"
	assert charclass("abcd").regex() == "[a-d]"
	assert charclass("abcdfghi").regex() == "[a-df-i]"
	assert charclass("^").regex() == "^"
	assert charclass("a^").regex() == "[\\^a]"
	assert charclass("0123456789a").regex() == "[0-9a]"
	assert charclass("\t\n\v\f\r A").regex() == "[\\t\\n\\v\\f\\r A]"
	assert charclass("0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz|").regex() == "[0-9A-Z_a-z|]"
	assert W.regex() == "\\W"
	assert D.regex() == "\\D"
	assert S.regex() == "\\S"
	assert charclass(negateMe=True).regex() == "."
	assert charclass("", negateMe=True).regex() == "."
	assert charclass("a", negateMe=True).regex() == "[^a]"
	assert charclass("{", negateMe=True).regex() == "[^{]"
	assert charclass("\t", negateMe=True).regex() == "[^\\t]"
	assert charclass("^", negateMe=True).regex() == "[^\\^]"

	# charclass parsing
	assert charclass.match("a", 0) == (charclass("a"), 1)
	assert charclass.match("aa", 1) == (charclass("a"), 2)
	assert charclass.match("a$", 1) == (charclass("$"), 2)
	assert charclass.match(".", 0) == (charclass(negateMe=True), 1)
	try:
		charclass.match("[", 0)
		assert False
	except ParseFailureException:
		pass
	try:
		charclass.match("a", 1)
		assert False
	except ParseFailureException:
		pass

	# charclass set operations

	# negate()
	# ¬[ab] = [^ab]
	assert charclass("ab").negate() == charclass("ab", negateMe=True)
	# ¬[^ab] = [ab]
	assert charclass("ab", negateMe=True).negate() == charclass("ab")

	# union()
	# [ab] u [bc] = [abc]
	assert charclass("ab").union(charclass("bc")) == charclass("abc")
	# [ab] u [^bc] = [^c]
	assert charclass("ab").union(charclass("bc", negateMe=True)) == charclass("c", negateMe=True)
	# [^a] u [bc] = [^a]
	assert charclass("ab", negateMe=True).union(charclass("bc")) == charclass("a", negateMe=True)
	# [^ab] u [^bc] = [^b]
	assert charclass("ab", negateMe=True).union(charclass("bc", negateMe=True)) == charclass("b", negateMe=True)

	# difference()
	# [ab] - [bc] = [a]
	assert charclass("ab").difference(charclass("bc")) == charclass("a")
	# [ab] - [^bc] = [b]
	assert charclass("ab").difference(charclass("bc", negateMe=True)) == charclass("b")
	# [^ab] - [bc] = [^abc]
	assert charclass("ab", negateMe=True).difference(charclass("bc")) == charclass("abc", negateMe=True)
	# [^ab] - [^bc] = [c]
	assert charclass("ab", negateMe=True).difference(charclass("bc", negateMe=True)) == charclass("c")

	# intersection()
	# [ab] n [bc] = [b]
	assert charclass("ab").intersection(charclass("bc")) == charclass("b")
	# [ab] n [^bc] = [a]
	assert charclass("ab").intersection(charclass("bc", negateMe=True)) == charclass("a")
	# [^ab] n [bc] = [c]
	assert charclass("ab", negateMe=True).intersection(charclass("bc")) == charclass("c")
	# [^ab] n [^bc] = [^abc]
	assert charclass("ab", negateMe=True).intersection(charclass("bc", negateMe=True)) == charclass("abc", negateMe=True)

	# issubset()
	# [a] < [ab] = True
	assert charclass("a").issubset(charclass("ab"))
	# [c] < [^ab] = True
	assert charclass("c").issubset(charclass("ab", negateMe=True))
	# [^c] < [ab] = False
	assert not charclass("c", negateMe=True).issubset(charclass("ab"))
	# [^ab] < [^a] = True
	assert charclass("ab", negateMe=True).issubset(charclass("a", negateMe=True))

	# mult equality
	assert mult(charclass("a"), one) == mult(charclass("a"), one)
	assert mult(charclass("a"), one) != mult(charclass("b"), one)
	assert mult(charclass("a"), one) != mult(charclass("a"), qm)
	assert mult(charclass("a"), one) != mult(charclass("a"), multiplier(1, 2))

	# mult.regex() tests
	a = charclass("a")
	assert mult(a, one).regex() == "a"
	assert mult(a, multiplier(2, 2)).regex() == "aa"
	assert mult(a, multiplier(3, 3)).regex() == "aaa"
	assert mult(a, multiplier(4, 4)).regex() == "aaaa"
	assert mult(a, multiplier(5, 5)).regex() == "a{5}"
	assert mult(a, qm).regex() == "a?"
	assert mult(a, star).regex() == "a*"
	assert mult(a, plus).regex() == "a+"
	assert mult(a, multiplier(2, 5)).regex() == "a{2,5}"
	assert mult(a, multiplier(2, None)).regex() == "a{2,}"
	assert mult(d, one).regex() == "\\d"
	assert mult(d, multiplier(2, 2)).regex() == "\\d\\d"
	assert mult(d, multiplier(3, 3)).regex() == "\\d{3}"

	# mult parsing
	assert mult.match("[a-g]+", 0) == (
		mult(charclass("abcdefg"), plus),
		6
	)
	assert mult.match("[a-g0-8$%]+", 0) == (
		mult(charclass("abcdefg012345678$%"), plus),
		11
	)
	assert mult.match("[a-g0-8$%\\^]+", 0) == (
		mult(charclass("abcdefg012345678$%^"), plus),
		13
	)
	assert mult.match("abcde[^fg]*", 5) == (
		mult(charclass("fg", negateMe=True), star),
		11
	)
	assert mult.match("abcde[^fg]*h{5}[a-z]+", 11) == (
		mult(charclass("h"), multiplier(5, 5)),
		15
	)
	assert mult.match("abcde[^fg]*h{5}[a-z]+", 15) == (
		mult(charclass("abcdefghijklmnopqrstuvwxyz"), plus),
		21
	)

	# mult.reduce() tests

	# mult -> mult
	assert mult(charclass("a"), qm).reduce() == mult(charclass("a"), qm)
	# mult -> charclass
	assert mult(charclass("a"), one).reduce() == charclass("a")

	# mult contains a pattern containing an empty conc? Pull the empty
	# part out where it's external
	assert mult(
		pattern(
			conc(mult(charclass("a"), one)),
			conc(mult(charclass("b"), star)),
			emptystring
		), multiplier(2, 2)
	).reduce() == mult(
		pattern(
			conc(mult(charclass("a"), one)),
			conc(mult(charclass("b"), star)),
		), multiplier(0, 2)
	)

	# but don't do this if emptystring is the only thing inside the pattern.
	# that would be bad
	assert mult(
		pattern(
			emptystring
		), multiplier(2, 2)
	).reduce() == mult(pattern(emptystring), multiplier(2, 2))

	# mult contains a pattern containing a single conc containing a single mult?
	# that can be reduced greatly
	# e.g. "([ab])*" -> "[ab]*"
	assert mult(
		pattern(
			conc(
				mult(charclass("ab"), one)
			)
		), star
	).reduce() == mult(charclass("ab"), star)
	# e.g. "(c{1,2}){3,4}" -> "c{3,8}"
	assert mult(
		pattern(
			conc(
				mult(charclass("c"), multiplier(1, 2))
			)
		), multiplier(3, 4)
	).reduce() == mult(charclass("c"), multiplier(3, 8))

	# recursive mult reduction
	assert mult(
		pattern(
			conc(mult(charclass("a"), one)),
			conc(mult(charclass("b"), one)),
		), star
	).reduce() == mult(charclass("ab"), star)

	# mult subtraction
	# a{4,5} - a{3} = a{1,2}
	assert mult(
		charclass("a"),
		multiplier(4, 5)
	) - mult(
		charclass("a"),
		multiplier(3, 3)
	) == mult(
		charclass("a"),
		multiplier(1, 2)
	)

	# conc equality
	assert conc(mult(charclass("a"), one)) == conc(mult(charclass("a"), one))
	assert conc(mult(charclass("a"), one)) != conc(mult(charclass("b"), one))
	assert conc(mult(charclass("a"), one)) != conc(mult(charclass("a"), qm))
	assert conc(mult(charclass("a"), one)) != conc(mult(charclass("a"), multiplier(1, 2)))
	assert conc(mult(charclass("a"), one)) != emptystring

	# conc.regex() tests
	assert conc(
		mult(charclass("a"), one),
		mult(charclass("b"), one),
		mult(charclass("c"), one),
		mult(charclass("d"), one),
		mult(charclass("e"), one),
		mult(charclass("fg", negateMe=True), star),
		mult(charclass("h"), multiplier(5, 5)),
		mult(charclass("abcdefghijklmnopqrstuvwxyz"), plus),
	).regex() == "abcde[^fg]*h{5}[a-z]+"

	# conc parsing
	assert conc.match("abcde[^fg]*h{5}[a-z]+", 0) == (
		conc(
			mult(charclass("a"), one),
			mult(charclass("b"), one),
			mult(charclass("c"), one),
			mult(charclass("d"), one),
			mult(charclass("e"), one),
			mult(charclass("fg", negateMe=True), star),
			mult(charclass("h"), multiplier(5, 5)),
			mult(charclass("abcdefghijklmnopqrstuvwxyz"), plus),
		), 21
	)
	assert conc.match("[bc]*[ab]*", 0) == (
		conc(
			mult(charclass("bc"), star),
			mult(charclass("ab"), star),
		),
		10
	)
	assert conc.match("abc...", 0) == (
		conc(
			mult(charclass("a"), one),
			mult(charclass("b"), one),
			mult(charclass("c"), one),
			mult(charclass(negateMe=True), one),
			mult(charclass(negateMe=True), one),
			mult(charclass(negateMe=True), one),
		),
		6
	)
	assert conc.match("\\d{4}-\\d{2}-\\d{2}", 0) == (
		conc(
			mult(charclass("0123456789"), multiplier(4, 4)),
			mult(charclass("-"), one),
			mult(charclass("0123456789"), multiplier(2, 2)),
			mult(charclass("-"), one),
			mult(charclass("0123456789"), multiplier(2, 2)),
		),
		17
	)

	# conc.reduce()
	# conc -> conc
	assert conc(
		mult(charclass("a"), one),
		mult(charclass("b"), one),
	).reduce() == conc(
		mult(charclass("a"), one),
		mult(charclass("b"), one),
	)
	# conc -> mult
	assert conc(
		mult(charclass("a"), multiplier(3, 4)),
	).reduce() == mult(charclass("a"), multiplier(3, 4))
	# conc -> charclass
	assert conc(
		mult(charclass("a"), one),
	).reduce() == charclass("a")

	# sequence squooshing of mults within a conc
	# e.g. "[$%\\^]?[$%\\^]" -> "[$%\\^]{1,2}"
	assert conc(
		mult(charclass("a"), one),
		mult(charclass("$%^"), qm),
		mult(charclass("$%^"), one),
		mult(charclass("b"), one),
	).reduce() == conc(
		mult(charclass("a"), one),
		mult(charclass("$%^"), multiplier(1, 2)),
		mult(charclass("b"), one)
	)

	# recursive conc reduction
	# (a){2}b -> a{2}b
	assert conc(
		mult(
			pattern(
				conc(
					mult(charclass("a"), qm)
				)
			), plus
		),
		mult(charclass("b"), one)
	).reduce() == conc(
		mult(charclass("a"), star),
		mult(charclass("b"), one)
	).reduce()

	# pattern equality
	assert pattern(
		conc(mult(charclass("a"), one)),
		conc(mult(charclass("b"), one)),
	) == pattern(
		conc(mult(charclass("b"), one)),
		conc(mult(charclass("a"), one)),
	)
	assert pattern(
		conc(mult(charclass("a"), one)),
		conc(mult(charclass("a"), one)),
	) == pattern(
		conc(mult(charclass("a"), one)),
	)

	# pattern.regex()
	assert pattern(
		conc(mult(charclass("a"), one)),
		conc(mult(charclass("b"), one)),
	).regex() == "a|b"
	assert pattern(
		conc(mult(charclass("a"), one)),
		conc(mult(charclass("a"), one)),
	).regex() == "a"
	assert pattern(
		conc(
			mult(charclass("a"), one),
			mult(charclass("b"), one),
			mult(charclass("c"), one),
		),
		conc(
			mult(charclass("d"), one),
			mult(charclass("e"), one),
			mult(charclass("f"), one),
			mult(
				pattern(
					conc(
						mult(charclass("g"), one),
						mult(charclass("h"), one),
						mult(charclass("i"), one),
					),
					conc(
						mult(charclass("j"), one),
						mult(charclass("k"), one),
						mult(charclass("l"), one),
					),
				), one
			),
		),
	).regex() == "abc|def(ghi|jkl)"

	# pattern.reduce() tests

	# pattern -> pattern
	# (ab|cd) -> (ab|cd)
	assert pattern(
		conc(
			mult(charclass("a"), multiplier(2, 2)),
			mult(charclass("b"), multiplier(2, 2)),
		),
		conc(
			mult(charclass("c"), multiplier(2, 2)),
			mult(charclass("d"), multiplier(2, 2)),
		),
	).reduce() == pattern(
		conc(
			mult(charclass("a"), multiplier(2, 2)),
			mult(charclass("b"), multiplier(2, 2)),
		),
		conc(
			mult(charclass("c"), multiplier(2, 2)),
			mult(charclass("d"), multiplier(2, 2)),
		),
	)

	# pattern -> conc
	assert pattern(
		conc(
			mult(charclass("a"), multiplier(2, 2)),
			mult(charclass("b"), multiplier(2, 2)),
		),
	).reduce() == conc(
		mult(charclass("a"), multiplier(2, 2)),
		mult(charclass("b"), multiplier(2, 2)),
	)

	# pattern -> mult
	assert pattern(
		conc(
			mult(charclass("a"), multiplier(2, 2)),
		),
	).reduce() == mult(charclass("a"), multiplier(2, 2))

	# pattern -> charclass
	assert pattern(
		conc(
			mult(charclass("a"), one),
		),
	).reduce() == charclass("a")

	# special pattern reduction technique.
	# 0|[1-9]|a{5,7} -> [0-9]|a{5,7}
	assert pattern(
		conc(mult(charclass("0"), one)),
		conc(mult(charclass("123456789"), one)),
		conc(mult(charclass("a"), multiplier(5, 7))),
	).reduce() == pattern(
		conc(mult(charclass("0123456789"), one)),
		conc(mult(charclass("a"), multiplier(5, 7))),
	)
	assert pattern(
		conc(mult(charclass("0"), star)),
		conc(mult(charclass("123456789"), star)),
		conc(mult(charclass("a"), multiplier(5, 7))),
	).reduce() == pattern(
		conc(mult(charclass("0123456789"), star)),
		conc(mult(charclass("a"), multiplier(5, 7))),
	)
	assert pattern(
		conc(mult(charclass("0"), star)),
		conc(mult(charclass("123456789"), star)),
		conc(mult(charclass("a"), plus)),
		conc(mult(charclass("b"), plus)),
	).reduce() == pattern(
		conc(mult(charclass("0123456789"), star)),
		conc(mult(charclass("ab"), plus)),
	)

	# recursive pattern reduction
	assert pattern(
		conc(mult(charclass("0"), one)),
		conc(
			mult(
				pattern(
					conc(mult(charclass("0"), one)),
					conc(mult(charclass("123456789"), one)),
					conc(mult(charclass("a"), multiplier(5, 7))),
				), one
			)
		)
	).reduce() == pattern(
		conc(mult(charclass("0"), one)),
		conc(
			mult(
				pattern(
					conc(mult(charclass("0123456789"), one)),
					conc(mult(charclass("a"), multiplier(5, 7))),
				), one
			)
		)
	)

#	assert pattern(
#		conc(
#			mult(charclass("a"), multiplier(1, 1)),
#			mult(charclass("a"), multiplier(1, 1))
#		),
#		conc(
#			mult(charclass("a"), multiplier(1, 1)),
#			mult(charclass("b"), multiplier(1, 1)),
#			mult(charclass("c"), multiplier(1, 1))
#		)
#	).commonprefix() == mult(charclass("a"), multiplier(1, 1))
#
#	assert pattern(
#		conc(
#			mult(charclass("a"), multiplier(2, 2)),
#			mult(charclass("b"), one),
#		),
#		conc(
#			mult(charclass("a"), plus),
#			mult(charclass("c"), one),
#		)
#	).commonprefix() == mult(charclass("a"), one)

	# common prefix reduction of pattern
	# a{2}b|a+c -> a{2}(ab|a*c)
	# TODO: FIX THIS.
	assert pattern(
		conc(
			mult(charclass("a"), multiplier(2, 2)),
			mult(charclass("b"), one),
		),
		conc(
			mult(charclass("a"), plus),
			mult(charclass("c"), one),
		)
	).reduce() == conc(
		mult(charclass("a"), one),
		mult(
			pattern(
				conc(
					mult(charclass("a"), one),
					mult(charclass("b"), one),
				),
				conc(
					mult(charclass("a"), star),
					mult(charclass("c"), one),
				),
			), one
		)
	)

	# pattern parsing
	assert pattern.match("abc|def(ghi|jkl)", 0) == (
		pattern(
			conc(
				mult(charclass("a"), one),
				mult(charclass("b"), one),
				mult(charclass("c"), one),
			),
			conc(
				mult(charclass("d"), one),
				mult(charclass("e"), one),
				mult(charclass("f"), one),
				mult(
					pattern(
						conc(
							mult(charclass("g"), one),
							mult(charclass("h"), one),
							mult(charclass("i"), one),
						),
						conc(
							mult(charclass("j"), one),
							mult(charclass("k"), one),
							mult(charclass("l"), one),
						),
					), one
				),
			)
		), 16
	)

	# charclass multiplication
	# a * 1 = a
	assert charclass("a") * one == charclass("a")
	# a * {1,3} = a{1,3}
	assert charclass("a") * multiplier(1, 3) == mult(charclass("a"), multiplier(1, 3))
	# a * {4,} = a{4,}
	assert charclass("a") * multiplier(4, None) == mult(charclass("a"), multiplier(4, None))

	# mult multiplication
	# a{2,3} * 1 = a{2,3}
	assert mult(
		charclass("a"), multiplier(2, 3)
	) * one == mult(charclass("a"), multiplier(2, 3))
	# a{2,3} * {4,5} = a{8,15}
	assert mult(
		charclass("a"), multiplier(2, 3)
	) * multiplier(4, 5) == mult(charclass("a"), multiplier(8, 15))
	# a{2,} * {2,None} = a{4,}
	assert mult(
		charclass("a"), multiplier(2, None)
	) * multiplier(2, None) == mult(charclass("a"), multiplier(4, None))

	# conc multiplication
	# ab? * {0,1} = (ab?)?
	assert conc(
		mult(charclass("a"), one),
		mult(charclass("b"), qm),
	) * qm == mult(
		pattern(
			conc(
				mult(charclass("a"), one),
				mult(charclass("b"), qm),
			),
		), qm
	)

	# pattern multiplication
	# (ab?|ba?) * {2,3} = (ab?|ba?){2,3}
	assert pattern(
		conc(
			mult(charclass("a"), one),
			mult(charclass("b"), qm),
		),
		conc(
			mult(charclass("b"), one),
			mult(charclass("a"), qm),
		),
	) * multiplier(2, 3) == mult(
		pattern(
			conc(
				mult(charclass("a"), one),
				mult(charclass("b"), qm),
			),
			conc(
				mult(charclass("b"), one),
				mult(charclass("a"), qm),
			),
		), multiplier(2, 3)
	)

	# multiplier & ("intersection") operator tests
	assert zero & zero == zero
	assert zero & qm   == zero
	assert zero & one  == zero
	assert zero & star == zero
	assert zero & plus == zero
	assert zero & inf  == zero
	assert qm   & zero == zero
	assert qm   & qm   == qm
	assert qm   & one  == zero
	assert qm   & star == qm
	assert qm   & plus == qm
	assert qm   & inf  == qm
	assert one  & zero == zero
	assert one  & qm   == zero
	assert one  & one  == one
	assert one  & star == zero
	assert one  & plus == one
	assert one  & inf  == one
	assert star & zero == zero
	assert star & qm   == qm
	assert star & one  == zero
	assert star & star == star
	assert star & plus == star
	assert star & inf  == star
	assert plus & zero == zero
	assert plus & qm   == qm
	assert plus & one  == one
	assert plus & star == star
	assert plus & plus == plus
	assert plus & inf  == plus
	assert inf  & zero == zero
	assert inf  & qm   == qm
	assert inf  & one  == one
	assert inf  & star == star
	assert inf  & plus == plus
	assert inf  & inf  == inf

	# a{3,4}, a{2,5} -> a{2,3} (with a{1,1}, a{0,2} left over)
	assert multiplier(3, 4) & multiplier(2, 5) == multiplier(2, 3)

	# a{2,}, a{1,5} -> a{1,5} (with a{1,}, a{0,0} left over)
	assert multiplier(2, None) & multiplier(1, 5) == multiplier(1, 5)

	# a{3,}, a{2,} -> a{2,} (with a, epsilon left over)
	assert multiplier(3, None) & multiplier(2, None) == multiplier(2, None)

	# a{3,}, a{3,} -> a{3,} (with None, None left over)
	assert multiplier(3, None) & multiplier(3, None) == multiplier(3, None)

	# mult intersection ("&") tests

	# a & b -> no intersection
	try:
		mult(charclass("a"), one) & mult(charclass("b"), one)
		assert(False)
	except NoCommonMultiplicandException:
		pass

	# a & a -> a
	assert mult(charclass("a"), one) & mult(charclass("a"), one) == mult(charclass("a"), one)

	# a{3,4} & a{2,5} -> a{2,3}
	assert mult(
		charclass("a"), multiplier(3, 4)
	) & mult(
		charclass("a"), multiplier(2, 5)
	) == mult(charclass("a"), multiplier(2, 3))

	# a{2,} & a{1,5} -> a{1,5}
	assert mult(
		charclass("a"), multiplier(2, None)
	) & mult(
		charclass("a"), multiplier(1, 5)
	) == mult(charclass("a"), multiplier(1, 5))

	# a{3,}, a{2,} -> a{2,} (with a, epsilon left over)
	assert mult(
		charclass("a"), multiplier(3, None)
	) & mult(
		charclass("a"), multiplier(2, None)
	) == mult(charclass("a"), multiplier(2, None))

	# a{3,}, a{3,} -> a{3,} (with None, None left over)
	assert mult(
		charclass("a"), multiplier(3, None)
	) & mult(
		charclass("a"), multiplier(3, None)
	) == mult(charclass("a"), multiplier(3, None))

	# pattern._concsuffix() tests

	# a | bc -> (a|bc)
	assert pattern(
		conc(
			mult(charclass("a"), one),
		),
		conc(
			mult(charclass("b"), one),
			mult(charclass("c"), one),
		),
	)._concsuffix() == conc(
		mult(
			pattern(
				conc(
					mult(charclass("a"), one),
				),
				conc(
					mult(charclass("b"), one),
					mult(charclass("c"), one),
				),
			), one
		),
	)

	# aa, bca -> (a|bc)a
	assert pattern(
		conc(
			mult(charclass("a"), one),
			mult(charclass("a"), one),
		),
		conc(
			mult(charclass("b"), one),
			mult(charclass("c"), one),
			mult(charclass("a"), one),
		),
	)._concsuffix() == conc(
		mult(
			pattern(
				conc(
					mult(charclass("a"), one),
				),
				conc(
					mult(charclass("b"), one),
					mult(charclass("c"), one),
				),
			), one,
		),
		mult(charclass("a"), one),
	)

	# xyza | abca | a -> (xyz|abc|)a -> (xyz|abc)?a
	assert pattern(
		conc(
			mult(charclass("x"), one),
			mult(charclass("y"), one),
			mult(charclass("z"), one),
			mult(charclass("a"), one),
		),
		conc(
			mult(charclass("a"), one),
			mult(charclass("b"), one),
			mult(charclass("c"), one),
			mult(charclass("a"), one),
		),
		conc(
			mult(charclass("a"), one),
		),
	)._concsuffix() == conc(
		mult(
			pattern(
				conc(
					mult(charclass("x"), one),
					mult(charclass("y"), one),
					mult(charclass("z"), one),
				),
				conc(
					mult(charclass("a"), one),
					mult(charclass("b"), one),
					mult(charclass("c"), one),
				),
			), qm
		),
		mult(charclass("a"), one),
	)

	# f{2,3}c, fc -> (f{1,2}|)fc -> (f{1,2})?fc
	assert pattern(
		conc(
			mult(charclass("f"), multiplier(2, 3)),
			mult(charclass("c"), one),
		),
		conc(
			mult(charclass("f"), one),
			mult(charclass("c"), one),
		),
	)._concsuffix() == conc(
		mult(
			pattern(
				conc(
					mult(charclass("f"), multiplier(1, 2)),
				),
			), qm
		),
		mult(charclass("f"), one),
		mult(charclass("c"), one),
	)

	# e | axe -> "(|ax)e" -> "(ax)?e"
	assert pattern(
		conc(
			mult(charclass("e"), one),
		),
		conc(
			mult(charclass("a"), one),
			mult(charclass("x"), one),
			mult(charclass("e"), one),
		),
	)._concsuffix() == conc(
		mult(
			pattern(
				conc(
					mult(charclass("a"), one),
					mult(charclass("x"), one),
				),
			), qm
		),
		mult(charclass("e"), one),
	)

	# aa | aa -> ()aa -> aa
	assert pattern(
		conc(
			mult(charclass("a"), one),
			mult(charclass("a"), one),
		),
		conc(
			mult(charclass("a"), one),
			mult(charclass("a"), one),
		),
	)._concsuffix() == conc(
		mult(charclass("a"), one),
		mult(charclass("a"), one),
	)

#	# _concprefix() tests
#
#	# aa, aa -> aa() -> aa
#	print(pattern(
#		conc(
#			mult(charclass("a"), one),
#			mult(charclass("a"), one),
#		),
#		conc(
#			mult(charclass("a"), one),
#			mult(charclass("a"), one),
#		),
#	)._concprefix())
#	assert pattern(
#		conc(
#			mult(charclass("a"), one),
#			mult(charclass("a"), one),
#		),
#		conc(
#			mult(charclass("a"), one),
#			mult(charclass("a"), one),
#		),
#	)._concprefix() == conc(
#		mult(charclass("a"), one),
#		mult(charclass("a"), one),
#	)
#
#	# abc, aa -> a(a|bc)
#	assert pattern(
#		conc(
#			mult(charclass("a"), one),
#			mult(charclass("b"), one),
#			mult(charclass("c"), one),
#		),
#		conc(
#			mult(charclass("a"), one),
#			mult(charclass("a"), one),
#		),
#	)._concprefix() == conc(
#		mult(charclass("a"), one),
#		mult(
#			pattern(
#				conc(
#					mult(charclass("a"), one),
#				),
#				conc(
#					mult(charclass("b"), one),
#					mult(charclass("c"), one),
#				),
#			), one,
#		),
#	)
#
#	# a, bc -> (a|bc)
#	assert pattern(
#		conc(
#			mult(charclass("a"), one),
#		),
#		conc(
#			mult(charclass("b"), one),
#			mult(charclass("c"), one),
#		),
#	)._concprefix() == conc(
#		mult(
#			pattern(
#				conc(
#					mult(charclass("a"), one),
#				),
#				conc(
#					mult(charclass("b"), one),
#					mult(charclass("c"), one),
#				),
#			), one
#		),
#	)
#
#	# cf{1,2}, cf -> cf(f?|) -> cf(f?)?
#	assert pattern(
#		conc(
#			mult(charclass("c"), one),
#			mult(charclass("f"), multiplier(1, 2)),
#		),
#		conc(
#			mult(charclass("c"), one),
#			mult(charclass("f"), one),
#		),
#	)._concprefix() == conc(
#		mult(charclass("c"), one),
#		mult(charclass("f"), one),
#		mult(
#			pattern(
#				conc(
#					mult(charclass("f"), qm)
#				)
#			), qm
#		),
#	)

	# concatenation tests (__add__())

	# empty conc + empty conc
	assert emptystring + emptystring == emptystring

	# charclass + charclass
	# a + b = ab
	assert charclass("a") + charclass("b") == conc(
		mult(charclass("a"), one),
		mult(charclass("b"), one),
	)
	# a + a = a{2}
	assert charclass("a") + charclass("a") == mult(charclass("a"), multiplier(2, 2))

	# charclass + mult
	# a + a = a{2}
	assert charclass("a") + mult(charclass("a"), one) == mult(charclass("a"), multiplier(2, 2))
	# a + a{2,} = a{3,}
	assert charclass("a") + mult(charclass("a"), multiplier(2, None)) == mult(charclass("a"), multiplier(3, None))
	# a + a{,8} = a{1,9}
	assert charclass("a") + mult(charclass("a"), multiplier(0, 8)) == mult(charclass("a"), multiplier(1, 9))
	# a + b{,8} = ab{,8}
	assert charclass("a") + mult(charclass("b"), multiplier(0, 8)) == conc(
		mult(charclass("a"), one),
		mult(charclass("b"), multiplier(0, 8)),
	)

	# mult + charclass
	# b + b = b{2}
	assert mult(charclass("b"), one) + charclass("b") == mult(charclass("b"), multiplier(2, 2))
	# b* + b = b+
	assert mult(charclass("b"), star) + charclass("b") == mult(charclass("b"), plus)
	 # b{,8} + b = b{1,9}
	assert mult(charclass("b"), multiplier(0, 8)) + charclass("b") == mult(charclass("b"), multiplier(1, 9))
	# b{,8} + c = b{,8}c
	assert mult(charclass("b"), multiplier(0, 8)) + charclass("c") == conc(
		mult(charclass("b"), multiplier(0, 8)),
		mult(charclass("c"), one),
	)

	# charclass + conc
	# a + nothing = a
	assert charclass("a") + emptystring == charclass("a")
	# a + bc = abc
	assert charclass("a") + conc(
		mult(charclass("b"), one),
		mult(charclass("c"), one),
	) == conc(
		mult(charclass("a"), one),
		mult(charclass("b"), one),
		mult(charclass("c"), one),
	)
	# a + ab = a{2}b
	assert charclass("a") + conc(
		mult(charclass("a"), one),
		mult(charclass("b"), one),
	) == conc(
		mult(charclass("a"), multiplier(2, 2)),
		mult(charclass("b"), one),
	)

	# conc + charclass
	# nothing + a = a
	assert emptystring + charclass("a") == charclass("a")
	# ab + c = abc
	assert conc(
		mult(charclass("a"), one),
		mult(charclass("b"), one),
	) + charclass("c") == conc(
		mult(charclass("a"), one),
		mult(charclass("b"), one),
		mult(charclass("c"), one),
	)
	# ab + b = ab{2}
	assert conc(
		mult(charclass("a"), one),
		mult(charclass("b"), one),
	) + charclass("b") == conc(
		mult(charclass("a"), one),
		mult(charclass("b"), multiplier(2, 2)),
	)

	# pattern + charclass
	# (a|bd) + c = (a|bd)c
	assert pattern(
		conc(
			mult(charclass("a"), one),
		),
		conc(
			mult(charclass("b"), one),
			mult(charclass("d"), one),
		),
	) + charclass("c") == conc(
		mult(
			pattern(
				conc(
					mult(charclass("a"), one),
				),
				conc(
					mult(charclass("b"), one),
					mult(charclass("d"), one),
				),
			), one
		),
		mult(charclass("c"), one),
	)
	# (ac{2}|bc+) + c = (ac|bc*)c{2}
	assert pattern(
		conc(
			mult(charclass("a"), one),
			mult(charclass("c"), multiplier(2, 2)),
		),
		conc(
			mult(charclass("b"), one),
			mult(charclass("c"), plus),
		),
	) + charclass("c") == conc(
		mult(
			pattern(
				conc(
					mult(charclass("a"), one),
					mult(charclass("c"), one),
				),
				conc(
					mult(charclass("b"), one),
					mult(charclass("c"), star),
				),
			), one
		),
		mult(charclass("c"), multiplier(2, 2)),
	)

	# charclass + pattern
	# a + (b|cd) = a(b|cd)
	assert charclass("a") + pattern(
		conc(
			mult(charclass("b"), one),
		),
		conc(
			mult(charclass("c"), one),
			mult(charclass("d"), one),
		),
	) == conc(
		mult(charclass("a"), one),
		mult(
			pattern(
				conc(
					mult(charclass("b"), one),
				),
				conc(
					mult(charclass("c"), one),
					mult(charclass("d"), one),
				),
			), one
		)
	)
	# a + (a{2}b|a+c) = a{2}(ab|a*c)
	assert charclass("a") + pattern(
		conc(
			mult(charclass("a"), multiplier(2, 2)),
			mult(charclass("b"), one),
		),
		conc(
			mult(charclass("a"), plus),
			mult(charclass("c"), one),
		),
	) == conc(
		mult(charclass("a"), multiplier(2, 2)),
		mult(
			pattern(
				conc(
					mult(charclass("a"), one),
					mult(charclass("b"), one),
				),
				conc(
					mult(charclass("a"), star),
					mult(charclass("c"), one),
				),
			), one
		),
	)

	# mult + mult
	# a{3,4} + b? = a{3,4}b?
	assert mult(charclass("a"), multiplier(3, 4)) + mult(charclass("b"), qm) == conc(
		mult(charclass("a"), multiplier(3, 4)),
		mult(charclass("b"), qm),
	)
	# a* + a{2} = a{2,}
	assert mult(charclass("a"), star) + mult(charclass("a"), multiplier(2, 2)) == mult(charclass("a"), multiplier(2, None))

	# mult + conc
	# a{2} + bc = a{2}bc
	assert mult(charclass("a"), multiplier(2, 2)) + conc(
		mult(charclass("b"), one),
		mult(charclass("c"), one),
	) == conc(
		mult(charclass("a"), multiplier(2, 2)),
		mult(charclass("b"), one),
		mult(charclass("c"), one),
	)
	# a? + ab = a{1,2}b
	assert mult(charclass("a"), qm) + conc(
		mult(charclass("a"), one),
		mult(charclass("b"), one),
	) == conc(
		mult(charclass("a"), multiplier(1, 2)),
		mult(charclass("b"), one),
	)

	# conc + mult
	# ab + c* = abc*
	assert conc(
		mult(charclass("a"), one),
		mult(charclass("b"), one),
	) + mult(charclass("c"), star) == conc(
		mult(charclass("a"), one),
		mult(charclass("b"), one),
		mult(charclass("c"), star),
	)
	# ab + b* = ab+
	assert conc(
		mult(charclass("a"), one),
		mult(charclass("b"), one),
	) + mult(charclass("b"), star) == conc(
		mult(charclass("a"), one),
		mult(charclass("b"), plus),
	)

	# mult + pattern
	# a{2,3} + (b|cd) = a{2,3}(b|cd)
	assert mult(charclass("a"), multiplier(2, 3)) + pattern(
		conc(
			mult(charclass("b"), one),
		),
		conc(
			mult(charclass("c"), one),
			mult(charclass("d"), one),
		),
	) == conc(
		mult(charclass("a"), multiplier(2, 3)),
		mult(
			pattern(
				conc(
					mult(charclass("b"), one),
				),
				conc(
					mult(charclass("c"), one),
					mult(charclass("d"), one),
				),
			), one
		)
	)
	# a{2,3} + (a{2}b|a+c) = a{3,4}(ab|a*c)
	assert mult(charclass("a"), multiplier(2, 3)) + pattern(
		conc(
			mult(charclass("a"), multiplier(2, 2)),
			mult(charclass("b"), one),
		),
		conc(
			mult(charclass("a"), plus),
			mult(charclass("c"), one),
		),
	) == conc(
		mult(charclass("a"), multiplier(3, 4)),
		mult(
			pattern(
				conc(
					mult(charclass("a"), one),
					mult(charclass("b"), one),
				),
				conc(
					mult(charclass("a"), star),
					mult(charclass("c"), one),
				),
			), one
		),
	)

	# pattern + mult
	# (b|cd) + a{2,3} = (b|cd)a{2,3}
	assert pattern(
		conc(
			mult(charclass("b"), one),
		),
		conc(
			mult(charclass("c"), one),
			mult(charclass("d"), one),
		),
	) + mult(charclass("a"), multiplier(2, 3)) == conc(
		mult(
			pattern(
				conc(
					mult(charclass("b"), one),
				),
				conc(
					mult(charclass("c"), one),
					mult(charclass("d"), one),
				),
			), one
		),
		mult(charclass("a"), multiplier(2, 3)),
	)
	# (ba{2}|ca+) + a{2,3} = (ba|ca*)a{3,4}
	assert pattern(
		conc(
			mult(charclass("b"), one),
			mult(charclass("a"), multiplier(2, 2)),
		),
		conc(
			mult(charclass("c"), one),
			mult(charclass("a"), plus),
		),
	) + mult(charclass("a"), multiplier(2, 3)) == conc(
		mult(
			pattern(
				conc(
					mult(charclass("b"), one),
					mult(charclass("a"), one),
				),
				conc(
					mult(charclass("c"), one),
					mult(charclass("a"), star),
				),
			), one
		),
		mult(charclass("a"), multiplier(3, 4)),
	)

	# conc + conc
	# ab + cd = abcd
	assert conc(
		mult(charclass("a"), one),
		mult(charclass("b"), one),
	) + conc(
		mult(charclass("c"), one),
		mult(charclass("d"), one),
	) == conc(
		mult(charclass("a"), one),
		mult(charclass("b"), one),
		mult(charclass("c"), one),
		mult(charclass("d"), one),
	)
	# ab + bc = ab{2}c
	assert conc(
		mult(charclass("a"), one),
		mult(charclass("b"), one),
	) + conc(
		mult(charclass("b"), one),
		mult(charclass("c"), one),
	) == conc(
		mult(charclass("a"), one),
		mult(charclass("b"), multiplier(2, 2)),
		mult(charclass("c"), one),
	)

	# conc + pattern
	# za{2,3} + (b|cd) = za{2,3}(b|cd)
	assert conc(
		mult(charclass("z"), one),
		mult(charclass("a"), multiplier(2, 3)),
	) + pattern(
		conc(
			mult(charclass("b"), one),
		),
		conc(
			mult(charclass("c"), one),
			mult(charclass("d"), one),
		),
	) == conc(
		mult(charclass("z"), one),
		mult(charclass("a"), multiplier(2, 3)),
		mult(
			pattern(
				conc(
					mult(charclass("b"), one),
				),
				conc(
					mult(charclass("c"), one),
					mult(charclass("d"), one),
				),
			), one,
		)
	)
	# za{2,3} + (a{2}b|a+c) = za{3,4}(ab|a*c)
	assert conc(
		mult(charclass("z"), one),
		mult(charclass("a"), multiplier(2, 3)),
	) + pattern(
		conc(
			mult(charclass("a"), multiplier(2, 2)),
			mult(charclass("b"), one),
		),
		conc(
			mult(charclass("a"), plus),
			mult(charclass("c"), one),
		),
	) == conc(
		mult(charclass("z"), one),
		mult(charclass("a"), multiplier(3, 4)),
		mult(
			pattern(
				conc(
					mult(charclass("a"), one),
					mult(charclass("b"), one),
				),
				conc(
					mult(charclass("a"), star),
					mult(charclass("c"), one),
				),
			), one
		),
	)

	# pattern + conc
	# (b|cd) + za{2,3} = (b|cd)za{2,3}
	assert pattern(
		conc(
			mult(charclass("b"), one),
		),
		conc(
			mult(charclass("c"), one),
			mult(charclass("d"), one),
		),
	) + conc(
		mult(charclass("z"), one),
		mult(charclass("a"), multiplier(2, 3)),
	) == conc(
		mult(
			pattern(
				conc(
					mult(charclass("b"), one),
				),
				conc(
					mult(charclass("c"), one),
					mult(charclass("d"), one),
				),
			), one
		),
		mult(charclass("z"), one),
		mult(charclass("a"), multiplier(2, 3)),
	)
	# (ba{2}|ca+) + a{2,3}z = (ba|ca*)a{3,4}z
	assert pattern(
		conc(
			mult(charclass("b"), one),
			mult(charclass("a"), multiplier(2, 2)),
		),
		conc(
			mult(charclass("c"), one),
			mult(charclass("a"), plus),
		),
	) + conc(
		mult(charclass("a"), multiplier(2, 3)),
		mult(charclass("z"), one),
	) == conc(
		mult(
			pattern(
				conc(
					mult(charclass("b"), one),
					mult(charclass("a"), one),
				),
				conc(
					mult(charclass("c"), one),
					mult(charclass("a"), star),
				),
			), one
		),
		mult(charclass("a"), multiplier(3, 4)),
		mult(charclass("z"), one),
	)

	# pattern + pattern
	# (a|bc) + (c|de) = (a|bc)(c|de)
	assert pattern(
		conc(
			mult(charclass("a"), one),
		),
		conc(
			mult(charclass("b"), one),
			mult(charclass("c"), one),
		),
	) + pattern(
		conc(
			mult(charclass("c"), one),
		),
		conc(
			mult(charclass("d"), one),
			mult(charclass("e"), one),
		),
	) == conc(
		mult(
			pattern(
				conc(
					mult(charclass("a"), one),
				),
				conc(
					mult(charclass("b"), one),
					mult(charclass("c"), one),
				),
			), one
		),
		mult(
			pattern(
				conc(
					mult(charclass("c"), one),
				),
				conc(
					mult(charclass("d"), one),
					mult(charclass("e"), one),
				),
			), one
		),
	)
	# (a|bc) + (a|bc) = (a|b){2}
	assert pattern(
		conc(
			mult(charclass("a"), one),
		),
		conc(
			mult(charclass("b"), one),
			mult(charclass("c"), one),
		),
	) + pattern(
		conc(
			mult(charclass("a"), one),
		),
		conc(
			mult(charclass("b"), one),
			mult(charclass("c"), one),
		),
	) == mult(
		pattern(
			conc(
				mult(charclass("a"), one),
			),
			conc(
				mult(charclass("b"), one),
				mult(charclass("c"), one),
			),
		), multiplier(2, 2)
	)

	print("OK")

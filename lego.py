# Copyright (C) 2012 by Sam Hughes

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

'''
	LEGO:
	Classes and methods for the creation and manipulation of regular expression
	objects and components.

	* A regular expression is a "pattern" object.
	* Each pattern alternates (with a pipe, "|") between zero or more "conc"
	(concatenation) objects.
	* Each conc is a concatenation of zero or more "mult" (multiplication)
	objects.
	* Each mult consists of a multiplicand and a multiplier. A multiplier consists
	of a minimum and a maximum, e.g. min = 0, max = 1 indicates the "?"
	multiplier. The multiplicand is either a nested pattern object, or a
	charclass object.
	* A charclass is a set of chars, such as "a", "[a-z]", "\\d", ".", with a
	possible "negated" flag as in "[^a]".
	* Since these can be combined together freely they are, in the absence of a
	better metaphor, collectively referred to as lego pieces.

	We also include methods for parsing a string into a pattern object,
	serialising a pattern object out as a string (or "regular expression", if you
	will), and for concatenating or alternating between arbitrary "pieces of
	lego", using overloaded operators.

	If the FSM module is available, call lego.fsm() on any lego piece to return
	a finite state machine capable of accepting strings described by that piece.

	Most important are the reduce() methods present in charclass, mult, conc and
	pattern. While there is no such thing as a canonical form for a given regex
	pattern, these procedures can drastically simplify a regex structure for
	readability. They're also pretty easy to extend.
'''

# http://qntm.org/lego
# http://qntm.org/greenery

# TODO: use charclass() instead of pattern() to represent "nothing". The empty
# character class can actually be printed as "[]", unlike the empty pattern.

class MatchFailureException(Exception):
	'''Thrown when parsing fails. Almost always caught and almost never fatal'''
	pass

class lego:
	'''
		Parent class for all lego pieces.
		All lego pieces have some things in common. This parent class mainly
		hosts documentation though.
	'''

	def __setattr__(self, name, value):
		'''
			Lego pieces are immutable. It caused some pretty serious problems when
			I didn't have this.
		'''
		raise Exception("Can't set " + str(self) + " attribute " + str(name) + " to " + str(value))

	def fsm(self, alphabet):
		'''
			Return the present lego piece in the form of a finite state machine,
			as imported from the fsm module.
		'''
		pass

	def __repr__(self):
		'''
			Actually return a string approximating the instantiation line
			for the present lego piece. This is different from regex()
			in that it provides a lot more information for debug purposes.
			This gets called when you do str(lego).
		'''
		pass

	def regex(self):
		'''
			Render the present lego piece in the form of a regular expression.
			Some lego pieces may be created which cannot be rendered in this way.
			In particular: an empty charclass, a pattern containing no concs,
			a multiplier of zero.
		'''
		pass

	def match(cls, string, i):
		'''
			Start at index i in the supplied string and try to match one of the
			present class. Elementary recursive descent parsing with very little
			need for flair. The opposite of regex(), above. (In most cases.)
			Throws a MatchFailureException in the event of failure.
		'''
		pass

	def reduce(self):
		'''
			The most important and algorithmically complex method. Takes the current
			lego piece and simplifies it in every way possible, returning a simpler
			lego piece which is quite probably not of the same class as the original.
			Approaches vary by the class of the present lego piece.

			It is critically important to (1) always call reduce() on whatever you're
			returning before you return it and therefore (2) always return something
			STRICTLY SIMPLER than the current object. Otherwise, infinite loops become
			possible in reduce() calls.
		'''
		pass

	def __add__(self, other):
		'''
			Concatenate any two lego pieces, regardless of differing classes. Because
			reduce() (above) is always called afterwards, the result is as simplified
			as possible.
			Call using "a = b + c"
		'''
		pass

	def __mul__(self, multiplier):
		'''
			Equivalent to repeated concatenation. Multiplier consists of a minimum
			and a maximum; maximum may be infinite (for Kleene star closure).
			Call using "a = b * qm"
		'''
		pass

	def __or__(self, other):
		'''
			Alternate between any two lego pieces, regardless of differing classes.
			Again, reduce() is called afterwards, usually with excellent results.
			Call using "a = b | c"
		'''
		pass

	def __and__(self, other):
		'''
			Intersection function. Return a lego piece that can match any string
			that both self and other can match. Fairly elementary results relating
			to regular languages and finite state machines show that this is
			possible, but implementation is a BEAST in most cases. Here, we convert
			both lego pieces to FSMs (see fsm(), above) for the intersection, then
			back to lego afterwards.
			Call using "a = b & c"
		'''
		pass

	def alphabet(self):
		'''
			Return a set of all unique characters used in this lego piece.
			In theory this could be a static property, self.alphabet, not
			a function, self.alphabet(), but in the vast majority of cases
			this will never be queried so it's a waste of computation to
			calculate it every time a lego piece is instantiated.
		'''
		pass

	@classmethod
	def matchStatic(cls, string, i, static):
		if string[i:len(static)+i] == static:
			return i+len(static)
		raise MatchFailureException(
			"Can't find '" + static + "' at index " + str(i) + " in '" + string + "'"
		)

	@classmethod
	def matchAny(cls, string, i, collection=None):
		if collection is None:
			if i >= len(string):
				raise MatchFailureException
			return string[i], i+1
		else:
			for char in collection:
				try:
					return char, cls.matchStatic(string, i, char)
				except MatchFailureException:
					pass
			raise MatchFailureException("Can't find any of '" + str(collection) + "' at index " + str(i) + " in '" + string + "'")

class NoRegexException(Exception):
	'''
		This occurs if you try to regex() something which can't be printed, such
		as an empty charclass, a zero or infinite multiplier, an empty pattern
	'''
	pass

class charclass(lego):
	'''
		A charclass is basically a frozenset of symbols. The reason for the
		charclass object instead of using frozenset directly is to allow us to
		set a "negated" flag. A charclass with the negation flag set is assumed
		to contain every symbol that is in the alphabet of all symbols but not
		explicitly listed inside the frozenset. e.g. [^a]. This is very handy
		if the full alphabet is extremely large, but also requires dedicated
		combination functions.
	'''

	def __init__(self, chars=set(), negateMe=False):
		# chars should consist only of chars
		if otherchars in set(chars):
			raise Exception("Can't put non-character 'otherchars' in a charclass")
		self.__dict__["chars"]   = frozenset(chars)
		self.__dict__["negated"] = negateMe

	def __eq__(self, other):
		return type(other) == type(self) \
		and self.chars == other.chars \
		and self.negated == other.negated

	def __hash__(self):
		return tuple.__hash__((self.chars, self.negated))

	def __mul__(self, multiplier):
		# e.g. "a" * {0,1} = "a?"
		if multiplier == one:
			return self
		return mult(self, multiplier)

	def regex(self):
		# e.g. \w
		if self in shorthand.keys():
			return shorthand[self]

		# i.e. charclass()
		if len(self.chars) < 1:
			raise NoRegexException("Can't print an empty charclass")

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
				output += escapeChar(currentRange[0]) + "-" + escapeChar(currentRange[-1])

			currentRange = ""

		output = ""

		# use shorthand for known character ranges
		# note the nested processing order. DO NOT process \d before processing
		# \w. if more character class constants arise which do not nest nicely,
		# a problem will arise because there is no clear ordering to use...

		# look for ranges
		currentRange = ""
		for char in sorted(self.chars, key=str):

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

	def fsm(self, alphabet):
		from fsm import fsm
		# 0 is initial, 1 is final, 2 is oblivion

		# If negated, make a singular FSM accepting any other characters
		if self.negated:
			map = {
				0: dict([(symbol, 2 if symbol in self.chars else 1) for symbol in alphabet]),
				1: dict([(symbol, 2) for symbol in alphabet]),
				2: dict([(symbol, 2) for symbol in alphabet]),
			}
		
		# If normal, make a singular FSM accepting only these characters
		else:
			map = {
				0: dict([(symbol, 1 if symbol in self.chars else 2) for symbol in alphabet]),
				1: dict([(symbol, 2) for symbol in alphabet]),
				2: dict([(symbol, 2) for symbol in alphabet]),
			}

		return fsm(
			alphabet = alphabet,
			states   = {0, 1, 2},
			initial  = 0,
			finals   = {1},
			map      = map,
		)

	def __repr__(self):
		string = ""
		if self.negated is True:
			string += "~"
		string += "charclass(\""
		string += "".join(str(char) for char in sorted(self.chars, key=str))
		string += "\")"
		return string

	def reduce(self):
		# Charclasses cannot be reduced()
		return self

	def __add__(self, other):
		return mult(self, one) + other

	def alphabet(self):
		return self.chars

	@classmethod
	def match(cls, string, i):
		# wildcard ".", "\\w", "\\d", etc.
		for key in shorthand.keys():
			try:
				return key, cls.matchStatic(string, i, shorthand[key])
			except MatchFailureException:
				pass

		# "[^dsgsdg]"
		try:
			return cls.matchNegatedRange(string, i)
		except MatchFailureException:
			pass

		# "[sdfsf]"
		try:
			return cls.matchRange(string, i)
		except MatchFailureException:
			pass

		# e.g. if seeing "\\t", return "\t"
		for key in escapes.keys():
			try:
				return charclass(key), cls.matchStatic(string, i, escapes[key])
			except MatchFailureException:
				pass

		# e.g. if seeing "\\{", return "{"
		for char in allSpecial:
			try:
				return charclass(char), cls.matchStatic(string, i, "\\" + char)
			except MatchFailureException:
				pass

		# single non-special character, not contained inside square brackets
		char, i = cls.matchAny(string, i)
		if char in allSpecial:
			raise MatchFailureException

		return charclass(char), i

	@classmethod
	def matchNegatedRange(cls, string, i):
		i = cls.matchStatic(string, i, "[^")
		chars, i = cls.matchRangeInterior(string, i)
		i = cls.matchStatic(string, i, "]")
		return ~charclass(chars), i

	@classmethod
	def matchRange(cls, string, i):
		i = cls.matchStatic(string, i, "[")
		chars, i = cls.matchRangeInterior(string, i)
		i = cls.matchStatic(string, i, "]")
		return charclass(chars), i

	@classmethod
	def matchRangeInterior(cls, string, i):
		internals = ""
		try:
			while True:
				internal, i = cls.matchRangeInternal(string, i)
				internals += internal
		except MatchFailureException:
			pass
		return internals, i

	@classmethod
	def matchRangeInternal(cls, string, i):
		firstChar, i = cls.matchInternalChar(string, i)
		try:
			j = cls.matchStatic(string, i, "-")
			lastChar, j = cls.matchInternalChar(string, j)

			for allowableRange in allowableRanges:
				if firstChar in allowableRange:
					# first and last must be in the same character range
					if lastChar not in allowableRange:
						raise MatchFailureException("Char '" + lastChar + "' not allowed as end of range")

					firstIndex = allowableRange.index(firstChar)
					lastIndex = allowableRange.index(lastChar)

					# and in order i.e. a < b
					if firstIndex >= lastIndex:
						raise MatchFailureException(
							"Disordered range ('" + firstChar + "' !< '" + lastChar + "')"
						)

					# OK
					return allowableRange[firstIndex:lastIndex + 1], j

			raise MatchFailureException("Char '" + firstChar + "' not allowed as start of a range")
		except MatchFailureException:
			return firstChar, i

	@classmethod
	def matchInternalChar(cls, string, i):

		# e.g. if we see "\\t", return "\t"
		for key in escapes.keys():
			try:
				return key, cls.matchStatic(string, i, escapes[key])
			except MatchFailureException:
				pass

		# special chars e.g. "\\-" returns "-"
		for char in classSpecial:
			try:
				return char, cls.matchStatic(string, i, "\\" + char)
			except MatchFailureException:
				pass

		# single non-special character, not contained
		# inside square brackets
		char, j = cls.matchAny(string, i)
		if char in classSpecial:
			raise MatchFailureException

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
			else:
				output += escapeChar(currentRange[0]) + "-" + escapeChar(currentRange[-1])

			currentRange = ""

		output = ""

		# use shorthand for known character ranges
		# note the nested processing order. DO NOT process \d before processing
		# \w. if more character class constants arise which do not nest nicely,
		# a problem will arise because there is no clear ordering to use...

		# look for ranges
		currentRange = ""
		for char in sorted(self.chars, key=str):

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
	def __invert__(self):
		'''
			Negate the current charclass. e.g. [ab] becomes [^ab]. Call
			using "charclass2 = ~charclass1"
		'''
		return charclass(self.chars, negateMe=not self.negated)

	def __or__(self, other):
		if type(other) != charclass:
			return mult(self, one) | other

		# ¬A OR ¬B = ¬(A AND B)
		# ¬A OR B = ¬(A - B)
		# A OR ¬B = ¬(B - A)
		# A OR B
		if self.negated:
			if other.negated:
				return ~charclass(self.chars & other.chars)
			return ~charclass(self.chars - other.chars)
		if other.negated:
			return ~charclass(other.chars - self.chars)
		return charclass(self.chars | other.chars)

		raise Exception("What")

	def __sub__(self, other):
		'''Subtract B from A'''

		# ¬A - ¬B = B - A
		# ¬A - B = ¬(A OR B)
		# A - ¬B = A AND B
		# A - B
		if self.negated:
			if other.negated:
				return charclass(other.chars - self.chars)
			return ~charclass(self.chars | other.chars)
		if other.negated:
			return charclass(self.chars & other.chars)
		return charclass(self.chars - other.chars)

	def __and__(self, other):
		if type(other) != charclass:
			return mult(self, one) & other

		# ¬A AND ¬B = ¬(A OR B)
		# ¬A AND B = B - A
		# A AND ¬B = A - B
		# A AND B
		if self.negated:
			if other.negated:
				return ~charclass(self.chars | other.chars)
			return charclass(other.chars - self.chars)
		if other.negated:
			return charclass(self.chars - other.chars)
		return charclass(self.chars & other.chars)

	def issubset(self, other):
		'''Find out if A is a subset of B'''

		# ¬A < ¬B if B < A
		# ¬A < B is impossible
		# A < ¬B if A n B = 0
		# A < B
		if self.negated:
			if other.negated:
				return other.chars.issubset(self.chars)
			return False
		if other.negated:
			return self.chars.isdisjoint(other.chars)
		return self.chars.issubset(other.chars)

class bound:
	'''An integer but sometimes also possibly infinite (None)'''
	def __init__(self, v):
		if v is not None:
			if type(v) != int:
				raise Exception("Value must be None or an integer")
			if v < 0:
				raise Exception("Value must be >= 0 or None")

		self.__dict__['v'] = v

	def __repr__(self):
		return "bound(" + str(self.v) + ")"

	def regex(self):
		if self == infinity:
			# This only happens for an unlimited upper bound
			return ""
		return str(self.v)

	def __eq__(self, other):
		return type(self) == type(other) \
		and self.v == other.v

	def __hash__(self):
		return self.v.__hash__()

	def __lt__(self, other):
		if self == infinity:  
			return False
		if other == infinity:
			return True
		return self.v < other.v

	def __gt__(self, other):
		if other == infinity:
			return False
		if self == infinity:
			return True
		return self.v > other.v

	def __mul__(self, other):
		'''Multiply this bound by another'''
		if self == infinity or other == infinity:
			return infinity
		return bound(self.v * other.v)

	def __add__(self, other):
		'''Add this bound to another'''
		if self == infinity or other == infinity:
			return infinity
		return bound(self.v + other.v)

	def __sub__(self, other):
		'''
			Subtract another bound from this one.
			This is a bit of a nightmare.
			Caution: this operation is not meaningful for all bounds.
		'''
		if other == infinity:
			if self == infinity:
				# Infinity minus infinity is zero. This has to be true so that
				# we can for example subtract multiplier(0, infinity) from
				# multiplier(1, infinity) to get multiplier(1, 1)
				return bound(0)
			raise Exception("Can't subtract " + str(other) + " from " + str(self))
		if self == infinity:
			return self
		return bound(self.v - other.v)

	def __and__(self, other):
		'''
			Find the shared part of two bounds.
			This is a bit of a nightmare as well
		'''
		if self == infinity:
			return other
		if other == infinity:
			return self
		return bound(min(self.v, other.v))

class multiplier(lego):
	'''
		A min and a max. The vast majority of characters in regular
		expressions occur without a specific multiplier, which is implicitly
		equivalent to a min of 1 and a max of 1, but many more have explicit
		multipliers like "*" (min = 0, max = infinity) and so on.
		Although it seems odd and can lead to some confusing edge cases, we do
		also permit a max of 0 (iff min is 0 too). This allows the multiplier
		"zero" to exist, which actually are quite useful in their own special way.
	'''
	def __init__(self, min, max):
		if type(min) != bound:
			min = bound(min)
		if min == infinity:
			raise Exception("Can't have an infinite lower bound")
		if type(max) != bound:
			max = bound(max)
		if max < min:
			raise Exception("max '" + str(max) + "' must match or exceed min '" + str(min) + "'")

		# More useful than "min" and "max" in many situations
		# are "mandatory" and "optional".
		mandatory = min
		optional = max - min

		self.__dict__['min'] = min
		self.__dict__['max'] = max
		self.__dict__['mandatory'] = mandatory
		self.__dict__['optional'] = optional

	def __eq__(self, other):
		return type(self) == type(other) \
		and self.min == other.min \
		and self.max == other.max

	def __hash__(self):
		return tuple.__hash__((self.min, self.max))

	def __repr__(self):
		return "multiplier(" + str(self.min.regex()) + ", " + str(self.max.regex()) + ")"

	def regex(self):
		if self.max == bound(0):
			raise NoRegexException("No regex available for " + str(self))
		if self in symbolic.keys():
			return symbolic[self]
		if self.max == infinity:
			return "{" + self.min.regex() + ",}"
		if self.min == self.max:
			return "{" + self.min.regex() + "}"
		return "{" + self.min.regex() + "," + self.max.regex() + "}"

	@classmethod
	def match(cls, string, i):
		# {2,3}
		try:
			j = cls.matchStatic(string, i, "{")
			min, j = cls.matchInteger(string, j)
			j = cls.matchStatic(string, j, ",")
			max, j = cls.matchInteger(string, j)
			j = cls.matchStatic(string, j, "}")
			return multiplier(min, max), j
		except MatchFailureException:
			pass

		# {2,}
		try:
			j = cls.matchStatic(string, i, "{")
			min, j = cls.matchInteger(string, j)
			j = cls.matchStatic(string, j, ",}")
			return multiplier(min, infinity), j
		except MatchFailureException:
			pass

		# {2}
		try:
			j = cls.matchStatic(string, i, "{")
			min, j = cls.matchInteger(string, j)
			j = cls.matchStatic(string, j, "}")
			return multiplier(min, min), j
		except MatchFailureException:
			pass

		# "?"/"*"/"+"/""
		# we do these in reverse order of symbol length, because
		# that forces "" to be done last
		for key in sorted(symbolic, key=lambda key: -len(symbolic[key])):
			try:
				return key, cls.matchStatic(string, i, symbolic[key])
			except MatchFailureException:
				pass

		raise MatchFailureException

	@classmethod
	def matchInteger(cls, string, i):
		try:
			return 0, cls.matchStatic(string, i, "0")
		except MatchFailureException:
			pass

		digit, i = cls.matchAny(string, i, "123456789")
		integer = int(digit)
		try:
			while True:
				digit, i = cls.matchAny(string, i, "0123456789")
				integer *= 10
				integer += int(digit)
		except MatchFailureException:
			return integer, i

	def __mul__(self, other):
		'''Multiply this multiplier by another'''
		return multiplier(self.min * other.min, self.max * other.max)

	def __add__(self, other):
		'''Add two multipliers together'''
		return multiplier(self.min + other.min, self.max + other.max)

	def __sub__(self, other):
		'''
			Subtract another multiplier from this one.
			This is a bit of a nightmare.
			Caution: multipliers are not totally ordered.
			This operation is not meaningful for all pairs of multipliers.
		'''
		mandatory = self.mandatory - other.mandatory
		optional = self.optional - other.optional
		return multiplier(mandatory, mandatory + optional)

	def __and__(self, other):
		'''
			Find the shared part of two multipliers.
			This is a bit of a nightmare as well
		'''
		mandatory = self.mandatory & other.mandatory
		optional = self.optional & other.optional
		return multiplier(mandatory, mandatory + optional)

class NoCommonMultiplicandException(Exception):
	'''
		This happens when you try to intersect or subtract two mults which
		don't have a common multiplicand.
	'''

class mult(lego):
	'''
		A mult is a combination of a multiplicand (a charclass or subpattern) with
		a multiplier (a min and a max). The vast majority of characters in regular
		expressions occur without a specific multiplier, which is implicitly
		equivalent to a min of 1 and a max of 1, but many more have explicit
		multipliers like "*" (min = 0, max = infinity) and so on.

		e.g. a, b{2}, c?, d*, [efg]{2,5}, f{2,}, (anysubpattern)+, .*, and so on
	'''

	def __init__(self, cand, ier):
		if type(cand) not in {charclass, pattern}:
			raise Exception("Wrong type '" + str(type(cand)) + "' for multiplicand")
		if type(ier) != multiplier:
			raise Exception("Wrong type '" + str(type(ier)) + "' for multiplier")

		self.__dict__["multiplicand"] = cand
		self.__dict__["multiplier"]   = ier

	def __eq__(self, other):
		return type(self) == type(other) \
		and type(self.multiplicand) == type(other.multiplicand) \
		and self.multiplicand == other.multiplicand \
		and self.multiplier == other.multiplier

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
		return conc(self) + other

	def __or__(self, other):
		return conc(self) | other

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
		# TODO: is there a bug here?
		if type(other) != mult:
			return conc(self) & other

		if other.multiplicand != self.multiplicand:
			raise NoCommonMultiplicandException("Can't find intersection of " + str(other) + " with " + str(self))

		return mult(self.multiplicand, self.multiplier & other.multiplier)

	def alphabet(self):
		return self.multiplicand.alphabet()

	def reduce(self):
		# If our multiplicand is a pattern containing an empty conc()
		# we can pull that "optional" bit out into our own multiplier
		# instead.
		# e.g. (A|B|C|)D -> (A|B|C)?D
		# e.g. (A|B|C|){2} -> (A|B|C){0,2}
		if type(self.multiplicand) == pattern \
		and emptystring in self.multiplicand.concs:
			return mult(
				pattern(
					*self.multiplicand.concs.difference({emptystring})
				),
				self.multiplier * qm,
			).reduce()

		# If our multiplicand is an empty pattern or charclass, which
		# *can never match*, then we are lost, UNLESS it's possible
		# to match it zero times.
		if self.multiplicand == nothing \
		or self.multiplicand == charclass():
			if self.multiplier.min == bound(0):
				return emptystring.reduce()
			else:
				return nothing.reduce()

		# Failing that, we have a positive multiplicand which we
		# intend to match zero times. In this case the only possible
		# match is the empty string.
		if self.multiplier == zero:
			return emptystring.reduce()

		# no point multiplying in the singular
		if self.multiplier == one:
			return self.multiplicand.reduce()
	
		# Try recursively reducing our internal
		reducedMultiplicand = self.multiplicand.reduce()
		# "bulk up" smaller lego pieces to pattern if need be
		if type(reducedMultiplicand) == mult:
			reducedMultiplicand = conc(reducedMultiplicand)
		if type(reducedMultiplicand) == conc:
			reducedMultiplicand = pattern(reducedMultiplicand)
		if reducedMultiplicand != self.multiplicand:
			return mult(reducedMultiplicand, self.multiplier).reduce()

		# If our multiplicand is a pattern containing a single conc
		# containing a single mult, we can separate that out a lot
		# e.g. ([ab])* -> [ab]*
		if type(self.multiplicand) == pattern \
		and len(self.multiplicand.concs) == 1:
			singleton = [c for c in self.multiplicand.concs][0]
			if len(singleton.mults) == 1:
				return mult(
					singleton.mults[0].multiplicand,
					singleton.mults[0].multiplier * self.multiplier
				).reduce()

		return self

	def regex(self):
		output = ""

		# recurse into subpattern
		if type(self.multiplicand) == pattern:
			output += "(" + self.multiplicand.regex() + ")"

		else: 
			output += self.multiplicand.regex()

		suffix = self.multiplier.regex()

		# Pick whatever is shorter/more comprehensible.
		# e.g. "aa" beats "a{2}", "ababab" beats "(ab){3}"
		if self.multiplier.min == self.multiplier.max \
		and len(output) * self.multiplier.min.v <= len(output) + len(suffix):
			output += str(output) * (self.multiplier.min.v - 1) # because it has one already
		else:
			output += suffix

		return output

	def fsm(self, alphabet):
		return self.multiplicand.fsm(alphabet) * (self.multiplier.min.v, self.multiplier.max.v)

	@classmethod
	def match(cls, string, i):
		try:
			j = cls.matchStatic(string, i, "(")
			cand, j = pattern.match(string, j)
			j = cls.matchStatic(string, j, ")")
		except MatchFailureException:
			cand, j = charclass.match(string, i)

		ier, j = multiplier.match(string, j)
		return mult(cand, ier), j

class conc(lego):
	'''
		A conc (short for "concatenation") is a tuple of mults i.e. an unbroken
		string of mults occurring one after the other.
		e.g. abcde[^fg]*h{4}[a-z]+(subpattern)(subpattern2)
		To express the empty string, use an empty conc, conc().
	'''

	def __init__(self, *mults):
		for m in mults:
			if type(m) != mult:
				raise Exception(str(m) + " is not a mult")
		self.__dict__["mults"] = tuple(mults)

	def __eq__(self, other):
		return type(self) == type(other) \
		and self.mults == other.mults

	def __hash__(self):
		return tuple.__hash__(self.mults)

	def __repr__(self):
		string = "conc("
		string += ", ".join(str(m) for m in self.mults)
		string += ")"
		return string

	def __mul__(self, multiplier):
		if multiplier == one:
			return self
		# Have to replace self with a pattern unfortunately
		return pattern(self) * multiplier

	def __add__(self, other):
		# other must be a conc too
		if type(other) in {charclass, pattern}:
			other = mult(other, one)
		if type(other) == mult:
			other = conc(other)

		return conc(*(self.mults + other.mults)).reduce()

	def __or__(self, other):
		return pattern(self) | other

	def __and__(self, other):
		return pattern(self) & other

	def reduce(self):
		# If we contain a mult with a multiplicand of charclass() (empty)
		# and a nonzero mandatory multiplier, then this entire conc can
		# never return anything.
		for m in self.mults:
			if (m.multiplicand == nothing or m.multiplicand == charclass()) \
			and bound(0) < m.multiplier.min:
				return nothing.reduce()

		# no point concatenating one thing (note: concatenating nothing is
		# entirely valid)
		if len(self.mults) == 1:
			return self.mults[0].reduce()

		# Try recursively reducing our internals
		reducedMults = [m.reduce() for m in self.mults]
		# "bulk up" smaller lego pieces to concs if need be
		reducedMults = [pattern(x) if type(x) == conc else x for x in reducedMults]
		reducedMults = [mult(x, one) if type(x) in {charclass, pattern} else x for x in reducedMults]
		reducedMults = tuple(reducedMults)
		if reducedMults != self.mults:
			return conc(*reducedMults).reduce()

		# multiple mults with identical multiplicands in a row?
		# squish those together
		# e.g. ab?b?c -> ab{0,2}c
		if len(self.mults) > 1:
			for i in range(len(self.mults)-1):
				if self.mults[i].multiplicand == self.mults[i+1].multiplicand:
					squished = mult(
						self.mults[i].multiplicand,
						self.mults[i].multiplier + self.mults[i+1].multiplier
					)
					newMults = self.mults[:i] + (squished,) + self.mults[i+2:]
					return conc(*newMults).reduce()

		# Conc contains (among other things) a *singleton* mult containing a pattern
		# with only one internal conc? Flatten out.
		# e.g. "a(d(ab|a*c))" -> "ad(ab|a*c)"
		# BUT NOT "a(d(ab|a*c)){2,}"
		# AND NOT "a(d(ab|a*c)|y)"
		for i in range(len(self.mults)):
			m = self.mults[i]
			if m.multiplier == one \
			and type(m.multiplicand) == pattern \
			and len(m.multiplicand.concs) == 1:
				single = [c for c in m.multiplicand.concs][0]
				newMults = self.mults[:i] + single.mults + self.mults[i+1:]
				return conc(*newMults).reduce()

		return self

	def fsm(self, alphabet):
		from fsm import epsilon

		# start with a component accepting only the empty string
		fsm1 = epsilon(alphabet)
		for m in self.mults:
			fsm1 += m.fsm(alphabet)
		return fsm1

	def alphabet(self):
		return set().union(*[m.alphabet() for m in self.mults])

	def regex(self):
		return "".join(mult.regex() for mult in self.mults)

	@classmethod
	def match(cls, string, i):
		mults = list()
		try:
			while True:
				m, i = mult.match(string, i)
				mults.append(m)
		except MatchFailureException:
			pass
		return conc(*mults), i

class NoMultPrefixException(Exception):
	'''
		This happens if you call pattern._multprefix() on a pattern whose concs
		have no common multiplier at the front.
	'''

class NoMultSuffixException(Exception):
	'''
		This happens if you call pattern._multsuffix() on a pattern whose concs
		have no common multiplier at the end.
	'''

class pattern(lego):
	'''
		A pattern (also known as an "alt", short for "alternation") is a
		set of concs. The simplest pattern contains a single conc, but it
		is also possible for a pattern to contain multiple alternate possibilities.
		When written out as a regex, these would separated by pipes. A pattern
		containing no possibilities is possible and represents a regular expression
		matching no strings whatsoever (there is no conventional string form for
		this).
		
		e.g. "abc|def(ghi|jkl)" is an alt containing two concs: "abc" and
		"def(ghi|jkl)". The latter is a conc containing four mults: "d", "e", "f"
		and "(ghi|jkl)". The latter in turn is a mult consisting of an upper bound
		1, a lower bound 1, and a multiplicand which is a new subpattern, "ghi|jkl".
		This new subpattern again consists of two concs: "ghi" and "jkl".
	'''
	def __init__(self, *concs):
		for c in concs:
			if type(c) != conc:
				raise Exception("Can't put a " + str(type(c)) + " in a pattern")
		self.__dict__["concs"] = frozenset(concs)

	def __eq__(self, other):
		return type(self) == type(other) \
		and self.concs == other.concs

	def __hash__(self):
		return frozenset.__hash__(self.concs)

	def __repr__(self):
		string = "pattern("
		string += ", ".join(str(c) for c in self.concs)
		string += ")"
		return string

	def __mul__(self, multiplier):
		if multiplier == one:
			return self
		return mult(self, multiplier)

	def __add__(self, other):
		return mult(self, one) + other

	def alphabet(self):
		return set().union(*[c.alphabet() for c in self.concs])

	def __and__(self, other):
		# other must be pattern
		if type(other) == mult:
			other = conc(other)
		if type(other) == conc:
			other = pattern(conc)

		alphabet = self.alphabet() | other.alphabet()
	
		# We need to add an extra character in the alphabet which can stand for
		# "everything else". For example, if the regex is "abc.", then at the moment
		# our alphabet is {"a", "b", "c"}. But "." could match anything else not yet
		# specified. This extra letter stands for that ("[^abc]" in this case).
		alphabet.add(otherchars)

		# Which means that we can build finite state machines sharing that alphabet
		combinedFsm = self.fsm(alphabet) & other.fsm(alphabet)
		return combinedFsm.pattern()

	def __or__(self, other):
		# other must be a pattern too
		if type(other) == charclass:
			other = mult(other, one)
		if type(other) == mult:
			other = conc(other)
		if type(other) == conc:
			other = pattern(other)

		return pattern(*(self.concs | other.concs)).reduce()

	def regex(self):
		if len(self.concs) < 1:
			raise NoRegexException("Can't print an empty pattern.")

		# take the alternation of the input collection of regular expressions.
		# i.e. jam "|" between each element

		# 1+ elements.
		return "|".join(sorted(conc.regex() for conc in self.concs))

	def reduce(self):
		# If one of our internal concs contains a mult containing a charclass()
		# and a nonzero mandatory multiplier, that conc can never match anything
		# So remove it.
		for c in self.concs:
			for m in c.mults:
				if (m.multiplicand == nothing or m.multiplicand == charclass()) \
				and m.multiplier.min > bound(0):
					newConcs = self.concs - {c}
					return pattern(*newConcs).reduce()

		# no point alternating among one possibility
		if len(self.concs) == 1:
			return [e for e in self.concs][0].reduce()

		# Try recursively reducing our internals first.
		reducedConcs = [c.reduce() for c in self.concs]
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
		for c in self.concs:
			if len(c.mults) == 1 \
			and type(c.mults[0].multiplicand) == charclass:
				key = c.mults[0].multiplier
				if key in merged:
					merged[key] |= c.mults[0].multiplicand
					isChanged = True
				else:
					merged[key] = c.mults[0].multiplicand
			else:
				rest.append(c)
		if isChanged == True:
			for key in merged:
				rest.append(conc(mult(merged[key], key)))
			return pattern(*rest).reduce()

		# If the present pattern's concs all have a common prefix, split
		# that out. This increases the depth of the object
		# but it is still arguably simpler/ripe for further reduction
		concPrefix, leftovers = self._concprefix()
		if concPrefix != emptystring:
			mults = concPrefix.mults + (mult(leftovers, one),)
			return conc(*mults).reduce()

		# Same but for prefixes.
		leftovers, concSuffix = self._concsuffix()
		if concSuffix != emptystring:
			mults = (mult(leftovers, one),) + concSuffix.mults
			return conc(*mults).reduce()

		return self

	@classmethod
	def parse(cls, string):
		'''Parse a full string and return a pattern object. Fail if the whole string wasn't parsed'''
		result, i = cls.match(string, 0)
		if i != len(string):
			raise MatchFailureException("Could not parse '" + string + "' beyond index " + str(i))
		return result

	@classmethod
	def match(cls, string, i):
		concs = list()

		# first one
		c, i = conc.match(string, i)
		concs.append(c)

		# the rest
		try:
			while True:
				i = cls.matchStatic(string, i, "|")
				c, i = conc.match(string, i)
				concs.append(c)
		except MatchFailureException:
			pass

		return pattern(*concs), i

	def _multprefix(self):
		'''
			"ZA|ZB|ZC" -> "Z", "A|B|C"
			Find a common mult prefix of all the concs in the current pattern.
		'''
		commonMult = None
		for c in self.concs:

			# No common prefix here
			if len(c.mults) == 0:
				raise NoMultPrefixException

			if commonMult is None:
				commonMult = c.mults[0]
			else:
				try:
					commonMult &= c.mults[0]
				except NoCommonMultiplicandException:
					raise NoMultPrefixException

			# Can occur for e.g. "Z*AB|ZC". Multiplicand is shared,
			# but intersection of multipliers is zero
			if commonMult.multiplier == zero:
				raise NoMultPrefixException

		# Can occur if self is nothing
		if commonMult is None:
			raise NoMultPrefixException

		leftovers = []
		for c in self.concs:
		
			newMult1 = c.mults[0] - commonMult

			if newMult1.multiplier == zero:
				# omit that mult entirely since it has been factored out
				leftovers.append(conc(*c.mults[1:]))
	
			else:
				leftovers.append(conc(newMult1, *c.mults[1:]))
		
		# return the remainder as well
		leftovers = pattern(*leftovers)

		return commonMult, leftovers

	def _multsuffix(self):
		'''
			"AZ|BZ|CZ" -> "A|B|C", "Z"
			Find a common mult suffix of all the concs in the current pattern.
		'''
		commonMult = None
		for c in self.concs:

			# No common prefix here
			if len(c.mults) == 0:
				raise NoMultSuffixException

			if commonMult is None:
				commonMult = c.mults[-1]
			else:
				try:
					commonMult &= c.mults[-1]
				except NoCommonMultiplicandException:
					raise NoMultSuffixException

			# Can occur for e.g. "ABZ*|CZ". Multiplicand is shared,
			# but intersection of multipliers is zero
			if commonMult.multiplier == zero:
				raise NoMultSuffixException
		
		# Can occur if self is nothing
		if commonMult is None:
			raise NoMultSuffixException

		leftovers = []
		for c in self.concs:
		
			newMult1 = c.mults[-1] - commonMult

			if newMult1.multiplier == zero:
				# omit that mult entirely since it has been factored out
				leftovers.append(conc(*c.mults[:-1]))
	
			else:
				leftovers.append(conc(*(c.mults[:-1] + (newMult1,))))
		
		# return the remainder as well
		leftovers = pattern(*leftovers)

		return leftovers, commonMult

	def _concprefix(self):
		'''
			Find the longest conc which acts as prefix to every conc in this pattern.
			This could be the empty string. Return the common prefix along with all
			the leftovers after truncating that common prefix from each conc.
			"ZA|ZB|ZC" -> "Z", "(A|B|C)"
			"ZA|ZB|ZC|Z" -> "Z", "(A|B|C|)"
			"CZ|CZ" -> "CZ", "()"
		'''

		# Try to find just one mult to put into that common prefix.
		# Check the first mult in each conc to see if they have anything in common.
		prefix = []
		leftovers = self
		while True:
			try:
				commonMult, leftovers = leftovers._multprefix()
				prefix.append(commonMult)
			except NoMultPrefixException:
				return conc(*prefix), leftovers

	def _concsuffix(self):
		'''
			As _concprefix() but for suffixes. Note reversed, but still logical, order
			of arguments.
			"AAZY|BBZY|CCZY"   -> "(AA|BB|CC)", "ZY"
			"CZ|CZ" -> "()", "CZ"
		'''

		# Try to find just one mult to put into that common prefix.
		# Check the first mult in each conc to see if they have anything in common.
		suffix = []
		leftovers = self
		while True:
			try:
				leftovers, commonMult = leftovers._multsuffix()
				suffix.insert(0, commonMult)
			except NoMultSuffixException:
				return leftovers, conc(*suffix)

	def fsm(self, alphabet):
		from fsm import null

		fsm1 = null(alphabet)
		for c in self.concs:
			fsm1 |= c.fsm(alphabet)
		return fsm1

# Special and useful values go here.

# A special character representing "all characters not explicitly
# mentioned elsewhere in this alphabet"
otherchars = None

# Standard character classes
w = charclass("0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz")
d = charclass("0123456789")
s = charclass("\t\n\v\f\r ")
W = ~w
D = ~d
S = ~s
dot = ~charclass()

shorthand = {
	w : "\\w", d : "\\d", s : "\\s",
	W : "\\W", D : "\\D", S : "\\S",
	dot : ".",
}

# Characters which users may escape in a regex instead of inserting them
# literally. In ASCII order:
escapes = {
	"\t" : "\\t", # tab
	"\n" : "\\n", # line feed
	"\v" : "\\v", # vertical tab
	"\f" : "\\f", # form feed
	"\r" : "\\r", # carriage return
}

# These are the characters carrying special meanings when they appear "outdoors"
# within a regular expression. To be interpreted literally, they must be
# escaped with a backslash.
allSpecial = set("\\[]|().?*+{}")

# These are the characters carrying special meanings when they appear INSIDE a
# character class (delimited by square brackets) within a regular expression.
# To be interpreted literally, they must be escaped with a backslash.
# Notice how much smaller this class is than the one above; note also that the
# hyphen and caret do NOT appear above.
classSpecial = set("\\[]^-")

# these are the character ranges which can be used inside square brackets e.g.
# "[a-z]", "[F-J]". These ranges should be disjoint.
allowableRanges = {
	"ABCDEFGHIJKLMNOPQRSTUVWXYZ",
	"abcdefghijklmnopqrstuvwxyz",
	"0123456789",
}

# Use this for cases where no upper bound is needed
infinity = bound(None)

# Preset multipliers. These get used ALL THE TIME in unit tests
zero = multiplier(0, 0) # has some occasional uses
qm   = multiplier(0, 1)
one  = multiplier(1, 1)
star = multiplier(0, infinity)
plus = multiplier(1, infinity)

# Symbol lookup table for preset multipliers.
symbolic = {
	qm   : "?",
	one  : "" ,
	star : "*",
	plus : "+",
}

# A very special conc expressing the empty string, ""!
emptystring = conc()

# An even more special pattern expressing "no possibilities at all"!
# This regex can never match anything. It's debatable when to use this
# and when to use an empty charclass(), which has the same properties.
# I use pattern() because it's easier to add to, I guess?
nothing = pattern()

# Unit tests.
if __name__ == '__main__':

	# Odd bug with ([bc]*c)?[ab]*
	int5A = mult(charclass("bc"), star).fsm({"a", "b", "c", otherchars})
	assert int5A.accepts("")
	int5B = mult(charclass("c"), one).fsm({"a", "b", "c", otherchars})
	assert int5B.accepts("c")
	int5C = int5A + int5B
	assert (int5A + int5B).accepts("c")

	# Empty mult suppression
	assert conc(
		mult(charclass(), one), # this mult can never actually match anything
		mult(charclass("0"), one),
		mult(charclass("0123456789"), one),
	).reduce() == nothing
	assert conc(
		mult(nothing, one), # this mult can never actually match anything
		mult(charclass("0"), one),
		mult(charclass("0123456789"), one),
	).reduce() == nothing

	# Empty conc suppression in patterns.
	assert pattern(
		conc(
			mult(charclass(), one), # this mult can never actually match anything
			mult(charclass("0"), one),
			mult(charclass("0123456789"), one),
		) # so neither can this conc
	).reduce() == nothing
	assert pattern(
		conc(
			mult(nothing, one), # this mult can never actually match anything
			mult(charclass("0"), one),
			mult(charclass("0123456789"), one),
		) # so neither can this conc
	).reduce() == nothing

	# Empty pattern suppression in mults
	assert mult(nothing, qm).reduce() == emptystring

	# empty pattern behaviour
	try:
		nothing._multprefix()
		assert(False)
	except NoMultPrefixException:
		pass
	try:
		nothing._multsuffix()
		assert(False)
	except NoMultSuffixException:
		pass
	assert nothing._concprefix() == (emptystring, nothing)
	assert nothing._concsuffix() == (nothing, emptystring)
	assert nothing.reduce() == nothing

	# pattern.fsm()

	# "a[^a]"
	anota = pattern(
		conc(
			mult(charclass("a"), one),
			mult(~charclass("a"), one),
		)
	).fsm("ab")
	assert not anota.accepts("a")
	assert not anota.accepts("b")
	assert not anota.accepts("aa")
	assert anota.accepts("ab")
	assert not anota.accepts("ba")
	assert not anota.accepts("bb")
	
	# "0\\d"
	zeroD = pattern(
		conc(
			mult(charclass("0"), one),
			mult(charclass("123456789"), one)
		)
	).fsm(d.chars)
	assert zeroD.accepts("01")
	assert not zeroD.accepts("10")

	# "\\d{2}"
	d2 = pattern(
		conc(
			mult(
				d, multiplier(2, 2)
			)
		)
	).fsm(d.chars)
	assert not d2.accepts("")
	assert not d2.accepts("1")
	assert d2.accepts("11")
	assert not d2.accepts("111")

	# abc|def(ghi|jkl)
	conventional = pattern(
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
	).fsm(w.chars)
	assert not conventional.accepts("a")
	assert not conventional.accepts("ab")
	assert conventional.accepts("abc")
	assert not conventional.accepts("abcj")
	assert conventional.accepts("defghi")
	assert conventional.accepts("defjkl")

	# A subtlety in mult reduction.
	# ([$%\^]|){1} should become ([$%\^])? then [$%\^]?,
	# ([$%\^]|){1} should NOT become ([$%\^]|) (the pattern alone)
	assert mult(
		pattern(
			conc(),
			conc(
				mult(charclass("$%^"), one)
			)
		), one
	).reduce() == mult(charclass("$%^"), qm)

	# nested pattern reduction in a conc
	# a(d(ab|a*c)) -> ad(ab|a*c)
	assert conc(
		mult(charclass("a"), one),
		mult(
			pattern(
				# must contain only one conc. Otherwise, we have e.g. "a(zz|d(ab|a*c))"
				conc(
					# can contain anything
					mult(charclass("d"), one),
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
				),
			), one # must be one. Otherwise, we have e.g. "a(d(ab|a*c)){2}"
		)
	).reduce() == conc(
		mult(charclass("a"), one),
		mult(charclass("d"), one),
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

	# pattern._multprefix()

	# aa, aa -> a, (a|a)
	assert pattern(
		conc(
			mult(charclass("a"), one),
			mult(charclass("a"), one),
		),
		conc(
			mult(charclass("a"), one),
			mult(charclass("a"), one),
		),
	)._multprefix() == (
		mult(charclass("a"), one),
		pattern(
			conc(
				mult(charclass("a"), one)
			),
		),
	)

	# abc, aa -> a, (a|bc)
	assert pattern(
		conc(
			mult(charclass("a"), one),
			mult(charclass("b"), one),
			mult(charclass("c"), one),
		),
		conc(
			mult(charclass("a"), one),
			mult(charclass("a"), one),
		),
	)._multprefix() == (
		mult(charclass("a"), one),
		pattern(
			conc(
				mult(charclass("a"), one),
			),
			conc(
				mult(charclass("b"), one),
				mult(charclass("c"), one),
			),
		)
	)

	# a, bc -> exception
	try:
		pattern(
			conc(
				mult(charclass("a"), one),
			),
			conc(
				mult(charclass("b"), one),
				mult(charclass("c"), one),
			),
		)._multprefix()
		assert False
	except NoMultPrefixException:
		pass

	# cf{1,2}, cf -> c, (f|f?)
	assert pattern(
		conc(
			mult(charclass("c"), one),
			mult(charclass("f"), multiplier(1, 2)),
		),
		conc(
			mult(charclass("c"), one),
			mult(charclass("f"), one),
		),
	)._multprefix() == (
		mult(charclass("c"), one),
		pattern(
			conc(
				mult(charclass("f"), multiplier(1, 2)),
			),
			conc(
				mult(charclass("f"), one),
			),
		),
	)

	# pattern._concprefix() tests

	# aa, aa -> aa, ()
	assert pattern(
		conc(
			mult(charclass("a"), one),
			mult(charclass("a"), one),
		),
		conc(
			mult(charclass("a"), one),
			mult(charclass("a"), one),
		),
	)._concprefix() == (
		conc(
			mult(charclass("a"), one),
			mult(charclass("a"), one),
		),
		pattern(emptystring)
	)

	# abc, aa -> a, (a|bc)
	assert pattern(
		conc(
			mult(charclass("a"), one),
			mult(charclass("b"), one),
			mult(charclass("c"), one),
		),
		conc(
			mult(charclass("a"), one),
			mult(charclass("a"), one),
		),
	)._concprefix() == (
		conc(
			mult(charclass("a"), one),
		),
		pattern(
			conc(
				mult(charclass("a"), one),
			),
			conc(
				mult(charclass("b"), one),
				mult(charclass("c"), one),
			),
		)
	)

	# a, bc -> emptystring, (a|bc)
	assert pattern(
		conc(
			mult(charclass("a"), one),
		),
		conc(
			mult(charclass("b"), one),
			mult(charclass("c"), one),
		),
	)._concprefix() == (
		emptystring,
		pattern(
			conc(
				mult(charclass("a"), one),
			),
			conc(
				mult(charclass("b"), one),
				mult(charclass("c"), one),
			),
		)
	)

	# cf{1,2}, cf -> cf, (f?|)
	assert pattern(
		conc(
			mult(charclass("c"), one),
			mult(charclass("f"), multiplier(1, 2)),
		),
		conc(
			mult(charclass("c"), one),
			mult(charclass("f"), one),
		),
	)._concprefix() == (
		conc(
			mult(charclass("c"), one),
			mult(charclass("f"), one),
		),
		pattern(
			emptystring,
			conc(
				mult(charclass("f"), qm),
			),
		),
	)

	# ZA|ZB|ZC -> Z, A|B|C
	assert pattern(
		conc(
			mult(charclass("Z"), one),
			mult(charclass("A"), one),
		),
		conc(
			mult(charclass("Z"), one),
			mult(charclass("B"), one),
		),
		conc(
			mult(charclass("Z"), one),
			mult(charclass("C"), one),
		),
	)._concprefix() == (
		conc(mult(charclass("Z"), one)),
		pattern(
			conc(mult(charclass("A"), one)),
			conc(mult(charclass("B"), one)),
			conc(mult(charclass("C"), one)),
		)
	)

	# Z+A|ZB|ZZC -> Z*, A|B|ZC
	assert pattern(
		conc(
			mult(charclass("Z"), plus),
			mult(charclass("A"), one),
		),
		conc(
			mult(charclass("Z"), one),
			mult(charclass("B"), one),
		),
		conc(
			mult(charclass("Z"), one),
			mult(charclass("Z"), one),
			mult(charclass("C"), one),
		),
	)._concprefix() == (
		conc(mult(charclass("Z"), one)),
		pattern(
			conc(
				mult(charclass("Z"), star),
				mult(charclass("A"), one),
			),
			conc(
				mult(charclass("B"), one),
			),
			conc(
				mult(charclass("Z"), one),
				mult(charclass("C"), one),
			),
		),
	)

	# a{2}b|a+c -> a, (ab|a*c)
	assert pattern(
		conc(
			mult(charclass("a"), multiplier(2, 2)),
			mult(charclass("b"), one),
		),
		conc(
			mult(charclass("a"), plus),
			mult(charclass("c"), one),
		)
	)._concprefix() == (
		conc(mult(charclass("a"), one)),
		pattern(
			conc(
				mult(charclass("a"), one),
				mult(charclass("b"), one),
			),
			conc(
				mult(charclass("a"), star),
				mult(charclass("c"), one),
			),
		),
	)

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
	assert ~charclass("a") == ~charclass("a")
	assert ~charclass("a") != charclass("a")
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
	assert dot.regex() == "."
	assert (~charclass("")).regex() == "."
	assert (~charclass("a")).regex() == "[^a]"
	assert (~charclass("{")).regex() == "[^{]"
	assert (~charclass("\t")).regex() == "[^\\t]"
	assert (~charclass("^")).regex() == "[^\\^]"

	# charclass parsing
	assert charclass.match("a", 0) == (charclass("a"), 1)
	assert charclass.match("aa", 1) == (charclass("a"), 2)
	assert charclass.match("a$", 1) == (charclass("$"), 2)
	assert charclass.match(".", 0) == (dot, 1)
	try:
		charclass.match("[", 0)
		assert False
	except MatchFailureException:
		pass
	try:
		charclass.match("a", 1)
		assert False
	except MatchFailureException:
		pass

	# charclass set operations
	
	# charclass negation
	assert ~~charclass("a") == charclass("a")
	assert charclass("a") == ~~charclass("a")

	# charclass union
	# [ab] u [bc] = [abc]
	assert charclass("ab") | charclass("bc") == charclass("abc")
	# [ab] u [^bc] = [^c]
	assert charclass("ab") | ~charclass("bc") == ~charclass("c")
	# [^a] u [bc] = [^a]
	assert ~charclass("ab") | charclass("bc") == ~charclass("a")
	# [^ab] u [^bc] = [^b]
	assert ~charclass("ab") | ~charclass("bc") == ~charclass("b")

	# charclass subtraction
	# [ab] - [bc] = [a]
	assert charclass("ab") - charclass("bc") == charclass("a")
	# [ab] - [^bc] = [b]
	assert charclass("ab") - ~charclass("bc") == charclass("b")
	# [^ab] - [bc] = [^abc]
	assert ~charclass("ab") - charclass("bc") == ~charclass("abc")
	# [^ab] - [^bc] = [c]
	assert ~charclass("ab") - ~charclass("bc") == charclass("c")

	# charclass intersection
	# [ab] n [bc] = [b]
	assert charclass("ab") & charclass("bc") == charclass("b")
	# [ab] n [^bc] = [a]
	assert charclass("ab") & ~charclass("bc") == charclass("a")
	# [^ab] n [bc] = [c]
	assert ~charclass("ab") & charclass("bc") == charclass("c")
	# [^ab] n [^bc] = [^abc]
	assert ~charclass("ab") & ~charclass("bc") == ~charclass("abc")

	# issubset()
	# [a] < [ab] = True
	assert charclass("a").issubset(charclass("ab"))
	# [c] < [^ab] = True
	assert charclass("c").issubset(~charclass("ab"))
	# [^c] < [ab] = False
	assert not (~charclass("c")).issubset(charclass("ab"))
	# [^ab] < [^a] = True
	assert (~charclass("ab")).issubset(~charclass("a"))

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
	assert bound(2).regex() == "2"
	assert bound(None).regex() == ""
	assert multiplier(2, None).regex() == "{2,}"
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
		mult(~charclass("fg"), star),
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
	# mult -> charclass
	assert mult(charclass("a"), one).reduce() == charclass("a")
	assert mult(charclass("a"), qm).reduce() == mult(charclass("a"), qm)
	assert mult(charclass("a"), zero).reduce() == emptystring
	assert mult(charclass(), one).reduce() == nothing
	assert mult(charclass(), qm).reduce() == emptystring
	assert mult(charclass(), zero).reduce() == emptystring
	assert mult(charclass(), multiplier(0, 5)).reduce() == emptystring
	assert mult(nothing, one).reduce() == nothing
	assert mult(nothing, qm).reduce() == emptystring
	assert mult(nothing, zero).reduce() == emptystring
	assert mult(nothing, multiplier(0, 5)).reduce() == emptystring

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

	# This happens even if emptystring is the only thing left inside the mult
	assert mult(
		pattern(
			emptystring
		), multiplier(2, 2)
	).reduce() == emptystring

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
		mult(~charclass("fg"), star),
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
			mult(~charclass("fg"), star),
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
			mult(dot, one),
			mult(dot, one),
			mult(dot, one),
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
	assert conc(
		mult(charclass("a"), one),
		mult(charclass(), one),
		mult(charclass("b"), one),
	).reduce() == nothing
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

	# common prefix reduction of pattern
	# a{2}b|a+c -> a{2}(ab|a*c)
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

	# bound class tests

	assert bound(0) & bound(None) == bound(0)
	assert bound(1) & bound(None) == bound(1)
	assert qm.mandatory == bound(0)
	assert qm.optional == bound(1)

	# multiplier intersection operator tests
	assert zero & zero == zero
	assert zero & qm   == zero
	assert zero & one  == zero
	assert zero & star == zero
	assert zero & plus == zero

	assert qm   & zero == zero
	assert qm   & qm   == qm
	assert qm   & one  == zero
	assert qm   & star == qm
	assert qm   & plus == qm

	assert one  & zero == zero
	assert one  & qm   == zero
	assert one  & one  == one
	assert one  & star == zero
	assert one  & plus == one

	assert star & zero == zero
	assert star & qm   == qm
	assert star & one  == zero
	assert star & star == star
	assert star & plus == star

	assert plus & zero == zero
	assert plus & qm   == qm
	assert plus & one  == one
	assert plus & star == star
	assert plus & plus == plus

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

	# a | bc -> (a|bc), emptystring
	assert pattern(
		conc(
			mult(charclass("a"), one),
		),
		conc(
			mult(charclass("b"), one),
			mult(charclass("c"), one),
		),
	)._concsuffix() == (
		pattern(
			conc(
				mult(charclass("a"), one),
			),
			conc(
				mult(charclass("b"), one),
				mult(charclass("c"), one),
			),
		),
		emptystring
	)

	# aa, bca -> (a|bc), a
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
	)._concsuffix() == (
		pattern(
			conc(
				mult(charclass("a"), one),
			),
			conc(
				mult(charclass("b"), one),
				mult(charclass("c"), one),
			),
		),
		conc(mult(charclass("a"), one)),
	)

	# xyza | abca | a -> (xyz|abc|), a
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
	)._concsuffix() == (
		pattern(
			emptystring,
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
		),
		conc(mult(charclass("a"), one)),
	)

	# f{2,3}c, fc -> (f{1,2}|), fc
	assert pattern(
		conc(
			mult(charclass("f"), multiplier(2, 3)),
			mult(charclass("c"), one),
		),
		conc(
			mult(charclass("f"), one),
			mult(charclass("c"), one),
		),
	)._concsuffix() == (
		pattern(
			emptystring,
			conc(
				mult(charclass("f"), multiplier(1, 2)),
			),
		),
		conc(
			mult(charclass("f"), one),
			mult(charclass("c"), one),
		)
	)

	# e | axe -> "(|ax)", e
	assert pattern(
		conc(
			mult(charclass("e"), one),
		),
		conc(
			mult(charclass("a"), one),
			mult(charclass("x"), one),
			mult(charclass("e"), one),
		),
	)._concsuffix() == (
		pattern(
			emptystring,
			conc(
				mult(charclass("a"), one),
				mult(charclass("x"), one),
			),
		),
		conc(mult(charclass("e"), one))
	)

	# aa | aa -> (), aa
	assert pattern(
		conc(
			mult(charclass("a"), one),
			mult(charclass("a"), one),
		),
		conc(
			mult(charclass("a"), one),
			mult(charclass("a"), one),
		),
	)._concsuffix() == (
		pattern(emptystring),
		conc(
			mult(charclass("a"), one),
			mult(charclass("a"), one),
		)
	)

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

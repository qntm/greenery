# -*- coding: utf-8 -*-
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

from __future__ import absolute_import

class nomatch(Exception):
	'''Thrown when parsing fails. Almost always caught and almost never fatal'''
	pass

def reduce_after(method):
	'''reduce() the result of this method call (unless you already reduced it).'''
	def new_method(self, *args, **kwargs):
		result = method(self, *args, **kwargs)
		if method.__name__ == "reduce" and result == self:
			return result
		return result.reduce()
	return new_method

@reduce_after
def parse(string):
	'''
		Parse a full string and return a lego piece. Fail if the whole string
		wasn't parsed
	'''
	p, i = pattern.match(string, 0)
	if i != len(string):
		raise Exception("Could not parse '" + string + "' beyond index " + str(i))
	return p

def static(string, i, static):
	j = i+len(static)
	if string[i:j] == static:
		return j
	raise nomatch

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
		raise Exception("This object is immutable.")

	def fsm(self, alphabet):
		'''
			Return the present lego piece in the form of a finite state machine,
			as imported from the fsm module.
			If no alphabet is explicitly supplied, which seems quite probable,
			we use the lego.alphabet() method (later) to list all the characters
			mentioned in self. However, if we intend to connect this FSM to another
			one which uses different characters, we may need to supply an alphabet
			which is a superset of both sets.
		'''
		raise Exception("Not implemented")

	def __repr__(self):
		'''
			Return a string approximating the instantiation line
			for the present lego piece.
		'''
		raise Exception("Not implemented")

	def __str__(self):
		'''
			Render the present lego piece in the form of a regular expression.
			Some lego pieces may be created which cannot be rendered in this way.
			In particular: a pattern containing no concs; a multiplier of zero.
		'''
		raise Exception("Not implemented")

	def match(cls, string, i):
		'''
			Start at index i in the supplied string and try to match one of the
			present class. Elementary recursive descent parsing with very little
			need for flair. The opposite of __str__(), above. (In most cases.)
			Throws a nomatch in the event of failure.
		'''
		raise Exception("Not implemented")

	@reduce_after
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
		raise Exception("Not implemented")

	@reduce_after
	def __add__(self, other):
		'''
			Concatenate any two lego pieces, regardless of differing classes. Because
			reduce() (above) is always called afterwards, the result is as simplified
			as possible.
			Call using "a = b + c"
		'''
		raise Exception("Not implemented")

	@reduce_after
	def __mul__(self, multiplier):
		'''
			Equivalent to repeated concatenation. Multiplier consists of a minimum
			and a maximum; maximum may be infinite (for Kleene star closure).
			Call using "a = b * qm"
			Reduce() is always called afterwards.
		'''
		raise Exception("Not implemented")

	@reduce_after
	def __or__(self, other):
		'''
			Alternate between any two lego pieces, regardless of differing classes.
			Again, reduce() is called afterwards, usually with excellent results.
			Call using "a = b | c".
			This method MUST NOT call the fsm() method, because this method is used
			in turn when converting an FSM back to a regex.
		'''
		raise Exception("Not implemented")

	@reduce_after
	def __and__(self, other):
		'''
			Intersection function. Return a lego piece that can match any string
			that both self and other can match. Fairly elementary results relating
			to regular languages and finite state machines show that this is
			possible, but implementation is a BEAST in many cases. Here, we convert
			both lego pieces to FSMs (see fsm(), above) for the intersection, then
			back to lego afterwards.
			Call using "a = b & c"
		'''
		raise Exception("Not implemented")

	def alphabet(self):
		'''
			Return a set of all unique characters used in this lego piece.
			In theory this could be a static property, self.alphabet, not
			a function, self.alphabet(), but in the vast majority of cases
			this will never be queried so it's a waste of computation to
			calculate it every time a lego piece is instantiated.
			By convention, otherchars is always included in this result.
		'''
		raise Exception("Not implemented")

	@reduce_after
	def everythingbut(self):
		'''
			Return a lego object which will match any string not matched by self,
			and which will not match any string matched by self.
			Another task which is very difficult in general (and typically returns
			utter garbage when actually printed), but becomes trivial to code
			thanks to FSM routines.
		'''
		return self.fsm().everythingbut().lego()

	def __reversed__(self):
		'''
			Return a lego object which will match any string which, when reversed,
			self would match. E.g. if self matches "beer" then reversed(self) will
			match "reeb".
		'''
		raise Exception("Not implemented")

	def empty(self):
		'''
			Return False if there exists a string which the present lego piece
			can match. Return True if no such string exists. Examples of empty
			lego pieces are charclass() and pattern()
		'''
		raise Exception("Not implemented")

	def strings(self, otherchar=None):
		'''
			Each time next() is called on this iterator, a new string is returned
			which will the present lego piece can match. StopIteration is raised once
			all such strings have been returned, although a regex with a * in may
			match infinitely many strings.
		'''

		# In the case of a regex like "[^abc]", there are infinitely many (well, a
		# very large finite number of) single characters which will match. It's not
		# productive to iterate over all of these giving every single example.
		# You must supply your own "otherchar" to stand in for all of these
		# possibilities.

		for string in self.fsm().strings():

			# Have to represent "otherchars" somehow.
			if otherchars in string:
				if otherchar == None:
					raise Exception("Please choose an 'otherchar'")
				string = [
					otherchar if char == otherchars else char
					for char in string
				]

			yield "".join(string)

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
		assert otherchars not in set(chars)
		self.__dict__["chars"]   = frozenset(chars)
		self.__dict__["negated"] = negateMe

	def __eq__(self, other):
		try:
			return self.chars == other.chars and self.negated == other.negated
		except AttributeError:
			return False

	def __ne__(self, other):
		return not self.__eq__(other)

	def __hash__(self):
		return hash((self.chars, self.negated))

	@reduce_after
	def __mul__(self, ier):
		# e.g. "a" * {0,1} = "a?"
		if ier == one:
			return self
		return mult(self, ier)

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

	# Shorthand codes for use inside charclasses e.g. [abc\d]
	w = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz"
	d = "0123456789"
	s = "\t\n\v\f\r "
	shorthand = {
		w : "\\w",
		d : "\\d",
		s : "\\s",
	}

	def __str__(self):
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

			if char in charclass.allSpecial:
				return "\\" + char

			return char

		# multiple characters (or possibly 0 characters)
		return "[" + self.escape() + "]"

	def escape(self):

		def escapeChar(char):
			if char in charclass.classSpecial:
				return "\\" + char
			if char in escapes.keys():
				return escapes[char]
			return char

		def recordRange():
			# there's no point in putting a range when the whole thing is
			# 3 characters or fewer.
			if len(currentRange) < 4:
				return "".join(escapeChar(char) for char in currentRange)
			else:
				return escapeChar(currentRange[0]) + "-" + escapeChar(currentRange[-1])

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
				# charclass.allowableRanges listed above.
				superRange = None
				for allowableRange in charclass.allowableRanges:
					if char in allowableRange:
						superRange = allowableRange
						break

				if superRange is None:
					# if this character doesn't appear above, then any existing
					# currentRange should be sorted and filed now
					# if there is one
					output += recordRange()
					currentRange=""
				else:
					i = superRange.index(char)

					# char doesn't fit old range: restart
					if i == 0 or superRange[i-1] != currentRange[-1]:
						output += recordRange()
						currentRange = ""

			currentRange += char

		output += recordRange()

		return output

	def fsm(self, alphabet=None):
		from .fsm import fsm

		if alphabet is None:
			alphabet = self.alphabet()

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
		string += "charclass("
		if len(self.chars) > 0:
			string += repr("".join(str(char) for char in sorted(self.chars, key=str)))
		string += ")"
		return string

	@reduce_after
	def reduce(self):
		# Charclasses cannot be reduced().
		return self

	@reduce_after
	def __add__(self, other):
		return mult(self, one) + other

	def alphabet(self):
		return {otherchars} | self.chars

	def empty(self):
		return len(self.chars) == 0 and self.negated == False

	@classmethod
	def match(cls, string, i):
		if i >= len(string):
			raise nomatch

		def matchInternalChar(string, i):

			# e.g. if we see "\\t", return "\t"
			for key in escapes.keys():
				try:
					return key, static(string, i, escapes[key])
				except nomatch:
					pass

			# special chars e.g. "\\-" returns "-"
			for char in charclass.classSpecial:
				try:
					return char, static(string, i, "\\" + char)
				except nomatch:
					pass

			# single non-special character, not contained
			# inside square brackets
			char, j = string[i], i+1
			if char in charclass.classSpecial:
				raise nomatch

			return char, j

		def matchClassInterior1(string, i):

			# Attempt 1: shorthand e.g. "\w"
			for key in charclass.shorthand:
				try:
					return key, static(string, i, charclass.shorthand[key])
				except nomatch:
					pass

			# Attempt 2: a range e.g. "d-h"
			try:
				first, j = matchInternalChar(string, i)
				k = static(string, j, "-")
				last, k = matchInternalChar(string, k)

				for allowableRange in charclass.allowableRanges:
					if first not in allowableRange:
						continue

					# last must be in the same character range as first
					if last not in allowableRange:
						continue

					firstIndex = allowableRange.index(first)
					lastIndex = allowableRange.index(last)

					# and in order i.e. a < b
					if firstIndex >= lastIndex:
						continue

					# OK
					return allowableRange[firstIndex:lastIndex + 1], k

				raise nomatch("Range '" + first + "' to '" + last + "' not allowed")
			except nomatch:
				pass

			# Attempt 3: just a character on its own
			return matchInternalChar(string, i)

		def matchClassInterior(string, i):
			internals = ""
			try:
				while True:
					internal, i = matchClassInterior1(string, i)
					internals += internal
			except nomatch:
				pass
			return internals, i

		# wildcard ".", "\\w", "\\d", etc.
		for key in shorthand.keys():
			try:
				return key, static(string, i, shorthand[key])
			except nomatch:
				pass

		# "[^dsgsdg]"
		try:
			j = static(string, i, "[^")
			chars, j = matchClassInterior(string, j)
			j = static(string, j, "]")
			return ~charclass(chars), j
		except nomatch:
			pass

		# "[sdfsf]"
		try:
			j = static(string, i, "[")
			chars, j = matchClassInterior(string, j)
			j = static(string, j, "]")
			return charclass(chars), j
		except nomatch:
			pass

		# e.g. if seeing "\\t", return "\t"
		for key in escapes.keys():
			try:
				return charclass(key), static(string, i, escapes[key])
			except nomatch:
				pass

		# e.g. if seeing "\\{", return "{"
		for char in charclass.allSpecial:
			try:
				return charclass(char), static(string, i, "\\" + char)
			except nomatch:
				pass

		# single non-special character, not contained inside square brackets
		char, i = string[i], i+1
		if char in charclass.allSpecial:
			raise nomatch

		return charclass(char), i

	# set operations
	def __invert__(self):
		'''
			Negate the current charclass. e.g. [ab] becomes [^ab]. Call
			using "charclass2 = ~charclass1"
		'''
		return charclass(self.chars, negateMe=not self.negated)

	@reduce_after
	def __or__(self, other):
		try:
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

		# "other" lacks attribute "negated" or "chars"
		# "other" is not a charclass
		# Never mind!
		except AttributeError:
			return mult(self, one) | other

	@reduce_after
	def __and__(self, other):
		try:
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

		# "other" lacks attribute "negated" or "chars"
		# "other" is not a charclass
		# Never mind!
		except AttributeError:
			return mult(self, one) & other

	def __reversed__(self):
		return self

class bound:
	'''An integer but sometimes also possibly infinite (None)'''
	def __init__(self, v):
		assert v is None or v >= 0
		self.__dict__['v'] = v

	def __repr__(self):
		if self == inf:
			return "inf"
		return repr(self.v)

	def __str__(self):
		if self == inf:
			# This only happens for an unlimited upper bound
			return ""
		return str(self.v)

	def __eq__(self, other):
		try:
			return self.v == other.v
		except AttributeError:
			return False

	def __ne__(self, other):
		return not self.__eq__(other)

	def __hash__(self):
		return hash(self.v)

	def __lt__(self, other):
		if self == inf:
			return False
		if other == inf:
			return True
		return self.v < other.v

	def __ge__(self, other):
		return not self < other

	def __gt__(self, other):
		if other == inf:
			return False
		if self == inf:
			return True
		return self.v > other.v

	def __mul__(self, other):
		'''Multiply this bound by another'''
		if self == inf or other == inf:
			return inf
		return bound(self.v * other.v)

	def __add__(self, other):
		'''Add this bound to another'''
		if self == inf or other == inf:
			return inf
		return bound(self.v + other.v)

	def __sub__(self, other):
		'''
			Subtract another bound from this one.
			Caution: this operation is not meaningful for all bounds.
		'''
		if other == inf:
			assert self == inf

			# Infinity minus infinity is zero. This has to be true so that
			# we can for example subtract multiplier(bound(0), inf) from
			# multiplier(bound(1), inf) to get multiplier(bound(1), bound(1))
			return bound(0)
		if self == inf:
			return self
		return bound(self.v - other.v)

class multiplier:
	'''
		A min and a max. The vast majority of characters in regular
		expressions occur without a specific multiplier, which is implicitly
		equivalent to a min of 1 and a max of 1, but many more have explicit
		multipliers like "*" (min = 0, max = inf) and so on.
		Although it seems odd and can lead to some confusing edge cases, we do
		also permit a max of 0 (iff min is 0 too). This allows the multiplier
		"zero" to exist, which actually are quite useful in their own special way.
	'''

	def __init__(self, min, max):
		assert min != inf
		assert min <= max

		# More useful than "min" and "max" in many situations
		# are "mandatory" and "optional".
		mandatory = min
		optional = max - min

		self.__dict__['min'] = min
		self.__dict__['max'] = max
		self.__dict__['mandatory'] = mandatory
		self.__dict__['optional'] = optional

	def __eq__(self, other):
		try:
			return self.min == other.min and self.max == other.max
		except AttributeError:
			return False

	def __ne__(self, other):
		return not self.__eq__(other)

	def __hash__(self):
		return hash((self.min, self.max))

	def __repr__(self):
		return "multiplier(" + repr(self.min) + ", " + repr(self.max) + ")"

	def __str__(self):
		assert self.max != bound(0)
		if self in symbolic.keys():
			return symbolic[self]
		if self.max == inf:
			return "{" + str(self.min) + ",}"
		if self.min == self.max:
			return "{" + str(self.min) + "}"
		return "{" + str(self.min) + "," + str(self.max) + "}"

	@classmethod
	def match(cls, string, i):

		def matchAnyOf(string, i, collection):
			for char in collection:
				try:
					return char, static(string, i, char)
				except nomatch:
					pass
			raise nomatch

		def matchInteger(string, i):
			try:
				return 0, static(string, i, "0")
			except nomatch:
				pass

			digit, i = matchAnyOf(string, i, "123456789")
			integer = int(digit)
			try:
				while True:
					digit, i = matchAnyOf(string, i, "0123456789")
					integer *= 10
					integer += int(digit)
			except nomatch:
				return integer, i

		# {2,3}
		try:
			j = static(string, i, "{")
			min, j = matchInteger(string, j)
			j = static(string, j, ",")
			max, j = matchInteger(string, j)
			j = static(string, j, "}")
			return multiplier(bound(min), bound(max)), j
		except nomatch:
			pass

		# {2,}
		try:
			j = static(string, i, "{")
			min, j = matchInteger(string, j)
			j = static(string, j, ",}")
			return multiplier(bound(min), inf), j
		except nomatch:
			pass

		# {2}
		try:
			j = static(string, i, "{")
			min, j = matchInteger(string, j)
			j = static(string, j, "}")
			return multiplier(bound(min), bound(min)), j
		except nomatch:
			pass

		# "?"/"*"/"+"/""
		# we do these in reverse order of symbol length, because
		# that forces "" to be done last
		for key in sorted(symbolic, key=lambda key: -len(symbolic[key])):
			try:
				return key, static(string, i, symbolic[key])
			except nomatch:
				pass

		raise nomatch

	def canmultiplyby(self, other):
		'''
			Multiplication is not well-defined for all pairs of multipliers because
			the resulting possibilities do not necessarily form a continuous range.
			For example:
				{0,x} * {0,y} = {0,x*y}
				{2} * {3} = {6}
				{2} * {1,2} = ERROR

			The proof isn't simple but suffice it to say that {p,p+q} * {r,r+s} is
			equal to {pr, (p+q)(r+s)} only if s=0 or qr+1 >= p. If not, then at least
			one gap appears in the range. The first inaccessible number is (p+q)r + 1.
		'''
		return self.mandatory == zero or \
		self.optional * other.mandatory + bound(1) >= self.mandatory

	def __mul__(self, other):
		'''Multiply this multiplier by another'''
		assert self.canmultiplyby(other)
		return multiplier(self.min * other.min, self.max * other.max)

	def __add__(self, other):
		'''Add two multipliers together'''
		return multiplier(self.min + other.min, self.max + other.max)

	def __sub__(self, other):
		'''
			Subtract another multiplier from this one.
			Caution: multipliers are not totally ordered.
			This operation is not meaningful for all pairs of multipliers.
		'''
		mandatory = self.mandatory - other.mandatory
		optional = self.optional - other.optional
		return multiplier(mandatory, mandatory + optional)

	def canintersect(self, other):
		'''
			Intersection is not well-defined for all pairs of multipliers.
			For example:
				{2,3} & {3,4} = {3}
				{2,} & {1,7} = {2,7}
				{2} & {5} = ERROR
		'''
		return not (self.max < other.min or other.max < self.min)

	def __and__(self, other):
		'''
			Find the intersection of two multipliers: that is, a third multiplier
			expressing the range covered by both of the originals. This is not
			defined for all multipliers.
		'''
		assert self.canintersect(other)
		a = max(self.min, other.min)
		b = min(self.max, other.max)
		return multiplier(a, b)

	def common(self, other):
		'''
			Find the shared part of two multipliers. This is the largest multiplier
			which can be safely subtracted from both the originals. This may
			return the "zero" multiplier.
		'''
		mandatory = min(self.mandatory, other.mandatory)
		optional = min(self.optional, other.optional)
		return multiplier(mandatory, mandatory + optional)

class mult(lego):
	'''
		A mult is a combination of a multiplicand with
		a multiplier (a min and a max). The vast majority of characters in regular
		expressions occur without a specific multiplier, which is implicitly
		equivalent to a min of 1 and a max of 1, but many more have explicit
		multipliers like "*" (min = 0, max = inf) and so on.

		e.g. a, b{2}, c?, d*, [efg]{2,5}, f{2,}, (anysubpattern)+, .*, and so on
	'''

	def __init__(self, cand, ier):
		self.__dict__["multiplicand"] = cand
		self.__dict__["multiplier"]   = ier

	def __eq__(self, other):
		try:
			return self.multiplicand == other.multiplicand \
			and self.multiplier == other.multiplier
		except AttributeError:
			return False

	def __ne__(self, other):
		return not self.__eq__(other)

	def __hash__(self):
		return hash((self.multiplicand, self.multiplier))

	def __repr__(self):
		string = "mult("
		string += repr(self.multiplicand)
		string += ", " + repr(self.multiplier)
		string += ")"
		return string

	@reduce_after
	def __mul__(self, multiplier):
		if multiplier == one:
			return self
		if self.multiplier.canmultiplyby(multiplier):
			return mult(self.multiplicand, self.multiplier * multiplier)
		return mult(pattern(conc(self)), multiplier)

	@reduce_after
	def __add__(self, other):
		return conc(self) + other

	@reduce_after
	def __or__(self, other):
		return conc(self) | other

	def __sub__(self, other):
		'''
			Subtract another mult from this one and return the result.
			The reverse of concatenation. This is a lot trickier.
			e.g. a{4,5} - a{3} = a{1,2}
		'''
		assert other.multiplicand == self.multiplicand
		return mult(self.multiplicand, self.multiplier - other.multiplier)

	def common(self, other):
		'''
			Return the common part of these two mults. This is the largest mult
			which can be safely subtracted from both the originals. The multiplier
			on this mult could be zero: this is the case if, for example, the
			multiplicands disagree.
		'''
		if self.multiplicand == other.multiplicand:
			return mult(self.multiplicand, self.multiplier.common(other.multiplier))

		# Multiplicands disagree, no common part at all.
		return mult(nothing, zero)

	@reduce_after
	def __and__(self, other):
		if hasattr(other, "chars"):
			other = mult(other, one)

		# If two mults are given which have a common multiplicand, the shortcut
		# is just to take the intersection of the two multiplicands.
		try:
			if self.multiplicand == other.multiplicand \
			and self.canintersect(other):
				return mult(self.multiplicand, self.multiplier & other.multiplier)
		except AttributeError:
			# "other" isn't a mult; lacks either a multiplicand or a multiplier.
			# Never mind!
			pass

		# This situation is substantially more complicated if the multiplicand is,
		# for example, a pattern. It's difficult to reason sensibly about this
		# kind of thing.
		return conc(self) & other

	def alphabet(self):
		return {otherchars} | self.multiplicand.alphabet()
	
	def empty(self):
		return self.multiplicand.empty() and self.multiplier.min > bound(0)

	@reduce_after
	def reduce(self):
		# Can't match anything: reduce to nothing
		if self.empty():
			return nothing

		# If our multiplicand is a pattern containing an empty conc()
		# we can pull that "optional" bit out into our own multiplier
		# instead.
		# e.g. (A|B|C|)D -> (A|B|C)?D
		# e.g. (A|B|C|){2} -> (A|B|C){0,2}
		try:
			if emptystring in self.multiplicand.concs \
			and self.multiplier.canmultiplyby(qm):
				return mult(
					pattern(
						*self.multiplicand.concs.difference({emptystring})
					),
					self.multiplier * qm,
				)
		except AttributeError:
			# self.multiplicand has no attribute "concs"; isn't a pattern; never mind
			pass

		# If we have an empty multiplicand, we can only match it
		# zero times
		if self.multiplicand.empty() \
		and self.multiplier.min == bound(0):
			return emptystring

		# Failing that, we have a positive multiplicand which we
		# intend to match zero times. In this case the only possible
		# match is the empty string.
		if self.multiplier == zero:
			return emptystring

		# no point multiplying in the singular
		if self.multiplier == one:
			return self.multiplicand
	
		# Try recursively reducing our internal.
		reduced = self.multiplicand.reduce()
		# "bulk up" smaller lego pieces to pattern if need be
		if hasattr(reduced, "multiplicand"):
			reduced = conc(reduced)
		if hasattr(reduced, "mults"):
			reduced = pattern(reduced)
		if reduced != self.multiplicand:
			return mult(reduced, self.multiplier)

		# If our multiplicand is a pattern containing a single conc
		# containing a single mult, we can separate that out a lot
		# e.g. ([ab])* -> [ab]*
		try:
			if len(self.multiplicand.concs) == 1:
				singleton = [c for c in self.multiplicand.concs][0]
				if len(singleton.mults) == 1:
					singlemult = singleton.mults[0]
					if singlemult.multiplier.canmultiplyby(self.multiplier):
						return mult(
							singlemult.multiplicand,
							singlemult.multiplier * self.multiplier
						)
		except AttributeError:
			# self.multiplicand has no attribute "concs"; isn't a pattern; never mind
			pass

		return self

	def __str__(self):
		# recurse into subpattern
		if hasattr(self.multiplicand, "concs"):
			output = "(" + str(self.multiplicand) + ")"

		else: 
			output = str(self.multiplicand)

		suffix = str(self.multiplier)

		# Pick whatever is shorter/more comprehensible.
		# e.g. "aa" beats "a{2}", "ababab" beats "(ab){3}"
		if self.multiplier.min == self.multiplier.max \
		and len(output) * self.multiplier.min.v <= len(output) + len(suffix):
			return output * self.multiplier.min.v

		return output + suffix

	def fsm(self, alphabet=None):
		from .fsm import epsilon

		if alphabet is None:
			alphabet = self.alphabet()

		# worked example: (min, max) = (5, 7) or (5, inf)
		# (mandatory, optional) = (5, 2) or (5, inf)

		unit = self.multiplicand.fsm(alphabet)
		# accepts e.g. "ab"

		# accepts "ababababab"
		mandatory = unit * self.multiplier.mandatory.v

		# unlimited additional copies
		if self.multiplier.optional == inf:
			optional = unit.star()
			# accepts "(ab)*"

		else:
			optional = epsilon(alphabet) | unit
			# accepts "(ab)?"

			optional *= self.multiplier.optional.v
			# accepts "(ab)?(ab)?"

		return mandatory + optional

	@classmethod
	def match(cls, string, i):
		try:
			j = static(string, i, "(")
			cand, j = pattern.match(string, j)
			j = static(string, j, ")")
		except nomatch:
			cand, j = charclass.match(string, i)

		ier, j = multiplier.match(string, j)
		return mult(cand, ier), j

	def __reversed__(self):
		return mult(reversed(self.multiplicand), self.multiplier)

class conc(lego):
	'''
		A conc (short for "concatenation") is a tuple of mults i.e. an unbroken
		string of mults occurring one after the other.
		e.g. abcde[^fg]*h{4}[a-z]+(subpattern)(subpattern2)
		To express the empty string, use an empty conc, conc().
	'''

	def __init__(self, *mults):
		self.__dict__["mults"] = tuple(mults)

	def __eq__(self, other):
		try:
			return self.mults == other.mults
		except AttributeError:
			return False

	def __ne__(self, other):
		return not self.__eq__(other)

	def __hash__(self):
		return hash(self.mults)

	def __repr__(self):
		string = "conc("
		string += ", ".join(repr(m) for m in self.mults)
		string += ")"
		return string

	@reduce_after
	def __mul__(self, multiplier):
		if multiplier == one:
			return self
		# Have to replace self with a pattern unfortunately
		return pattern(self) * multiplier

	@reduce_after
	def __add__(self, other):
		# other must be a conc too
		if hasattr(other, "chars") or hasattr(other, "concs"):
			other = mult(other, one)
		if hasattr(other, "multiplicand"):
			other = conc(other)

		return conc(*(self.mults + other.mults))

	@reduce_after
	def __or__(self, other):
		return pattern(self) | other

	@reduce_after
	def __and__(self, other):
		return pattern(self) & other

	@reduce_after
	def reduce(self):
		# Can't match anything
		if self.empty():
			return nothing

		# no point concatenating one thing (note: concatenating 0 things is
		# entirely valid)
		if len(self.mults) == 1:
			return self.mults[0]

		# Try recursively reducing our internals
		reduced = [m.reduce() for m in self.mults]
		# "bulk up" smaller lego pieces to concs if need be
		reduced = [
			pattern(x) if hasattr(x, "mults") else x
			for x in reduced
		]
		reduced = [
			mult(x, one) if hasattr(x, "chars") or hasattr(x, "concs") else x
			for x in reduced
		]
		reduced = tuple(reduced)
		if reduced != self.mults:
			return conc(*reduced)

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
					new = self.mults[:i] + (squished,) + self.mults[i+2:]
					return conc(*new)

		# Conc contains (among other things) a *singleton* mult containing a pattern
		# with only one internal conc? Flatten out.
		# e.g. "a(d(ab|a*c))" -> "ad(ab|a*c)"
		# BUT NOT "a(d(ab|a*c)){2,}"
		# AND NOT "a(d(ab|a*c)|y)"
		for i in range(len(self.mults)):
			m = self.mults[i]
			try:
				if m.multiplier == one and len(m.multiplicand.concs) == 1:
					single = [c for c in m.multiplicand.concs][0]
					new = self.mults[:i] + single.mults + self.mults[i+1:]
					return conc(*new)
			except AttributeError:
				# m.multiplicand has no attribute "concs"; isn't a pattern; never mind
				pass

		return self

	def fsm(self, alphabet=None):
		from .fsm import epsilon

		if alphabet is None:
			alphabet = self.alphabet()

		# start with a component accepting only the empty string
		fsm1 = epsilon(alphabet)
		for m in self.mults:
			fsm1 += m.fsm(alphabet)
		return fsm1

	def alphabet(self):
		return {otherchars}.union(*[m.alphabet() for m in self.mults])

	def empty(self):
		for m in self.mults:
			if m.empty():
				return True
		return False

	def __str__(self):
		return "".join(str(m) for m in self.mults)

	@classmethod
	def match(cls, string, i):
		mults = list()
		try:
			while True:
				m, i = mult.match(string, i)
				mults.append(m)
		except nomatch:
			pass
		return conc(*mults), i

	def common(self, other, suffix=False):
		'''
			Return the common prefix of these two concs; that is, the largest conc
			which can be safely beheaded() from the front of both.
			The result could be emptystring.
			"ZYAA, ZYBB" -> "ZY"
			"CZ, CZ" -> "CZ"
			"YC, ZC" -> ""
			
			With the "suffix" flag set, works from the end. E.g.:
			"AAZY, BBZY" -> "ZY"
			"CZ, CZ" -> "CZ"
			"CY, CZ" -> ""
		'''
		mults = []
		
		indices = range(min(len(self.mults), len(other.mults))) # e.g. [0, 1, 2, 3]

		# Work backwards from the end of both concs instead.
		if suffix:
			indices = [-i - 1 for i in indices] # e.g. [-1, -2, -3, -4]

		for i in indices:

			common = self.mults[i].common(other.mults[i])

			# Happens when multiplicands disagree (e.g. "A.common(B)") or if
			# the multiplicand is shared but the common multiplier is zero
			# (e.g. "ABZ*.common(CZ)".)
			if common.multiplier == zero:
				break

			mults.append(common)

			# If we did not remove the entirety of both mults, we cannot continue.
			if common != self.mults[i] or common != other.mults[i]:
				break

		if suffix:
			mults = reversed(mults)

		return conc(*mults)

	def __sub__(self, other):
		'''
			Subtract another conc from this one.
			This is the opposite of concatenation. For example, if ABC + DEF = ABCDEF,
			then logically ABCDEF - DEF = ABC.
		'''
		
		# e.g. self has mults at indices [0, 1, 2, 3, 4, 5, 6] len=7
		# e.g. other has mults at indices [0, 1, 2] len=3
		new = list(self.mults)
		for i in reversed(range(len(other.mults))): # [2, 1, 0]
			# e.g. i = 1, j = 7 - 3 + 1 = 5
			j = len(self.mults) - len(other.mults) + i
			new[j] -= other.mults[i]

			if new[j].multiplier == zero:
				# omit that mult entirely since it has been factored out
				del new[j]

			# If the subtraction is incomplete but there is more to
			# other.mults, then we have a problem. For example, "ABC{2} - BC"
			# subtracts the C successfully but leaves something behind,
			# then tries to subtract the B too, which isn't possible
			else:
				assert i == 0

		return conc(*new)

	def behead(self, other):
		'''
			As with __sub__ but the other way around. For example, if
			ABC + DEF = ABCDEF, then ABCDEF.behead(AB) = CDEF.
		'''
		# Observe that FEDCBA - BA = FEDC.
		return reversed(reversed(self) - reversed(other))

	def __reversed__(self):
		return conc(*reversed([reversed(m) for m in self.mults]))

class pattern(lego):
	'''
		A pattern (also known as an "alt", short for "alternation") is a
		set of concs. A pattern expresses multiple alternate possibilities.
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
		self.__dict__["concs"] = frozenset(concs)

	def __eq__(self, other):
		try:
			return self.concs == other.concs
		except AttributeError:
			return False

	def __ne__(self, other):
		return not self.__eq__(other)

	def __hash__(self):
		return hash(self.concs)

	def __repr__(self):
		string = "pattern("
		string += ", ".join(repr(c) for c in self.concs)
		string += ")"
		return string

	@reduce_after
	def __mul__(self, multiplier):
		if multiplier == one:
			return self
		return mult(self, multiplier)

	@reduce_after
	def __add__(self, other):
		return mult(self, one) + other

	def alphabet(self):
		return {otherchars}.union(*[c.alphabet() for c in self.concs])

	def empty(self):
		for c in self.concs:
			if not c.empty():
				return False
		return True

	@reduce_after
	def __and__(self, other):
		# A deceptively simple method for an astoundingly difficult operation
		alphabet = self.alphabet() | other.alphabet()
	
		# Which means that we can build finite state machines sharing that alphabet
		combined = self.fsm(alphabet) & other.fsm(alphabet)
		return combined.lego()

	@reduce_after
	def __or__(self, other):
		# other must be a pattern too
		if hasattr(other, "chars"):
			other = mult(other, one)
		if hasattr(other, "multiplicand"):
			other = conc(other)
		if hasattr(other, "mults"):
			other = pattern(other)

		return pattern(*(self.concs | other.concs))

	def __str__(self):
		assert len(self.concs) >= 1

		# take the alternation of the input collection of regular expressions.
		# i.e. jam "|" between each element

		# 1+ elements.
		return "|".join(sorted(str(c) for c in self.concs))

	@reduce_after
	def reduce(self):
		# emptiness
		if self.empty():
			return nothing

		# If one of our internal concs is empty, remove it
		for c in self.concs:
			if c.empty():
				new = self.concs - {c}
				return pattern(*new)

		# no point alternating among one possibility
		if len(self.concs) == 1:
			return [e for e in self.concs][0]

		# Try recursively reducing our internals first.
		reduced = [c.reduce() for c in self.concs]
		# "bulk up" smaller lego pieces to concs if need be
		reduced = [
			mult(x, one) if hasattr(x, "chars") or hasattr(x, "concs") else x
			for x in reduced
		]
		reduced = [
			conc(x) if hasattr(x, "multiplicand") else x
			for x in reduced
		]
		reduced = frozenset(reduced)
		if reduced != self.concs:
			return pattern(*reduced)

		# If this pattern contains several concs each containing just 1 mult
		# each containing just a charclass, with a multiplier of 1,
		# then we can merge those branches together.
		# e.g. "0|[1-9]|ab" -> "[0-9]|ab"
		changed = False
		merger = None
		rest = []
		for c in self.concs:
			if len(c.mults) == 1 \
			and c.mults[0].multiplier == one \
			and hasattr(c.mults[0].multiplicand, "chars"):
				if merger is None:
					merger = c.mults[0].multiplicand
				else:
					merger |= c.mults[0].multiplicand
					changed = True
			else:
				rest.append(c)
		if changed:
			rest.append(conc(mult(merger, one)))
			return pattern(*rest)

		# If one of the present pattern's concs is the empty string, and
		# there is another conc with a single mult whose lower bound is 0, we
		# can omit the empty string.
		# E.g. "|(ab)*|def" => "(ab)*|def".
		# If there is another conc with a single mult whose lower bound is 1,
		# we can merge the empty string into that.
		# E.g. "|(ab)+|def" => "(ab)*|def".
		if conc() in self.concs:
			for c in self.concs:
				if len(c.mults) != 1:
					continue
				m = c.mults[0]
				if m.multiplier.min == bound(0):
					rest = self.concs - {conc()}
					return pattern(*rest)
				if m.multiplier.min == bound(1):
					rest = self.concs - {conc(), c} | {m * qm}
					return pattern(*rest)

		# If the present pattern's concs all have a common prefix, split
		# that out. This increases the depth of the object
		# but it is still arguably simpler/ripe for further reduction
		# e.g. "abc|ade" -> a(bc|de)"
		prefix = self._commonconc()
		if prefix != emptystring:
			leftovers = self.behead(prefix)
			mults = prefix.mults + (mult(leftovers, one),)
			return conc(*mults)

		# Same but for suffixes.
		# e.g. "xyz|stz -> (xy|st)z"
		suffix = self._commonconc(suffix=True)
		if suffix != emptystring:
			leftovers = self - suffix
			mults = (mult(leftovers, one),) + suffix.mults
			return conc(*mults)

		return self

	@classmethod
	def match(cls, string, i):
		concs = list()

		# first one
		c, i = conc.match(string, i)
		concs.append(c)

		# the rest
		while True:
			try:
				i = static(string, i, "|")
				c, i = conc.match(string, i)
				concs.append(c)
			except nomatch:
				return pattern(*concs), i

	def __sub__(self, other):
		'''
			The opposite of concatenation. Remove a common suffix from the present
			pattern; that is, from each of its constituent concs.
			AYZ|BYZ|CYZ - YZ = A|B|C.
		'''
		return pattern(*[c - other for c in self.concs])

	def behead(self, other):
		'''
			Like __sub__ but the other way around. Remove a common prefix from the
			present pattern; that is, from each of its constituent concs.
			ZA|ZB|ZC.behead(Z) = A|B|C
		'''
		return pattern(*[c.behead(other) for c in self.concs])

	def _commonconc(self, suffix=False):
		'''
			Find the longest conc which acts as prefix to every conc in this pattern.
			This could be the empty string. Return the common prefix along with all
			the leftovers after truncating that common prefix from each conc.
			"ZA|ZB|ZC" -> "Z", "(A|B|C)"
			"ZA|ZB|ZC|Z" -> "Z", "(A|B|C|)"
			"CZ|CZ" -> "CZ", "()"

			If "suffix" is True, the same result but for suffixes.
		'''
		assert len(self.concs) >= 1

		from functools import reduce
		return reduce(
			lambda x, y: x.common(y, suffix=suffix),
			self.concs
		)

	def fsm(self, alphabet=None):
		from .fsm import null

		if alphabet is None:
			alphabet = self.alphabet()

		fsm1 = null(alphabet)
		for c in self.concs:
			fsm1 |= c.fsm(alphabet)
		return fsm1

	def __reversed__(self):
		return pattern(*(reversed(c) for c in self.concs))

# Special and useful values go here.

# We need to add an extra character in the alphabet which can stand for
# "everything else". For example, if the regex is "abc.", then at the moment
# our alphabet is {"a", "b", "c"}. But "." could match anything else not yet
# specified. This extra letter stands for that ("[^abc]" in this case).
otherchars = None

# Standard character classes
w = charclass("0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz")
d = charclass("0123456789")
s = charclass("\t\n\v\f\r ")
W = ~w
D = ~d
S = ~s
dot = ~charclass()

# This charclasses expresses "no possibilities at all"
# and can never match anything.
nothing = charclass()

# Textual representations of standard character classes
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

# Use this for cases where no upper bound is needed
inf = bound(None)

# Preset multipliers. These get used ALL THE TIME in unit tests
zero = multiplier(bound(0), bound(0)) # has some occasional uses
qm   = multiplier(bound(0), bound(1))
one  = multiplier(bound(1), bound(1))
star = multiplier(bound(0), inf)
plus = multiplier(bound(1), inf)

# Symbol lookup table for preset multipliers.
symbolic = {
	qm   : "?",
	one  : "" ,
	star : "*",
	plus : "+",
}

# A very special conc expressing the empty string, ""
emptystring = conc()

# Unit tests.
if __name__ == '__main__':

	# Allow relative imports when executing within package directory, for running tests
	import sys, os
	sys.path.insert( 0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
	import greenery
	__package__ = str("greenery")
	del sys, os


	# "AAZY, BBZY" -> "ZY"
	assert conc(
		mult(charclass("A"), one),
		mult(charclass("A"), one),
		mult(charclass("Z"), one),
		mult(charclass("Y"), one),
	).common(
		conc(
			mult(charclass("B"), one),
			mult(charclass("B"), one),
			mult(charclass("Z"), one),
			mult(charclass("Y"), one),
		),
		suffix=True
	) == conc(
		mult(charclass("Z"), one),
		mult(charclass("Y"), one),
	)

	# "CZ, CZ" -> "CZ"
	assert conc(
		mult(charclass("C"), one),
		mult(charclass("Z"), one),
	).common(
		conc(
			mult(charclass("C"), one),
			mult(charclass("Z"), one),
		),
		suffix=True
	) == conc(
		mult(charclass("C"), one),
		mult(charclass("Z"), one),
	)

	# "CY, CZ" -> ""
	assert conc(
		mult(charclass("C"), one),
		mult(charclass("Y"), one),
	).common(
		conc(
			mult(charclass("C"), one),
			mult(charclass("Z"), one),
		),
		suffix=True
	) == emptystring

	# AZ, BZ -> Z
	assert conc(
		mult(charclass("A"), one),
		mult(charclass("Z"), one),
	).common(
		conc(
			mult(charclass("B"), one),
			mult(charclass("Z"), one),
		),
		suffix=True
	) == conc(
		mult(charclass("Z"), one)
	)

	# AZ*, BZ -> ()
	assert conc(
		mult(charclass("A"), one),
		mult(charclass("Z"), star),
	).common(
		conc(
			mult(charclass("B"), one),
			mult(charclass("Z"), one),
		),
		suffix=True
	) == emptystring

	# A, B -> no common part
	assert conc(
		mult(charclass("A"), one),
	).common(
		conc(
			mult(charclass("B"), one),
		),
		suffix=True
	) == emptystring
	
	# Conc subtraction

	# AZ - Z = A
	assert conc(
		mult(charclass("A"), one),
		mult(charclass("Z"), one),
	) - conc(
		mult(charclass("Z"), one)
	) == conc(
		mult(charclass("A"), one)
	)

	# ABXY+Z - XY+Z = AB
	assert conc(
		mult(charclass("A"), one),
		mult(charclass("B"), one),
		mult(charclass("X"), one),
		mult(charclass("Y"), plus),
		mult(charclass("Z"), one),
	) - conc(
		mult(charclass("X"), one),
		mult(charclass("Y"), plus),
		mult(charclass("Z"), one),
	) == conc(
		mult(charclass("A"), one),
		mult(charclass("B"), one),
	)

	# ABXY+Z.behead(ABXY+) = Z
	assert conc(
		mult(charclass("A"), one),
		mult(charclass("B"), one),
		mult(charclass("X"), one),
		mult(charclass("Y"), plus),
		mult(charclass("Z"), one),
	).behead(
		conc(
			mult(charclass("A"), one),
			mult(charclass("B"), one),
			mult(charclass("X"), one),
			mult(charclass("Y"), plus),
		)
	)== conc(
		mult(charclass("Z"), one),
	)

	# X{2}Y+Z.behead(XY+) = exception
	try:
		conc(
			mult(charclass("X"), multiplier),
			mult(charclass("Y"), plus),
			mult(charclass("Z"), one),
		).behead(
			conc(
				mult(charclass("X"), one),
				mult(charclass("Y"), plus),
			)
		)
		assert(False)
	except:
		pass

	# A - () = A
	assert conc(
		mult(charclass("A"), one),
	) - emptystring == conc(
		mult(charclass("A"), one),
	)

	# Odd bug with ([bc]*c)?[ab]*
	int5A = mult(charclass("bc"), star).fsm({"a", "b", "c", otherchars})
	assert int5A.accepts("")
	int5B = mult(charclass("c"), one).fsm({"a", "b", "c", otherchars})
	assert int5B.accepts("c")
	int5C = int5A + int5B
	assert (int5A + int5B).accepts("c")

	# Empty mult suppression
	assert conc(
		mult(nothing, one), # this mult can never actually match anything
		mult(charclass("0"), one),
		mult(charclass("0123456789"), one),
	).reduce() == nothing
	assert conc(
		mult(pattern(), one), # this mult can never actually match anything
		mult(charclass("0"), one),
		mult(charclass("0123456789"), one),
	).reduce() == nothing

	# Empty conc suppression in patterns.
	assert pattern(
		conc(
			mult(nothing, one), # this mult can never actually match anything
			mult(charclass("0"), one),
			mult(charclass("0123456789"), one),
		) # so neither can this conc
	).reduce() == nothing
	assert pattern(
		conc(
			mult(pattern(), one), # this mult can never actually match anything
			mult(charclass("0"), one),
			mult(charclass("0123456789"), one),
		) # so neither can this conc
	).reduce() == nothing

	# Empty pattern suppression in mults
	assert mult(nothing, qm).reduce() == emptystring
	assert mult(pattern(), qm).reduce() == emptystring

	# empty pattern behaviour
	assert pattern().reduce() == charclass()

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
				d, multiplier(bound(2), bound(2))
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

	# "(aa)".behead("a") = "a"
	assert pattern(
		conc(
			mult(charclass("a"), one),
			mult(charclass("a"), one),
		),
	).behead(conc(mult(charclass("a"), one))) == pattern(
		conc(
			mult(charclass("a"), one)
		),
	)

	# (abc|aa).behead(a) = (a|bc)
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
	).behead(conc(mult(charclass("a"), one))) == pattern(
		conc(
			mult(charclass("a"), one),
		),
		conc(
			mult(charclass("b"), one),
			mult(charclass("c"), one),
		),
	)

	# (cf{1,2}|cf) - c = (f|f?)
	assert pattern(
		conc(
			mult(charclass("c"), one),
			mult(charclass("f"), multiplier(bound(1), bound(2))),
		),
		conc(
			mult(charclass("c"), one),
			mult(charclass("f"), one),
		),
	).behead(conc(mult(charclass("c"), one))) == pattern(
		conc(
			mult(charclass("f"), multiplier(bound(1), bound(2))),
		),
		conc(
			mult(charclass("f"), one),
		),
	)

	# pattern._commonconc() tests

	# aa, aa -> aa
	assert pattern(
		conc(
			mult(charclass("a"), one),
			mult(charclass("a"), one),
		),
		conc(
			mult(charclass("a"), one),
			mult(charclass("a"), one),
		),
	)._commonconc() == conc(
		mult(charclass("a"), one),
		mult(charclass("a"), one),
	)

	# (aa|aa).behead(aa) = ()
	assert pattern(
		conc(
			mult(charclass("a"), one),
			mult(charclass("a"), one),
		),
		conc(
			mult(charclass("a"), one),
			mult(charclass("a"), one),
		),
	).behead(
		conc(
			mult(charclass("a"), one),
			mult(charclass("a"), one),
		)
	) == pattern(emptystring)

	# abc, aa -> a
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
	)._commonconc() == conc(
		mult(charclass("a"), one),
	)

	# (abc|aa).behead(a) = (a|bc)
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
	).behead(
		conc(
			mult(charclass("a"), one),
		)
	) == pattern(
		conc(
			mult(charclass("a"), one),
		),
		conc(
			mult(charclass("b"), one),
			mult(charclass("c"), one),
		),
	)

	# a, bc -> emptystring
	assert pattern(
		conc(
			mult(charclass("a"), one),
		),
		conc(
			mult(charclass("b"), one),
			mult(charclass("c"), one),
		),
	)._commonconc() == emptystring

	# (a|bc).behead(emptystring) = (a|bc)
	assert pattern(
		conc(
			mult(charclass("a"), one),
		),
		conc(
			mult(charclass("b"), one),
			mult(charclass("c"), one),
		),
	).behead(emptystring) == pattern(
		conc(
			mult(charclass("a"), one),
		),
		conc(
			mult(charclass("b"), one),
			mult(charclass("c"), one),
		),
	)

	# cf{1,2}, cf -> cf, (f?|)
	assert pattern(
		conc(
			mult(charclass("c"), one),
			mult(charclass("f"), multiplier(bound(1), bound(2))),
		),
		conc(
			mult(charclass("c"), one),
			mult(charclass("f"), one),
		),
	)._commonconc() == conc(
		mult(charclass("c"), one),
		mult(charclass("f"), one),
	)

	# (cf{1,2}|cf).behead(cf) = (f?|)
	assert pattern(
		conc(
			mult(charclass("c"), one),
			mult(charclass("f"), multiplier(bound(1), bound(2))),
		),
		conc(
			mult(charclass("c"), one),
			mult(charclass("f"), one),
		),
	).behead(
		conc(
			mult(charclass("c"), one),
			mult(charclass("f"), one),
		)
	) == pattern(
		emptystring,
		conc(
			mult(charclass("f"), qm),
		),
	)

	# ZA|ZB|ZC -> Z
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
	)._commonconc() == conc(mult(charclass("Z"), one))

	# ZA|ZB|ZC.behead(Z) = A|B|C
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
	).behead(
		conc(mult(charclass("Z"), one))
	) == pattern(
		conc(mult(charclass("A"), one)),
		conc(mult(charclass("B"), one)),
		conc(mult(charclass("C"), one)),
	)

	# Z+A|ZB|ZZC -> Z
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
	)._commonconc() == conc(mult(charclass("Z"), one))

	# Z+A|ZB|ZZC.behead(Z) = Z*A|B|ZC
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
	).behead(
		conc(mult(charclass("Z"), one))
	) == pattern(
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
	)

	# a{2}b|a+c -> a
	assert pattern(
		conc(
			mult(charclass("a"), multiplier(bound(2), bound(2))),
			mult(charclass("b"), one),
		),
		conc(
			mult(charclass("a"), plus),
			mult(charclass("c"), one),
		)
	)._commonconc() == conc(mult(charclass("a"), one))

	# a{2}b|a+c.behead(a) = (ab|a*c)
	assert pattern(
		conc(
			mult(charclass("a"), multiplier(bound(2), bound(2))),
			mult(charclass("b"), one),
		),
		conc(
			mult(charclass("a"), plus),
			mult(charclass("c"), one),
		)
	).behead(
		conc(mult(charclass("a"), one))
	) == pattern(
		conc(
			mult(charclass("a"), one),
			mult(charclass("b"), one),
		),
		conc(
			mult(charclass("a"), star),
			mult(charclass("c"), one),
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

	# str(charclass)
	assert str(w) == "\\w"
	assert str(d) == "\\d"
	assert str(s) == "\\s"
	assert str(charclass("a")) == "a"
	assert str(charclass("{")) == "\\{"
	assert str(charclass("\t")) == "\\t"
	assert str(charclass("ab")) == "[ab]"
	assert str(charclass("a{")) == "[a{]"
	assert str(charclass("a\t")) == "[\\ta]"
	assert str(charclass("a-")) == "[\\-a]"
	assert str(charclass("a[")) == "[\\[a]"
	assert str(charclass("a]")) == "[\\]a]"
	assert str(charclass("ab")) == "[ab]"
	assert str(charclass("abc")) == "[abc]"
	assert str(charclass("abcd")) == "[a-d]"
	assert str(charclass("abcdfghi")) == "[a-df-i]"
	assert str(charclass("^")) == "^"
	assert str(charclass("\\")) == "\\\\"
	assert str(charclass("a^")) == "[\\^a]"
	assert str(charclass("0123456789a")) == "[0-9a]"
	assert str(charclass("\t\n\v\f\r A")) == "[\\t\\n\\v\\f\\r A]"
	assert str(charclass("0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz|")) == "[0-9A-Z_a-z|]"
	assert str(W) == "\\W"
	assert str(D) == "\\D"
	assert str(S) == "\\S"
	assert str(dot) == "."
	assert str(~charclass("")) == "."
	assert str(~charclass("a")) == "[^a]"
	assert str(~charclass("{")) == "[^{]"
	assert str(~charclass("\t")) == "[^\\t]"
	assert str(~charclass("^")) == "[^\\^]"

	# charclass parsing
	assert charclass.match("a", 0) == (charclass("a"), 1)
	assert charclass.match("aa", 1) == (charclass("a"), 2)
	assert charclass.match("a$", 1) == (charclass("$"), 2)
	assert charclass.match(".", 0) == (dot, 1)
	try:
		charclass.match("[", 0)
		assert False
	except IndexError:
		pass
	try:
		charclass.match("a", 1)
		assert False
	except nomatch:
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

	# charclass intersection
	# [ab] n [bc] = [b]
	assert charclass("ab") & charclass("bc") == charclass("b")
	# [ab] n [^bc] = [a]
	assert charclass("ab") & ~charclass("bc") == charclass("a")
	# [^ab] n [bc] = [c]
	assert ~charclass("ab") & charclass("bc") == charclass("c")
	# [^ab] n [^bc] = [^abc]
	assert ~charclass("ab") & ~charclass("bc") == ~charclass("abc")

	# mult equality
	assert mult(charclass("a"), one) == mult(charclass("a"), one)
	assert mult(charclass("a"), one) != mult(charclass("b"), one)
	assert mult(charclass("a"), one) != mult(charclass("a"), qm)
	assert mult(charclass("a"), one) != mult(charclass("a"), multiplier(bound(1), bound(2)))

	# str(mult) tests
	a = charclass("a")
	assert str(mult(a, one)) == "a"
	assert str(mult(a, multiplier(bound(2), bound(2)))) == "aa"
	assert str(mult(a, multiplier(bound(3), bound(3)))) == "aaa"
	assert str(mult(a, multiplier(bound(4), bound(4)))) == "aaaa"
	assert str(mult(a, multiplier(bound(5), bound(5)))) == "a{5}"
	assert str(mult(a, qm)) == "a?"
	assert str(mult(a, star)) == "a*"
	assert str(mult(a, plus)) == "a+"
	assert str(mult(a, multiplier(bound(2), bound(5)))) == "a{2,5}"
	assert str(bound(2)) == "2"
	assert str(inf) == ""
	assert str(multiplier(bound(2), inf)) == "{2,}"
	assert str(mult(a, multiplier(bound(2), inf))) == "a{2,}"
	assert str(mult(d, one)) == "\\d"
	assert str(mult(d, multiplier(bound(2), bound(2)))) == "\\d\\d"
	assert str(mult(d, multiplier(bound(3), bound(3)))) == "\\d{3}"

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
		mult(charclass("h"), multiplier(bound(5), bound(5))),
		15
	)
	assert mult.match("abcde[^fg]*h{5}[a-z]+T{1,}", 15) == (
		mult(charclass("abcdefghijklmnopqrstuvwxyz"), plus),
		21
	)
	assert mult.match("abcde[^fg]*h{5}[a-z]+T{2,}", 21) == (
		mult(charclass("T"), multiplier(bound(2), inf)),
		26
	)

	# mult.reduce() tests

	# mult -> mult
	# mult -> charclass
	assert mult(charclass("a"), one).reduce() == charclass("a")
	assert mult(charclass("a"), qm).reduce() == mult(charclass("a"), qm)
	assert mult(charclass("a"), zero).reduce() == emptystring
	assert mult(nothing, one).reduce() == nothing
	assert mult(nothing, qm).reduce() == emptystring
	assert mult(nothing, zero).reduce() == emptystring
	assert mult(nothing, multiplier(bound(0), bound(5))).reduce() == emptystring
	assert mult(pattern(), one).reduce() == nothing
	assert mult(pattern(), qm).reduce() == emptystring
	assert mult(pattern(), zero).reduce() == emptystring
	assert mult(pattern(), multiplier(bound(0), bound(5))).reduce() == emptystring

	# mult contains a pattern containing an empty conc? Pull the empty
	# part out where it's external
	# e.g. (a|b*|) -> (a|b*)?
	assert mult(
		pattern(
			conc(mult(charclass("a"), one)),
			conc(mult(charclass("b"), star)),
			emptystring
		), one
	).reduce() == mult(
		pattern(
			conc(mult(charclass("a"), one)),
			conc(mult(charclass("b"), star)),
		), qm
	)

	# e.g. (a|b*|)c -> (a|b*)?c
	assert conc(
		mult(
			pattern(
				conc(mult(charclass("a"), one)),
				conc(mult(charclass("b"), star)),
				emptystring
			), one
		),
		mult(charclass("c"), one),
	).reduce() == conc(
		mult(
			pattern(
				conc(mult(charclass("a"), one)),
				conc(mult(charclass("b"), star)),
			), qm
		),
		mult(charclass("c"), one),
	)


	# This happens even if emptystring is the only thing left inside the mult
	assert mult(
		pattern(
			emptystring
		), one
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
				mult(charclass("c"), multiplier(bound(1), bound(2)))
			)
		), multiplier(bound(3), bound(4))
	).reduce() == mult(charclass("c"), multiplier(bound(3), bound(8)))

	# recursive mult reduction
	# (a|b)* -> [ab]*
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
		multiplier(bound(4), bound(5))
	) - mult(
		charclass("a"),
		multiplier(bound(3), bound(3))
	) == mult(
		charclass("a"),
		multiplier(bound(1), bound(2))
	)

	# conc equality
	assert conc(mult(charclass("a"), one)) == conc(mult(charclass("a"), one))
	assert conc(mult(charclass("a"), one)) != conc(mult(charclass("b"), one))
	assert conc(mult(charclass("a"), one)) != conc(mult(charclass("a"), qm))
	assert conc(mult(charclass("a"), one)) != conc(mult(charclass("a"), multiplier(bound(1), bound(2))))
	assert conc(mult(charclass("a"), one)) != emptystring

	# str(conc) tests
	assert str(conc(
		mult(charclass("a"), one),
		mult(charclass("b"), one),
		mult(charclass("c"), one),
		mult(charclass("d"), one),
		mult(charclass("e"), one),
		mult(~charclass("fg"), star),
		mult(charclass("h"), multiplier(bound(5), bound(5))),
		mult(charclass("abcdefghijklmnopqrstuvwxyz"), plus),
	)) == "abcde[^fg]*h{5}[a-z]+"

	# conc parsing
	assert conc.match("abcde[^fg]*h{5}[a-z]+", 0) == (
		conc(
			mult(charclass("a"), one),
			mult(charclass("b"), one),
			mult(charclass("c"), one),
			mult(charclass("d"), one),
			mult(charclass("e"), one),
			mult(~charclass("fg"), star),
			mult(charclass("h"), multiplier(bound(5), bound(5))),
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
			mult(charclass("0123456789"), multiplier(bound(4), bound(4))),
			mult(charclass("-"), one),
			mult(charclass("0123456789"), multiplier(bound(2), bound(2))),
			mult(charclass("-"), one),
			mult(charclass("0123456789"), multiplier(bound(2), bound(2))),
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
		mult(charclass("a"), multiplier(bound(3), bound(4))),
	).reduce() == mult(charclass("a"), multiplier(bound(3), bound(4)))
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
		mult(charclass("$%^"), multiplier(bound(1), bound(2))),
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

	# str(pattern)
	assert str(pattern(
		conc(mult(charclass("a"), one)),
		conc(mult(charclass("b"), one)),
	)) == "a|b"
	assert str(pattern(
		conc(mult(charclass("a"), one)),
		conc(mult(charclass("a"), one)),
	)) == "a"
	assert str(pattern(
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
	)) == "abc|def(ghi|jkl)"

	# pattern.reduce() tests

	# pattern -> pattern
	# (ab|cd) -> (ab|cd)
	assert pattern(
		conc(
			mult(charclass("a"), multiplier(bound(2), bound(2))),
			mult(charclass("b"), multiplier(bound(2), bound(2))),
		),
		conc(
			mult(charclass("c"), multiplier(bound(2), bound(2))),
			mult(charclass("d"), multiplier(bound(2), bound(2))),
		),
	).reduce() == pattern(
		conc(
			mult(charclass("a"), multiplier(bound(2), bound(2))),
			mult(charclass("b"), multiplier(bound(2), bound(2))),
		),
		conc(
			mult(charclass("c"), multiplier(bound(2), bound(2))),
			mult(charclass("d"), multiplier(bound(2), bound(2))),
		),
	)

	# pattern -> conc
	assert pattern(
		conc(
			mult(charclass("a"), multiplier(bound(2), bound(2))),
			mult(charclass("b"), multiplier(bound(2), bound(2))),
		),
	).reduce() == conc(
		mult(charclass("a"), multiplier(bound(2), bound(2))),
		mult(charclass("b"), multiplier(bound(2), bound(2))),
	)

	# pattern -> mult
	assert pattern(
		conc(
			mult(charclass("a"), multiplier(bound(2), bound(2))),
		),
	).reduce() == mult(charclass("a"), multiplier(bound(2), bound(2)))

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
		conc(mult(charclass("a"), multiplier(bound(5), bound(7)))),
	).reduce() == pattern(
		conc(mult(charclass("0123456789"), one)),
		conc(mult(charclass("a"), multiplier(bound(5), bound(7)))),
	)

	# recursive pattern reduction
	assert pattern(
		conc(mult(charclass("0"), one)),
		conc(
			mult(
				pattern(
					conc(mult(charclass("0"), one)),
					conc(mult(charclass("123456789"), one)),
					conc(mult(charclass("a"), multiplier(bound(5), bound(7)))),
				), one
			)
		)
	).reduce() == pattern(
		conc(mult(charclass("0"), one)),
		conc(
			mult(
				pattern(
					conc(mult(charclass("0123456789"), one)),
					conc(mult(charclass("a"), multiplier(bound(5), bound(7)))),
				), one
			)
		)
	)

	# common prefix reduction of pattern
	# a{2}b|a+c -> a{2}(ab|a*c)
	assert pattern(
		conc(
			mult(charclass("a"), multiplier(bound(2), bound(2))),
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
	assert charclass("a") * multiplier(bound(1), bound(3)) == mult(charclass("a"), multiplier(bound(1), bound(3)))
	# a * {4,} = a{4,}
	assert charclass("a") * multiplier(bound(4), inf) == mult(charclass("a"), multiplier(bound(4), inf))

	# mult multiplication
	# a{2,3} * 1 = a{2,3}
	assert mult(
		charclass("a"), multiplier(bound(2), bound(3))
	) * one == mult(charclass("a"), multiplier(bound(2), bound(3)))
	# a{2,3} * {4,5} = a{8,15}
	assert mult(
		charclass("a"), multiplier(bound(2), bound(3))
	) * multiplier(bound(4), bound(5)) == mult(charclass("a"), multiplier(bound(8), bound(15)))
	# a{2,} * {2,} = a{4,}
	assert mult(
		charclass("a"), multiplier(bound(2), inf)
	) * multiplier(bound(2), inf) == mult(charclass("a"), multiplier(bound(4), inf))

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
	) * multiplier(bound(2), bound(3)) == mult(
		pattern(
			conc(
				mult(charclass("a"), one),
				mult(charclass("b"), qm),
			),
			conc(
				mult(charclass("b"), one),
				mult(charclass("a"), qm),
			),
		), multiplier(bound(2), bound(3))
	)

	# bound class tests

	assert min(bound(0), inf) == bound(0)
	assert min(bound(1), inf) == bound(1)
	assert qm.mandatory == bound(0)
	assert qm.optional == bound(1)

	# multiplier intersection operator tests
	assert zero.common(zero) == zero
	assert zero.common(qm  ) == zero
	assert zero.common(one ) == zero
	assert zero.common(star) == zero
	assert zero.common(plus) == zero
	assert qm  .common(zero) == zero
	assert qm  .common(qm  ) == qm
	assert qm  .common(one ) == zero
	assert qm  .common(star) == qm
	assert qm  .common(plus) == qm
	assert one .common(zero) == zero
	assert one .common(qm  ) == zero
	assert one .common(one ) == one
	assert one .common(star) == zero
	assert one .common(plus) == one
	assert star.common(zero) == zero
	assert star.common(qm  ) == qm
	assert star.common(one ) == zero
	assert star.common(star) == star
	assert star.common(plus) == star
	assert plus.common(zero) == zero
	assert plus.common(qm  ) == qm
	assert plus.common(one ) == one
	assert plus.common(star) == star
	assert plus.common(plus) == plus

	# a{3,4}, a{2,5} -> a{2,3} (with a{1,1}, a{0,2} left over)
	assert multiplier(bound(3), bound(4)).common(multiplier(bound(2), bound(5))) == multiplier(bound(2), bound(3))
	assert multiplier(bound(3), bound(4)) - multiplier(bound(2), bound(3)) == one
	assert multiplier(bound(2), bound(5)) - multiplier(bound(2), bound(3)) == multiplier(bound(0), bound(2))

	# a{2,}, a{1,5} -> a{1,5} (with a{1,}, a{0,0} left over)
	assert multiplier(bound(2), inf).common(multiplier(bound(1), bound(5))) == multiplier(bound(1), bound(5))
	assert multiplier(bound(2), inf) - multiplier(bound(1), bound(5)) == plus
	assert multiplier(bound(1), bound(5)) - multiplier(bound(1), bound(5)) == zero

	# a{3,}, a{2,} -> a{2,} (with a, epsilon left over)
	assert multiplier(bound(3), inf).common(multiplier(bound(2), inf)) == multiplier(bound(2), inf)
	assert multiplier(bound(3), inf) - multiplier(bound(2), inf) == one
	assert multiplier(bound(2), inf) - multiplier(bound(2), inf) == zero

	# a{3,}, a{3,} -> a{3,} (with zero, zero left over)
	assert multiplier(bound(3), inf).common(multiplier(bound(3), inf)) == multiplier(bound(3), inf)
	assert multiplier(bound(3), inf) - multiplier(bound(3), inf) == zero

	# mult intersection ("&") tests
	# a & b? = nothing
	assert mult(charclass("a"), one) & mult(charclass("b"), qm) == charclass()
	assert mult(charclass("a"), one) & mult(charclass("b"), qm) == nothing
	# a & a? = nothing
	assert mult(charclass('a'), one).reduce() == charclass("a")
	assert mult(charclass("a"), one) & mult(charclass("a"), qm) == charclass("a")
	# a{2} & a{2,} = a{2}
	assert mult(charclass("a"), multiplier(bound(2), bound(2))) \
	& mult(charclass("a"), multiplier(bound(2), inf)) \
	== mult(charclass("a"), multiplier(bound(2), bound(2)))
	# a & b -> no intersection.
	assert mult(charclass("a"), one) & mult(charclass("b"), one) == nothing
	# a & a -> a
	assert mult(charclass("a"), one) & mult(charclass("a"), one) == charclass("a")
	# a* & a -> no intersection
	assert mult(charclass("a"), star) & mult(charclass("a"), one) == charclass("a")
	# a* & b* -> emptystring
	assert mult(charclass("a"), star) & mult(charclass("b"), star) == emptystring
	# a* & a+ -> a+
	assert mult(charclass("a"), star) & mult(charclass("a"), plus) == mult(charclass("a"), plus)
	# aa & aaaa -> []
	assert mult(charclass("a"), multiplier(bound(2), bound(2))) \
	& mult(charclass("a"), multiplier(bound(4), bound(4))) \
	== nothing

	# a{3,4} & a{2,5} -> a{2,3}
	assert mult(
		charclass("a"), multiplier(bound(3), bound(4))
	).common(mult(
		charclass("a"), multiplier(bound(2), bound(5))
	)) == mult(charclass("a"), multiplier(bound(2), bound(3)))

	# a{2,} & a{1,5} -> a{1,5}
	assert mult(
		charclass("a"), multiplier(bound(2), inf)
	).common(mult(
		charclass("a"), multiplier(bound(1), bound(5))
	)) == mult(charclass("a"), multiplier(bound(1), bound(5)))

	# a{3,}, a{2,} -> a{2,} (with a, epsilon left over)
	assert mult(
		charclass("a"), multiplier(bound(3), inf)
	).common(mult(
		charclass("a"), multiplier(bound(2), inf)
	)) == mult(charclass("a"), multiplier(bound(2), inf))

	# a{3,}, a{3,} -> a{3,} (with inf, inf left over)
	assert mult(
		charclass("a"), multiplier(bound(3), inf)
	) & mult(
		charclass("a"), multiplier(bound(3), inf)
	) == mult(charclass("a"), multiplier(bound(3), inf))

	# pattern._commonconc(suffix=True) tests

	# a | bc -> emptystring
	assert pattern(
		conc(
			mult(charclass("a"), one),
		),
		conc(
			mult(charclass("b"), one),
			mult(charclass("c"), one),
		),
	)._commonconc(suffix=True) == emptystring

	# (a|bc) - () = (a|bc)
	assert pattern(
		conc(
			mult(charclass("a"), one),
		),
		conc(
			mult(charclass("b"), one),
			mult(charclass("c"), one),
		),
	) - emptystring == pattern(
		conc(
			mult(charclass("a"), one),
		),
		conc(
			mult(charclass("b"), one),
			mult(charclass("c"), one),
		),
	)

	# (aa|bca) -> a
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
	)._commonconc(suffix=True) == conc(mult(charclass("a"), one))

	# (aa|bca) - a = (a|bc)
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
	) - conc(mult(charclass("a"), one)) == pattern(
		conc(
			mult(charclass("a"), one),
		),
		conc(
			mult(charclass("b"), one),
			mult(charclass("c"), one),
		),
	)

	# xyza | abca | a -> a
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
	)._commonconc(suffix=True) == conc(mult(charclass("a"), one))

	# (xyza|abca|a) - a = (xyz|abc|)
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
	) - conc(mult(charclass("a"), one)) == pattern(
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
	)

	# f{2,3}c, fc -> fc
	assert pattern(
		conc(
			mult(charclass("f"), multiplier(bound(2), bound(3))),
			mult(charclass("c"), one),
		),
		conc(
			mult(charclass("f"), one),
			mult(charclass("c"), one),
		),
	)._commonconc(suffix=True) == conc(
		mult(charclass("f"), one),
		mult(charclass("c"), one),
	)

	# (f{2,3}c|fc) - fc = (f{1,2}|)
	assert pattern(
		conc(
			mult(charclass("f"), multiplier(bound(2), bound(3))),
			mult(charclass("c"), one),
		),
		conc(
			mult(charclass("f"), one),
			mult(charclass("c"), one),
		),
	) - conc(
		mult(charclass("f"), one),
		mult(charclass("c"), one),
	) == pattern(
		emptystring,
		conc(
			mult(charclass("f"), multiplier(bound(1), bound(2))),
		),
	)

	# (aa) -> aa
	assert pattern(
		conc(
			mult(charclass("a"), one),
			mult(charclass("a"), one),
		),
	)._commonconc(suffix=True) == conc(
		mult(charclass("a"), one),
		mult(charclass("a"), one),
	)
	
	# (aa) - aa = ()
	assert pattern(
		conc(
			mult(charclass("a"), one),
			mult(charclass("a"), one),
		),
	) - conc(
		mult(charclass("a"), one),
		mult(charclass("a"), one),
	) == pattern(emptystring)

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
	assert charclass("a") + charclass("a") == mult(charclass("a"), multiplier(bound(2), bound(2)))

	# charclass + mult
	# a + a = a{2}
	assert charclass("a") + mult(charclass("a"), one) == mult(charclass("a"), multiplier(bound(2), bound(2)))
	# a + a{2,} = a{3,}
	assert charclass("a") + mult(charclass("a"), multiplier(bound(2), inf)) == mult(charclass("a"), multiplier(bound(3), inf))
	# a + a{,8} = a{1,9}
	assert charclass("a") + mult(charclass("a"), multiplier(bound(0), bound(8))) == mult(charclass("a"), multiplier(bound(1), bound(9)))
	# a + b{,8} = ab{,8}
	assert charclass("a") + mult(charclass("b"), multiplier(bound(0), bound(8))) == conc(
		mult(charclass("a"), one),
		mult(charclass("b"), multiplier(bound(0), bound(8))),
	)

	# mult + charclass
	# b + b = b{2}
	assert mult(charclass("b"), one) + charclass("b") == mult(charclass("b"), multiplier(bound(2), bound(2)))
	# b* + b = b+
	assert mult(charclass("b"), star) + charclass("b") == mult(charclass("b"), plus)
	 # b{,8} + b = b{1,9}
	assert mult(charclass("b"), multiplier(bound(0), bound(8))) + charclass("b") == mult(charclass("b"), multiplier(bound(1), bound(9)))
	# b{,8} + c = b{,8}c
	assert mult(charclass("b"), multiplier(bound(0), bound(8))) + charclass("c") == conc(
		mult(charclass("b"), multiplier(bound(0), bound(8))),
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
		mult(charclass("a"), multiplier(bound(2), bound(2))),
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
		mult(charclass("b"), multiplier(bound(2), bound(2))),
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
			mult(charclass("c"), multiplier(bound(2), bound(2))),
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
		mult(charclass("c"), multiplier(bound(2), bound(2))),
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
			mult(charclass("a"), multiplier(bound(2), bound(2))),
			mult(charclass("b"), one),
		),
		conc(
			mult(charclass("a"), plus),
			mult(charclass("c"), one),
		),
	) == conc(
		mult(charclass("a"), multiplier(bound(2), bound(2))),
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
	assert mult(charclass("a"), multiplier(bound(3), bound(4))) + mult(charclass("b"), qm) == conc(
		mult(charclass("a"), multiplier(bound(3), bound(4))),
		mult(charclass("b"), qm),
	)
	# a* + a{2} = a{2,}
	assert mult(charclass("a"), star) + mult(charclass("a"), multiplier(bound(2), bound(2))) == mult(charclass("a"), multiplier(bound(2), inf))

	# mult + conc
	# a{2} + bc = a{2}bc
	assert mult(charclass("a"), multiplier(bound(2), bound(2))) + conc(
		mult(charclass("b"), one),
		mult(charclass("c"), one),
	) == conc(
		mult(charclass("a"), multiplier(bound(2), bound(2))),
		mult(charclass("b"), one),
		mult(charclass("c"), one),
	)
	# a? + ab = a{1,2}b
	assert mult(charclass("a"), qm) + conc(
		mult(charclass("a"), one),
		mult(charclass("b"), one),
	) == conc(
		mult(charclass("a"), multiplier(bound(1), bound(2))),
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
	assert mult(charclass("a"), multiplier(bound(2), bound(3))) + pattern(
		conc(
			mult(charclass("b"), one),
		),
		conc(
			mult(charclass("c"), one),
			mult(charclass("d"), one),
		),
	) == conc(
		mult(charclass("a"), multiplier(bound(2), bound(3))),
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
	assert mult(charclass("a"), multiplier(bound(2), bound(3))) + pattern(
		conc(
			mult(charclass("a"), multiplier(bound(2), bound(2))),
			mult(charclass("b"), one),
		),
		conc(
			mult(charclass("a"), plus),
			mult(charclass("c"), one),
		),
	) == conc(
		mult(charclass("a"), multiplier(bound(3), bound(4))),
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
	) + mult(charclass("a"), multiplier(bound(2), bound(3))) == conc(
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
		mult(charclass("a"), multiplier(bound(2), bound(3))),
	)
	# (ba{2}|ca+) + a{2,3} = (ba|ca*)a{3,4}
	assert pattern(
		conc(
			mult(charclass("b"), one),
			mult(charclass("a"), multiplier(bound(2), bound(2))),
		),
		conc(
			mult(charclass("c"), one),
			mult(charclass("a"), plus),
		),
	) + mult(charclass("a"), multiplier(bound(2), bound(3))) == conc(
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
		mult(charclass("a"), multiplier(bound(3), bound(4))),
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
		mult(charclass("b"), multiplier(bound(2), bound(2))),
		mult(charclass("c"), one),
	)

	# conc + pattern
	# za{2,3} + (b|cd) = za{2,3}(b|cd)
	assert conc(
		mult(charclass("z"), one),
		mult(charclass("a"), multiplier(bound(2), bound(3))),
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
		mult(charclass("a"), multiplier(bound(2), bound(3))),
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
		mult(charclass("a"), multiplier(bound(2), bound(3))),
	) + pattern(
		conc(
			mult(charclass("a"), multiplier(bound(2), bound(2))),
			mult(charclass("b"), one),
		),
		conc(
			mult(charclass("a"), plus),
			mult(charclass("c"), one),
		),
	) == conc(
		mult(charclass("z"), one),
		mult(charclass("a"), multiplier(bound(3), bound(4))),
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
		mult(charclass("a"), multiplier(bound(2), bound(3))),
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
		mult(charclass("a"), multiplier(bound(2), bound(3))),
	)
	# (ba{2}|ca+) + a{2,3}z = (ba|ca*)a{3,4}z
	assert pattern(
		conc(
			mult(charclass("b"), one),
			mult(charclass("a"), multiplier(bound(2), bound(2))),
		),
		conc(
			mult(charclass("c"), one),
			mult(charclass("a"), plus),
		),
	) + conc(
		mult(charclass("a"), multiplier(bound(2), bound(3))),
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
		mult(charclass("a"), multiplier(bound(3), bound(4))),
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
		), multiplier(bound(2), bound(2))
	)

	assert nothing.empty()
	assert charclass().empty()
	assert not dot.empty()
	assert not mult(charclass("a"), zero).empty()
	assert mult(charclass(), one).empty()
	assert not mult(charclass(), qm).empty()
	assert conc(mult(charclass("a"), one), mult(charclass(), one)).empty()
	assert not conc(mult(charclass("a"), one), mult(charclass(), qm)).empty()
	assert pattern().empty()
	assert not pattern(conc(mult(charclass("a"), zero))).empty()
	assert not pattern(conc(mult(charclass(), qm))).empty()

	assert str(parse("a.b")) == "a.b" # not "a[ab]b"
	assert str(parse("\\d{4}")) == "\\d{4}"

	# Intersection tests
	assert str(parse("a*") & parse("b*")) == ""
	assert str(parse("a") & parse("b")) == "[]"
	assert str(parse("\\d") & parse(".")) == "\\d"
	assert str(parse("\\d{2}") & parse("0.")) == "0\\d"
	assert str(parse("\\d{2}") & parse("19.*")) == "19"
	assert str(parse("\\d{3}") & parse("19.*")) == "19\\d"
	assert str(parse("abc...") & parse("...def")) == "abcdef"
	assert str(parse("[bc]*[ab]*") & parse("[ab]*[bc]*")) in {"([ab]*a|[bc]*c)?b*", "b*(a[ab]*|c[bc]*)?"}
	assert str(parse("\\W*") & parse("[a-g0-8$%\\^]+") & parse("[^d]{2,8}")) == "[$%\\^]{2,8}"
	assert str(parse("\\d{4}-\\d{2}-\\d{2}") & parse("19.*")) == "19\\d\\d-\\d\\d-\\d\\d"

	# Reduction tests
	assert str(parse("(|(|(|(|(|(|[$%\^])[$%\^])[$%\^])[$%\^])[$%\^])[$%\^])[$%\^][$%\^]")) == "[$%\^]{2,8}"
	assert str(parse("[0-9a-fA-F][0-9a-fA-F][0-9a-fA-F]")) == "[0-9A-Fa-f]{3}"

	# This one is horrendous and we have to jump through some hoops to get to
	# a sensible result. Probably not a good unit test actually.
	long = \
	"(aa|bb*aa)a*|((ab|bb*ab)|(aa|bb*aa)a*b)((ab|bb*ab)|(aa|bb*aa)a*b)*" + \
	"(aa|bb*aa)a*|((ab|bb*ab)|(aa|bb*aa)a*b)((ab|bb*ab)|(aa|bb*aa)a*b)*"
	long = parse(long)
	long = reversed(long.fsm())
	long = reversed(long.lego())
	assert str(long) == "[ab]*a[ab]"
	short = "[ab]*a?b*|[ab]*b?a*"
	assert str(parse(".*") & parse(short).reduce()) == "[ab]*"

	# DEFECT: "0{2}|1{2}" was erroneously reduced() to "[01]{2}"
	bad = parse("0{2}|1{2}").fsm({"0", "1", otherchars})
	assert bad.accepts("00")
	assert bad.accepts("11")
	assert not bad.accepts("01")
	assert str(parse("0|[1-9]|ab")) == "\d|ab"

	# lego.alphabet() should include "otherchars"
	assert parse("").alphabet() == {otherchars}

	# You should be able to fsm() a single lego piece without supplying a specific
	# alphabet. That should be determinable from context.
	assert str(parse("a.b").fsm().lego()) == "a.b" # not "a[ab]b"

	# A suspiciously familiar example
	bad = parse("0{2}|1{2}").fsm()
	assert bad.accepts("00")
	assert bad.accepts("11")
	assert not bad.accepts("01")
	assert str(parse("0|[1-9]|ab")) == "\d|ab"

	# everythingbut().
	# Regexes are usually gibberish but we make a few claims
	a = parse("a")
	notA = a.everythingbut().fsm()
	assert notA.accepts("")
	assert not notA.accepts("a")
	assert notA.accepts("aa")

	# everythingbut(), called twice, should take us back to where we started.
	beer = parse("beer")
	notBeer = beer.everythingbut()
	beer2 = notBeer.everythingbut()
	assert str(beer2) == "beer"

	# ".*" becomes "[]" and vice versa under this call.
	everything = parse(".*")
	assert str(everything.everythingbut()) == str(nothing)
	assert str(nothing.everythingbut()) == str(everything)

	# epsilon reduction in patterns.
	assert parse("|(ab)*|def").reduce() == parse("(ab)*|def")
	assert parse("|(ab)+|def").reduce() == parse("(ab)*|def")
	assert parse("|.+").reduce() == parse(".*")
	assert parse("|a+|b+") in {parse("a+|b*"), parse("a*|b+")}

	# Regex reversal
	assert reversed(parse("b")) == parse("b")
	assert reversed(parse("e*")) == parse("e*")
	assert reversed(parse("bear")) == parse("raeb")
	assert reversed(parse("beer")) == parse("reeb")
	assert reversed(parse("abc|def|ghi")) == parse("cba|fed|ihg")
	assert reversed(parse("(abc)*d")) == parse("d(cba)*")

	# Defect: (a{2})* should NOT reduce to a*
	a2 = mult(charclass("a"), multiplier(bound(2), bound(2)))
	a2star = a2 * star
	assert a2star == mult(pattern(conc(a2)), star)

	# Allow "\w", "\d" and "\s" in charclasses
	assert parse("[\w~]*").fsm().accepts("a0~")
	assert parse("[\da]*").fsm().accepts("0129a")
	assert parse("[\s]+").fsm().accepts(" \t \t ")

	# String generators: charclass
	gen = charclass("xyz").strings()
	assert next(gen) == "x"
	assert next(gen) == "y"
	assert next(gen) == "z"
	try:
		next(gen)
		assert False
	except StopIteration:
		assert True

	# Generators: mult
	# One term
	gen = mult(charclass("ab"), one).strings()
	assert next(gen) == "a"
	assert next(gen) == "b"
	try:
		next(gen)
		assert False
	except StopIteration:
		assert True

	# No terms
	gen = mult(charclass("ab"), zero).strings()
	assert next(gen) == ""
	try:
		next(gen)
		assert False
	except StopIteration:
		assert True

	# Many terms
	# [ab]*
	gen = mult(charclass("ab"), star).strings()
	assert next(gen) == ""
	assert next(gen) == "a"
	assert next(gen) == "b"
	assert next(gen) == "aa"
	assert next(gen) == "ab"
	assert next(gen) == "ba"
	assert next(gen) == "bb"
	assert next(gen) == "aaa"

	# Generators: conc
	# [ab][cd]
	gen = conc(mult(charclass("ab"), one), mult(charclass("cd"), one)).strings()
	assert next(gen) == "ac"
	assert next(gen) == "ad"
	assert next(gen) == "bc"
	assert next(gen) == "bd"
	try:
		next(gen)
		assert False
	except StopIteration:
		assert True

	# Generators: pattern
	# [ab]|[cd]
	gen = pattern(
		conc(mult(charclass("ab"), one)),
		conc(mult(charclass("cde"), one)),
	).strings()
	assert next(gen) == "a"
	assert next(gen) == "b"
	assert next(gen) == "c"
	assert next(gen) == "d"
	assert next(gen) == "e"
	try:
		next(gen)
		assert False
	except StopIteration:
		assert True

	# more complex
	gen = parse("abc|def(ghi|jkl)").strings()
	assert next(gen) == "abc"
	assert next(gen) == "defghi"
	assert next(gen) == "defjkl"

	gen = parse("[0-9a-fA-F]{3,10}").strings()
	assert next(gen) == "000"
	assert next(gen) == "001"
	assert next(gen) == "002"

	# Infinite generator, flummoxes both depth-first and breadth-first searches
	gen = parse("a*b*").strings()
	assert next(gen) == ""
	assert next(gen) == "a"
	assert next(gen) == "b"
	assert next(gen) == "aa"
	assert next(gen) == "ab"
	assert next(gen) == "bb"
	assert next(gen) == "aaa"
	assert next(gen) == "aab"
	assert next(gen) == "abb"
	assert next(gen) == "bbb"
	assert next(gen) == "aaaa"

	# Generator needs to handle wildcards as well
	gen = parse("a.b").strings(otherchar="*")
	assert next(gen) == "a*b"
	assert next(gen) == "aab"
	assert next(gen) == "abb"
	try:
		next(gen)
		assert False
	except StopIteration:
		assert True

	# Complexify!
	gen = (parse("[bc]*[ab]*") & parse("[ab]*[bc]*")).strings()
	assert next(gen) == ""
	assert next(gen) == "a"
	assert next(gen) == "b"
	assert next(gen) == "c"
	assert next(gen) == "aa"
	assert next(gen) == "ab"
	# no "ac"
	assert next(gen) == "ba"
	assert next(gen) == "bb"
	assert next(gen) == "bc"
	# no "ca"
	assert next(gen) == "cb"
	assert next(gen) == "cc"
	assert next(gen) == "aaa"

	# Problem relating to isinstance(). The class "mult" was occurring as both
	# lego.mult and as __main__.mult and apparently these count as different
	# classes for some reason, so isinstance(m, mult) was returning false.
	starfree = (parse("").everythingbut() + parse("aa") + parse("").everythingbut()).everythingbut()

	assert repr(~charclass("a")) == "~charclass('a')"

	print("OK")

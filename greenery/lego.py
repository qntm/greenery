# -*- coding: utf-8 -*-

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

	If the FSM module is available, call lego.to_fsm() on any lego piece to return
	a finite state machine capable of accepting strings described by that piece.

	Most important are the reduce() methods present in charclass, mult, conc and
	pattern. While there is no such thing as a canonical form for a given regex
	pattern, these procedures can drastically simplify a regex structure for
	readability. They're also pretty extensible.
'''
from greenery import fsm

class nomatch(Exception):
	'''Thrown when parsing fails. Almost always caught and almost never fatal'''
	pass

def reduce_after(method):
	'''reduce() the result of this method call (unless you already reduced it).'''
	def new_method(self, *args, **kwargs):
		result = method(self, *args, **kwargs)
		if result == self:
			return result
		return result.reduce()
	return new_method

def parse(string):
	'''
		Parse a full string and return a lego piece. Fail if the whole string
		wasn't parsed
	'''
	return pattern.parse(string)

def from_fsm(f):
	'''
		Turn the supplied finite state machine into a `lego` object. This is
		accomplished using the Brzozowski algebraic method.
	'''
	# Make sure the supplied alphabet is kosher. It must contain only single-
	# character strings or `fsm.anything_else`.
	for symbol in f.alphabet:
		if symbol == fsm.anything_else:
			continue
		if isinstance(symbol, str) and len(symbol) == 1:
			continue
		raise Exception("Symbol " + repr(symbol) + " cannot be used in a regular expression")

	# We need a new state not already used
	outside = object()

	# The set of strings that would be accepted by this FSM if you started
	# at state i is represented by the regex R_i.
	# If state i has a sole transition "a" to state j, then we know R_i = a R_j.
	# If state i is final, then the empty string is also accepted by this regex.
	# And so on...

	# From this we can build a set of simultaneous equations in len(f.states)
	# variables. This system is easily solved for all variables, but we only
	# need one: R_a, where a is the starting state.

	# The first thing we need to do is organise the states into order of depth,
	# so that when we perform our back-substitutions, we can start with the
	# last (deepest) state and therefore finish with R_a.
	states = [f.initial]
	i = 0
	while i < len(states):
		current = states[i]
		if current in f.map:
			for symbol in sorted(f.map[current], key=fsm.key):
				next = f.map[current][symbol]
				if next not in states:
					states.append(next)
		i += 1

	# Our system of equations is represented like so:
	brz = {}
	for a in f.states:
		brz[a] = {}
		for b in f.states | {outside}:
			brz[a][b] = nothing

	# Populate it with some initial data.
	for a in f.map:
		for symbol in f.map[a]:
			b = f.map[a][symbol]
			if symbol == fsm.anything_else:
				brz[a][b] |= ~charclass(f.alphabet - {fsm.anything_else})
			else:
				brz[a][b] |= charclass({symbol})
		if a in f.finals:
			brz[a][outside] |= emptystring

	# Now perform our back-substitution
	for i in reversed(range(len(states))):
		a = states[i]

		# Before the equation for R_a can be substituted into the other
		# equations, we need to resolve the self-transition (if any).
		# e.g.    R_a = 0 R_a |   1 R_b |   2 R_c
		# becomes R_a =         0*1 R_b | 0*2 R_c
		loop = brz[a][a] * star # i.e. "0*"
		del brz[a][a]

		for right in brz[a]:
			brz[a][right] = loop + brz[a][right]

		# Note: even if we're down to our final equation, the above step still
		# needs to be performed before anything is returned.

		# Now we can substitute this equation into all of the previous ones.
		for j in range(i):
			b = states[j]

			# e.g. substituting R_a =  0*1 R_b |      0*2 R_c
			# into              R_b =    3 R_a |        4 R_c | 5 R_d
			# yields            R_b = 30*1 R_b | (30*2|4) R_c | 5 R_d
			univ = brz[b][a] # i.e. "3"
			del brz[b][a]

			for right in brz[a]:
				brz[b][right] |= univ + brz[a][right]

	return brz[f.initial][outside].reduce()

def static(string, i, static):
	j = i+len(static)
	if string[i:j] == static:
		return j
	raise nomatch

def select_static(string, i, *statics):
	for st in statics:
		j = i+len(st)
		if string[i:j] == st:
			return st, j
	raise nomatch

def read_until(string, i, stop_char):
	start = i
	while True:
		if i >= len(string):
			raise nomatch
		if string[i] == stop_char:
			break
		i += 1
	return i + 1, string[start:i]

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

	def to_fsm(self, alphabet):
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

	@classmethod
	def match(cls, string, i = 0):
		'''
			Start at index i in the supplied string and try to match one of the
			present class. Elementary recursive descent parsing with very little
			need for flair. The opposite of __str__(), above. (In most cases.)
			Throws a nomatch in the event of failure.
		'''
		raise Exception("Not implemented")

	@classmethod
	def parse(cls, string):
		'''
			Parse the entire supplied string as an instance of the present class.
			Mainly for internal use in unit tests because it drops through to match()
			in a convenient way.
		'''
		obj, i = cls.match(string, 0)
		if i != len(string):
			raise Exception("Could not parse '" + string + "' beyond index " + str(i))
		return obj

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

	def __add__(self, other):
		'''
			Concatenate any two lego pieces, regardless of differing classes. Because
			reduce() (above) is always called afterwards, the result is as simplified
			as possible.
			Call using "a = b + c"
		'''
		raise Exception("Not implemented")

	def __mul__(self, multiplier):
		'''
			Equivalent to repeated concatenation. Multiplier consists of a minimum
			and a maximum; maximum may be infinite (for Kleene star closure).
			Call using "a = b * qm"
			Reduce() is always called afterwards.
		'''
		raise Exception("Not implemented")

	def __or__(self, other):
		'''
			Alternate between any two lego pieces, regardless of differing classes.
			Again, reduce() is called afterwards, usually with excellent results.
			Call using "a = b | c".
			This method MUST NOT call the to_fsm() method, because this method is used
			in turn when converting an FSM back to a regex.
		'''
		raise Exception("Not implemented")

	def __and__(self, other):
		'''
			Intersection function. Return a lego piece that can match any string
			that both self and other can match. Fairly elementary results relating
			to regular languages and finite state machines show that this is
			possible, but implementation is a BEAST in many cases. Here, we convert
			both lego pieces to FSMs (see to_fsm(), above) for the intersection, then
			back to lego afterwards.
			Call using "a = b & c"
		'''
		raise Exception("Not implemented")

	def equivalent(self, other):
		'''
			Two lego objects are equivalent if they recognise the same strings. Note
			that in the general case this is actually quite an intensive calculation,
			but far from unsolvable, as we demonstrate here:
		'''
		return self.to_fsm().equivalent(other.to_fsm())

	def alphabet(self):
		'''
			Return a set of all unique characters used in this lego piece.
			In theory this could be a static property, self.alphabet, not
			a function, self.alphabet(), but in the vast majority of cases
			this will never be queried so it's a waste of computation to
			calculate it every time a lego piece is instantiated.
			By convention, fsm.anything_else is always included in this result.
		'''
		raise Exception("Not implemented")

	def everythingbut(self):
		'''
			Return a lego object which will match any string not matched by self,
			and which will not match any string matched by self.
			Another task which is very difficult in general (and typically returns
			utter garbage when actually printed), but becomes trivial to code
			thanks to FSM routines.
		'''
		return from_fsm(self.to_fsm().everythingbut())

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

	def matches(self, string):
		return self.to_fsm().accepts(string)

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
		for string in self.to_fsm().strings():

			# Have to represent `fsm.anything_else` somehow.
			if fsm.anything_else in string:
				if otherchar == None:
					raise Exception("Please choose an 'otherchar'")
				string = [
					otherchar if char == fsm.anything_else else char
					for char in string
				]

			yield "".join(string)


class anchor(lego):
	def __init__(self, v):
		self.__dict__["v"] = v

	def __eq__(self, other):
		try:
			return self.v == other.v
		except AttributeError:
			return False

	def __ne__(self, other):
		return not self.__eq__(other)

	def __hash__(self):
		return hash(self.v)

	def __str__(self):
		return anchors[self]

	@reduce_after
	def reduce(self):
		return self

	def __repr__(self):
		return "anchor(%s)" % anchors[self]

	def empty(self):
		return False

	@classmethod
	def match(cls, string, i = 0):
		for _anchor, value in anchors.iteritems():
			try:
				return _anchor, static(string, i, value)
			except nomatch:
				pass

		raise nomatch

	def __reversed__(self):
		return self


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
		chars = frozenset(chars)
		# chars should consist only of chars
		if fsm.anything_else in chars:
			raise Exception("Can't put " + repr(fsm.anything_else) + " in a charclass")
		self.__dict__["chars"]   = chars
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

	# These are the characters which may be first inside char class definition and may not be escaped
	classFirstCharSpecialCases = set('-')

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

			# If char is an ASCII control character, don't print it directly,
			# return a hex escape sequence e.g. "\\x00". Note that this includes
			# tab and other characters already handled above
			if 0 <= ord(char) <= 0x1F or ord(char) == 0x7f:
				return "\\x" + "{0:02x}".format(ord(char))

			return char

		# multiple characters (or possibly 0 characters)
		return "[" + self.escape() + "]"

	def escape(self):

		def escapeChar(char):
			if char in charclass.classSpecial:
				return "\\" + char
			if char in escapes.keys():
				return escapes[char]

			# If char is an ASCII control character, don't print it directly,
			# return a hex escape sequence e.g. "\\x00". Note that this includes
			# tab and other characters already handled above
			if 0 <= ord(char) <= 0x1F or ord(char) == 0x7f:
				return "\\x" + "{0:02x}".format(ord(char))

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
		for char in sorted(self.chars, key=ord):

			# range is not empty: new char must fit after previous one
			if len(currentRange) > 0:

				i = ord(char)

				# char doesn't fit old range: restart
				if i != ord(currentRange[-1]) + 1:
					output += recordRange()
					currentRange = ""

			currentRange += char

		output += recordRange()

		return output

	def to_fsm(self, alphabet=None):
		if alphabet is None:
			alphabet = self.alphabet()

		# 0 is initial, 1 is final

		# If negated, make a singular FSM accepting any other characters
		if self.negated:
			map = {
				0: dict([(symbol, 1) for symbol in alphabet - self.chars]),
			}

		# If normal, make a singular FSM accepting only these characters
		else:
			map = {
				0: dict([(symbol, 1) for symbol in self.chars]),
			}

		return fsm.fsm(
			alphabet = alphabet,
			states   = {0, 1},
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

	def __add__(self, other):
		return mult(self, one) + other

	def alphabet(self):
		return {fsm.anything_else} | self.chars

	def empty(self):
		return len(self.chars) == 0 and self.negated == False

	@classmethod
	def match(cls, string, i = 0):
		if i >= len(string):
			raise nomatch

		# Turn e.g. "\\x40" into "@". Exactly two hex digits
		def unescapeHex(string, i):
			hex_digits = "0123456789AaBbCcDdEeFf"

			j = static(string, i, "\\x")

			hex1 = string[j] # e.g. "4"
			if not hex1 in hex_digits:
				raise nomatch
			j += len(hex1)

			hex2 = string[j] # e.g. "0"
			if not hex2 in hex_digits:
				raise nomatch
			j += len(hex2)

			codepoint = int(hex1 + hex2, 16) # e.g. 64
			char = chr(codepoint) # "@"
			return char, j

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

			# hex escape e.g. "\\x40" returns "@"
			try:
				return unescapeHex(string, i)
			except nomatch:
				pass

			# single non-special character, not contained
			# inside square brackets
			char, j = string[i], i+1
			if char in charclass.classSpecial:
				raise nomatch

			return char, j

		def matchClassInteriorFirst(string, i):
			try:
				return select_static(string, i, *charclass.classFirstCharSpecialCases)
			except nomatch:
				pass
			return matchClassInterior1(string, i)

		def matchClassInterior1(string, i):

			# Attempt 1: shorthand e.g. "\w"
			for key in charclass.shorthand:
				try:
					return key, static(string, i, charclass.shorthand[key])
				except nomatch:
					pass

			# Attempt 2: a range e.g. "d-h"
			try:
				first, j = matchInternalChar(string, i) # `first` is "d"
				k = static(string, j, "-")
				last, k = matchInternalChar(string, k) # `last` is "h"

				firstIndex = ord(first) # 100
				lastIndex = ord(last) # 104

				# Be strict here, "d-d" is not allowed
				if firstIndex >= lastIndex:
					raise nomatch("Range '" + first + "' to '" + last + "' not allowed")

				chars = "".join([
					chr(i) for i in range(firstIndex, lastIndex + 1)
				])
				return chars, k
			except nomatch:
				pass

			# Attempt 3: just a character on its own
			return matchInternalChar(string, i)

		def matchClassInterior(string, i):
			internals = ""
			try:
				internal, i = matchClassInteriorFirst(string, i)
				internals += internal
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

		# e.g. if seeing "\\x40", return "@"
		try:
			char, j = unescapeHex(string, i)
			return charclass(char), j
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
		if not v is None and v < 0:
			raise Exception("Invalid bound: " + repr(v))
		self.__dict__['v'] = v

	def __repr__(self):
		return "bound(" + repr(self.v) + ")"

	def __str__(self):
		if self == inf:
			# This only happens for an unlimited upper bound
			return ""
		return str(self.v)

	@classmethod
	def match(cls, string, i = 0):
		def matchAnyOf(string, i, collection):
			for char in collection:
				try:
					return char, static(string, i, char)
				except nomatch:
					pass
			raise nomatch

		# "0"
		try:
			return bound(0), static(string, i, "0")
		except nomatch:
			pass

		# "1", etc.
		try:
			digit, j = matchAnyOf(string, i, "123456789")
			integer = int(digit)
			try:
				while True:
					digit, j = matchAnyOf(string, j, "0123456789")
					integer *= 10
					integer += int(digit)
			except nomatch:
				return bound(integer), j
		except nomatch:
			pass

		# "" empty string = infinite bound as in "{4,}"
		return inf, i

	def __eq__(self, other):
		try:
			return self.v == other.v
		except AttributeError:
			return False

	def __ne__(self, other):
		return not self == other

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
			if self != inf:
				raise Exception("Can't subtract " + repr(other) + " from " + repr(self))

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

	def __init__(self, min, max, greedy=True):
		if min == inf:
			raise Exception("Minimum bound of a multiplier can't be " + repr(inf))
		if min > max:
			raise Exception("Invalid multiplier bounds: " + repr(min) + " and " + repr(max))

		# More useful than "min" and "max" in many situations
		# are "mandatory" and "optional".
		mandatory = min
		optional = max - min

		self.__dict__['min'] = min
		self.__dict__['max'] = max
		self.__dict__['greedy'] = greedy
		self.__dict__['mandatory'] = mandatory
		self.__dict__['optional'] = optional

	def __eq__(self, other):
		try:
			return self.min == other.min and self.max == other.max and self.greedy == other.greedy
		except AttributeError:
			return False

	def __ne__(self, other):
		return not self.__eq__(other)

	def __hash__(self):
		return hash((self.min, self.max, self.greedy))

	def __repr__(self):
		return "multiplier(" + repr(self.min) + ", " + repr(self.max) + ")"

	def __str__(self):
		if self.max == bound(0):
			raise Exception("Can't serialise a multiplier with bound " + repr(self.max))
		if self in symbolic.keys():
			return symbolic[self]
		if self.min == self.max:
			return "{" + str(self.min) + "}"
		return "{" + str(self.min) + "," + str(self.max) + "}"

	@classmethod
	def match(cls, string, i = 0):

		# {2,3} or {2,}
		try:
			j = static(string, i, "{")
			min, j = bound.match(string, j)
			j = static(string, j, ",")
			max, j = bound.match(string, j)
			j = static(string, j, "}")
			return multiplier(min, max), j
		except nomatch:
			pass

		# {2}
		try:
			j = static(string, i, "{")
			min, j = bound.match(string, j)
			j = static(string, j, "}")
			return multiplier(min, min), j
		except nomatch:
			pass

		# "*?"/"+?"/"?"/"*"/"+"/""
		# we do these in reverse order of symbol length, because
		# that forces "" to be done last
		for key in sorted(symbolic, key=lambda key: -len(symbolic[key])):
			try:
				return key, static(string, i, symbolic[key])
			except nomatch:
				pass

		raise nomatch

	@classmethod
	def parse(cls, string):
		'''
			Parse the entire supplied string as an instance of the present class.
			Mainly for internal use in unit tests because it drops through to match()
			in a convenient way.
		'''
		obj, i = cls.match(string, 0)
		if i != len(string):
			raise Exception("Could not parse '" + string + "' beyond index " + str(i))
		return obj

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
		if not self.canmultiplyby(other):
			raise Exception("Can't multiply " + repr(self) + " by " + repr(other))
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
			defined for all multipliers since they may not overlap.
		'''
		if not self.canintersect(other):
			raise Exception("Can't compute intersection of " + repr(self) + " and " + repr(other))
		a = max(self.min, other.min)
		b = min(self.max, other.max)
		return multiplier(a, b)

	def canunion(self, other):
		'''Union is not defined for all pairs of multipliers. e.g. {0,1} | {3,4}'''
		return not (self.max + bound(1) < other.min or other.max + bound(1) < self.min)

	def __or__(self, other):
		'''
			Find the union of two multipliers: that is, a third multiplier expressing
			the range covered by either of the originals. This is not defined for
			all multipliers since they may not intersect.
		'''
		if not self.canunion(other):
			raise Exception("Can't compute the union of " + repr(self) + " and " + repr(other))
		a = min(self.min, other.min)
		b = max(self.max, other.max)
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

	def __mul__(self, multiplier):
		if multiplier == one:
			return self
		if self.multiplier.canmultiplyby(multiplier):
			return mult(self.multiplicand, self.multiplier * multiplier)
		return mult(pattern(conc(self)), multiplier)

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
			raise Exception("Can't subtract " + repr(other) + " from " + repr(self))
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
		return {fsm.anything_else} | self.multiplicand.alphabet()

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

	def to_fsm(self, alphabet=None):
		if alphabet is None:
			alphabet = self.alphabet()

		# worked example: (min, max) = (5, 7) or (5, inf)
		# (mandatory, optional) = (5, 2) or (5, inf)

		unit = self.multiplicand.to_fsm(alphabet)
		# accepts e.g. "ab"

		# accepts "ababababab"
		mandatory = unit * self.multiplier.mandatory.v

		# unlimited additional copies
		if self.multiplier.optional == inf:
			optional = unit.star()
			# accepts "(ab)*"

		else:
			optional = fsm.epsilon(alphabet) | unit
			# accepts "(ab)?"

			optional *= self.multiplier.optional.v
			# accepts "(ab)?(ab)?"

		return mandatory + optional

	@classmethod
	def match(cls, string, i = 0):

		def matchMultiplicand(string, i):
			# explicitly non-capturing "(?:...)" syntax. No special significance
			try:
				j = static(string, i, "(?")
				st, j = select_static(string, j, ':', 'P<')
				if st == 'P<':
					j, group_name = read_until(string, j, '>')
				multiplicand, j = pattern.match(string, j)
				j = static(string, j, ")")
				return multiplicand, j
			except nomatch:
				pass

			# normal "(...)" syntax
			try:
				j = static(string, i, "(")
				multiplicand, j = pattern.match(string, j)
				j = static(string, j, ")")
				return multiplicand, j
			except nomatch:
				pass

			# Just a charclass on its own
			return charclass.match(string, i)

		multiplicand, j = matchMultiplicand(string, i)
		multiplier_, j = multiplier.match(string, j)
		return mult(multiplicand, multiplier_), j

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

	def __mul__(self, multiplier):
		if multiplier == one:
			return self
		# Have to replace self with a pattern unfortunately
		return pattern(self) * multiplier

	def __add__(self, other):
		# other must be a conc too
		if hasattr(other, "chars") or hasattr(other, "concs"):
			other = mult(other, one)
		if hasattr(other, "multiplicand"):
			other = conc(other)

		return conc(*(self.mults + other.mults))

	def __or__(self, other):
		return pattern(self) | other

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

		# Conc contains "()" (i.e. a mult containing only a pattern containing the
		# empty string)? That can be removed e.g. "a()b" -> "ab"
		for i in range(len(self.mults)):
			if self.mults[i].multiplicand == pattern(emptystring):
				new = self.mults[:i] + self.mults[i+1:]
				return conc(*new)

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

	def to_fsm(self, alphabet=None):
		if alphabet is None:
			alphabet = self.alphabet()

		# start with a component accepting only the empty string
		fsm1 = fsm.epsilon(alphabet)
		for m in self.mults:
			fsm1 += m.to_fsm(alphabet)
		return fsm1

	def alphabet(self):
		return {fsm.anything_else}.union(*[m.alphabet() for m in self.mults])

	def empty(self):
		for m in self.mults:
			if m.empty():
				return True
		return False

	def __str__(self):
		return "".join(str(m) for m in self.mults)

	@classmethod
	def match(cls, string, i = 0):
		mults = list()
		try:
			while True:
				try:
					m, i = anchor.match(string, i)
				except nomatch:
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
				if i != 0:
					raise Exception("Can't subtract " + repr(other) + " from " + repr(self))

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

	def __mul__(self, multiplier):
		if multiplier == one:
			return self
		return mult(self, multiplier)

	def __add__(self, other):
		return mult(self, one) + other

	def alphabet(self):
		return {fsm.anything_else}.union(*[c.alphabet() for c in self.concs])

	def empty(self):
		for c in self.concs:
			if not c.empty():
				return False
		return True

	def __and__(self, other):
		# A deceptively simple method for an astoundingly difficult operation
		alphabet = self.alphabet() | other.alphabet()

		# Which means that we can build finite state machines sharing that alphabet
		combined = self.to_fsm(alphabet) & other.to_fsm(alphabet)
		return from_fsm(combined)

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
		if len(self.concs) == 0:
			raise Exception("Can't serialise " + repr(self))

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

		# If this pattern contains several concs each containing just 1 mult and
		# their multiplicands agree, we may be able to merge the multipliers
		# e.g. "a{1,2}|a{3,4}|bc" -> "a{1,4}|bc"
		oldconcs = list(self.concs) # so we can index the things
		for i in range(len(oldconcs)):
			conc1 = oldconcs[i]
			if len(conc1.mults) != 1:
				continue
			multiplicand1 = conc1.mults[0].multiplicand
			for j in range(i + 1, len(oldconcs)):
				conc2 = oldconcs[j]
				if len(conc2.mults) != 1:
					continue
				multiplicand2 = conc2.mults[0].multiplicand
				if multiplicand2 != multiplicand1:
					continue
				multiplicand = multiplicand1
				multiplier1 = conc1.mults[0].multiplier
				multiplier2 = conc2.mults[0].multiplier
				if not multiplier1.canunion(multiplier2):
					continue
				multiplier = multiplier1 | multiplier2
				newconcs = \
					oldconcs[:i] + \
					oldconcs[i + 1:j] + \
					oldconcs[j + 1:] + \
					[conc(mult(multiplicand, multiplier))]
				return pattern(*newconcs)

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
	def match(cls, string, i = 0):
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
		if len(self.concs) == 0:
			raise Exception("Can't call _commonconc on " + repr(self))

		from functools import reduce
		return reduce(
			lambda x, y: x.common(y, suffix=suffix),
			self.concs
		)

	def to_fsm(self, alphabet=None):
		if alphabet is None:
			alphabet = self.alphabet()

		fsm1 = fsm.null(alphabet)
		for c in self.concs:
			fsm1 |= c.to_fsm(alphabet)
		return fsm1

	def __reversed__(self):
		return pattern(*(reversed(c) for c in self.concs))

# Special and useful values go here.

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
	"\\b": "\\b",
	"\\B": "\\B",
}

# Use this for cases where no upper bound is needed
inf = bound(None)

# Preset multipliers. These get used ALL THE TIME in unit tests
zero = multiplier(bound(0), bound(0)) # has some occasional uses
qm   = multiplier(bound(0), bound(1))
one  = multiplier(bound(1), bound(1))
star = multiplier(bound(0), inf)
plus = multiplier(bound(1), inf)
lazy_star = multiplier(bound(0), inf, greedy=False)
lazy_plus = multiplier(bound(1), inf, greedy=False)

# Symbol lookup table for preset multipliers.
symbolic = {
	qm   : "?",
	one  : "" ,
	star : "*",
	plus : "+",
	lazy_star: '*?',
	lazy_plus: '+?',
}

# A very special conc expressing the empty string, ""
emptystring = conc()

b = anchor("\\b")
B = anchor("\\B")
G = anchor("\\G")
z = anchor("\\z")
Z = anchor("\\Z")
A = anchor("\\A")
dollar = anchor("$")
caret = anchor("^")

anchors = {
	b: b.v,
	B: B.v,
	G: G.v,
	z: z.v,
	Z: Z.v,
	A: A.v,
	dollar: dollar.v,
	caret: caret.v,
}
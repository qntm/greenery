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

# http://qntm.org/greenery

import sys
from lego import charclass, multiplier, mult, conc, pattern
from fsm import null
from mainops import fsmbuild, regexbuild

# TODO: investigate whether this whole transliteration malarkey is strictly
# necessary at all.

def getCharclasses(*patterns):
	'''
		Return a complete list of charclasses appearing in the supplied patterns
	'''
	charclasses = set()
	for thisPattern in patterns:
		for thisConc in thisPattern.concs:
			for thisMult in thisConc.mults:
				if type(thisMult.multiplicand) is charclass:
					charclasses.add(thisMult.multiplicand)
				else:
					charclasses.update(getCharclasses(thisMult.multiplicand))
	return charclasses

def transliterate(oldPattern, lookups):
	'''Replace character classes inside a nested data structure with their
	equivalents from a lookup table.'''
	newConcs = set()
	for oldConc in oldPattern.concs:
		newMults = list()
		for oldMult in oldConc.mults:
			oldMultiplicand = oldMult.multiplicand
			oldMultiplier = oldMult.multiplier

			if type(oldMultiplicand) is charclass:
				newMult = mult(lookups[oldMultiplicand], oldMultiplier)

			else:
				newMult = mult(transliterate(oldMultiplicand, lookups), oldMultiplier)

			newMults.append(newMult)
		newConc = conc(*newMults)
		newConcs.add(newConc)
	newPattern = pattern(*newConcs)
	return newPattern

def regexintersect(*strings):

	# turn each string into a pattern
	patterns = [pattern.parse(string) for string in strings]

	# The set of Unicode characters contains 1,114,112 characters, but we are not
	# worried about accounting for all of them in our alphabet. A typical list of
	# regexes will include a very small subset of those characters, but we are not
	# even worried about those: if a regex only includes references to [a-zA-Z] and
	# [a-zA-Z0-9], then we only really need *two* symbols: "[a-zA-Z]" and "[0-9]".
	# [a-zA-Z0-9] is equivalent to "[a-zA-Z]|[0-9]".

	# In order to construct this alphabet of symbols of this form, first we need to
	# identify the entire alphabet of chars which appear in our regexes.

	charclasses = getCharclasses(*patterns)

	# next we partition the space of Unicode characters into an "alphabet" of
	# "symbols", each symbol representing one or more Unicode characters, in such
	# a way that all of the charclasses seen above can be constructed from these
	# symbols in a unique way.
	# A sample "alphabet" is {[ab], [c], [^abc]}
	alphabet = {charclass(negateMe=True)}
	for Z in charclasses:

		# add Z to the alphabet.

		next = set()
		for A in alphabet:
			# add A - Z, A + Z.
			for newClass in [A.intersection(Z), A.difference(Z)]:
				if newClass != charclass():
					next.add(newClass)

		# add Z - (everything else)
		for A in alphabet:
			Z = Z.difference(A)
		if Z != charclass():
			next.add(Z)

		alphabet = next

	# Next we explicitly deconstruct each charclass we have seen into the form of
	# a union of disjoint symbols (unicode character sets) taken from our alphabet.
	# e.g. if our alphabet is [ab], [c], [^abc], then [^ab] is constructed as
	# [c]|[^abc]
	lookups = {}
	for Z in charclasses:
		key = Z
		symbols = set()
		for symbol in alphabet:
			if symbol.issubset(Z):
				symbols.add(symbol)
				Z = Z.difference(symbol)

		# once deconstructed, the multiplicand should be empty.
		if Z != charclass():
			raise Exception("Couldn't understand multiplicand")

		# this has to be a charclass. Even though the "chars" are themselves
		# charclasses. It's a whole mess. Just don't try to print them
		lookups[key] = charclass(symbols)

	# Now that we have this lookup table, use it on each pattern in turn.
	# The result is a set of patterns which now all use different, probably
	# overlapping parts of the *same unified alphabet!* under the covers.
	patterns = [transliterate(thisPattern, lookups) for thisPattern in patterns]

	# Which means that we can build finite state machines sharing that alphabet
	fsms = [fsmbuild(thisPattern, alphabet) for thisPattern in patterns]

	# and combine them
	combinedFsm = fsms[0]
	for x in fsms[1:]:
		combinedFsm &= x

	# and get a regex back
	return regexbuild(combinedFsm)

# AND DO IT
strings = sys.argv[1:]

if len(strings) > 0:
	print(regexintersect(*strings))

# no strings supplied? run unit tests
else:
	assert regexintersect("\\d{2}", "0.") == "0\\d"
	assert regexintersect("abc...", "...def") == "abcdef"
	assert regexintersect("a*", "b*") == ""
	assert regexintersect("a", "b") == None
	assert regexintersect("\\d{4}") == "\\d{4}"
	assert regexintersect("\\d", ".") == "\\d"
	assert regexintersect("\\d{2}", "19.*") == "19"
	assert regexintersect("\\d{3}", "19.*") == "19\\d"
	assert regexintersect("\\d{4}-\\d{2}-\\d{2}", "19.*") == "19\\d\\d-\\d\\d-\\d\\d"
	assert regexintersect("\\W*", "[a-g0-8$%\\^]+", "[^d]{2,8}") == "[$%\\^]{2,8}"
	assert regexintersect("[ab]*a?b*|[ab]*b?a*") == "[ab]*"
	assert regexintersect("[bc]*[ab]*", "[ab]*[bc]*") == "([ab]*a|[bc]*c)?b*"
	assert regexintersect("(aa|bb*aa)a*|((ab|bb*ab)|(aa|bb*aa)a*b)((ab|bb*ab)|(aa|bb*aa)a*b)*(aa|bb*aa)a*|((ab|bb*ab)|(aa|bb*aa)a*b)((ab|bb*ab)|(aa|bb*aa)a*b)*") == "[ab]*a[ab]"
	assert regexintersect("[0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F]") == "[0-9A-Fa-f]{5}"
	print("OK")

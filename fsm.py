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

# http://qntm.org/fsm
# http://qntm.org/greenery

from __future__ import absolute_import

class fsm:
	'''
		A Finite State Machine or FSM has an alphabet and a set of states. At any
		given moment, the FSM is in one state. When passed a symbol from the
		alphabet, the FSM jumps to another state (or possibly the same state).
		A map (Python dictionary) indicates where to jump.
		One state is nominated as a starting state. Zero or more states are
		nominated as final states. If, after consuming a string of symbols,
		the FSM is in a final state, then it is said to "accept" the string.
		This class also has some pretty powerful methods which allow FSMs to
		be concatenated, alternated between, multiplied, looped (Kleene star
		closure), intersected, and simplified.
		The majority of these methods are available using operator overloads.
	'''
	def __setattr__(self, name, value):
		'''Immutability prevents some potential problems.'''
		raise Exception("This object is immutable.")

	def __init__(self, alphabet, states, initial, finals, map):
		'''Initialise the hard way due to immutability.'''
		self.__dict__["alphabet"] = alphabet
		self.__dict__["states"  ] = set(states)
		self.__dict__["initial" ] = initial
		self.__dict__["finals"  ] = set(finals)
		self.__dict__["map"     ] = map

		# Validation. Thanks to immutability, this only needs to be carried out once.
		assert self.initial in self.states
		assert self.finals.issubset(self.states)
		for state in self.states:
			assert state in self.map.keys()
			for symbol in self.alphabet:
				assert symbol in self.map[state]
				assert self.map[state][symbol] in self.states

	def accepts(self, input):
		'''This is actually only used for unit testing purposes'''
		state = self.initial
		for symbol in input:
			state = self.map[state][symbol]
		return state in self.finals

	def reduce(self):
		'''
			A result by Brzozowski (1963) shows that a minimal finite state machine
			equivalent to the original can be obtained by reversing the original
			twice.
		'''
		return reversed(reversed(self))

	def __repr__(self):
		string = "fsm("
		string += "alphabet = " + repr(self.alphabet)
		string += ", states = " + repr(self.states)
		string += ", initial = " + repr(self.initial)
		string += ", finals = " + repr(self.finals)
		string += ", map = " + repr(self.map)
		string += ")"
		return string
	
	def __str__(self):
		rows = []

		# top row
		row = ["", "name", "final?"]
		row.extend(str(symbol) for symbol in sorted(self.alphabet, key=str))
		rows.append(row)

		# other rows
		for state in self.states:
			row = []
			if(state == self.initial):
				row.append("*")
			else:
				row.append("")
			row.append(str(state))
			if state in self.finals:
				row.append("True")
			else:
				row.append("False")
			row.extend(str(self.map[state][symbol]) for symbol in sorted(self.alphabet, key=str))
			rows.append(row)
		
		# column widths
		colwidths = []
		for x in range(len(rows[0])):
			colwidths.append(max(len(str(rows[y][x])) for y in range(len(rows))) + 1)

		# apply padding
		for y in range(len(rows)):
			for x in range(len(rows[y])):
				rows[y][x] = rows[y][x].ljust(colwidths[x])

		# horizontal line
		rows.insert(1, ["-" * colwidth for colwidth in colwidths])

		return "".join("".join(row) + "\n" for row in rows)

	def __add__(self, other):
		'''
			Concatenate two finite state machines together.
			For example, if self accepts "0*" and other accepts "1+(0|1)",
			will return a finite state machine accepting "0*1+(0|1)".
			Accomplished by effectively following non-deterministically.
			Call using "fsm3 = fsm1 + fsm2"
		'''
		# alphabets must be equal
		assert other.alphabet == self.alphabet

		# We start at the start of self. If this starting state happens to be
		# final in self, we also start at the start of other.
		if self.initial in self.finals:
			initial = frozenset([
				(0, self.initial),
				(1, other.initial),
			])
		else:
			initial = frozenset([(0, self.initial)])

		def final(state):
			for (id, substate) in state:
				# self
				if id == 0:
					if substate in self.finals:
						if other.initial in other.finals:
							return True

				# other
				elif id == 1:
					if substate in other.finals:
						return True

				else:
					raise Exception("What")

			return False

		# dedicated function accepts a "superset" and returns the next "superset"
		# obtained by following this transition in the new FSM
		def follow(current, symbol):

			next = []

			for (id, state) in current:
				if id == 0:
					next.append((0, self.map[state][symbol]))
					# final of self? merge with other initial
					if self.map[state][symbol] in self.finals:
						next.append((1, other.initial))
				elif id == 1:
					next.append((1, other.map[state][symbol]))
				else:
					raise Exception("Whaat")

			return frozenset(next)

		return crawl(self.alphabet, initial, final, follow).reduce()

	def star(self):
		'''
			If the present FSM accepts X, returns an FSM accepting X* (i.e. 0 or
			more Xes). This is NOT as simple as naively connecting the final states
			back to the initial state: see (b*ab)* for example. Instead we must create
			an articial "omega state" which is our only accepting state and which
			dives into the FSM and from which all exits return.
		'''

		# We need a new state not already used; guess first beyond current len
		omega = len(self.states)
		while omega in self.states:
			omega += 1

		initial = frozenset([omega])

		def follow(current, symbol):

			next = []

			for state in current:

				# the special new starting "omega" state behaves exactly like the
				# original starting state did
				if state == omega:
					state = self.initial

				substate = self.map[state][symbol]
				next.append(substate)

				# loop back to beginning
				if substate in self.finals:
					next.append(omega)

			return frozenset(next)

		# final if state contains omega
		def final(state):
			return omega in state

		return crawl(self.alphabet, initial, final, follow).reduce()

	def __mul__(self, multiplier):
		'''
			Given an FSM and a multiplier, return the multiplied FSM.
		'''
		assert multiplier >= 0

		if multiplier == 0:
			return epsilon(self.alphabet)

		# worked example: multiplier = 5
		output = self
		# accepts e.g. "ab"

		for i in range(multiplier - 1):
			output += self
		# now accepts e.g. "ababababab"

		return output.reduce()

	def __or__(self, other):
		'''
			Alternation.
			Return a finite state machine which accepts any sequence of symbols
			that is accepted by either self or other.
			Call using "fsm3 = fsm1 | fsm2"
		'''

		# alphabets must be equal
		assert other.alphabet == self.alphabet

		initial = (self.initial, other.initial)

		# dedicated function accepts a "superset" and returns the next "superset"
		# obtained by following this transition in the new FSM
		def follow(current, symbol):
			return (self.map[current[0]][symbol], other.map[current[1]][symbol])

		# state is final if *any* of its internal states are final
		def final(state):
			return state[0] in self.finals or state[1] in other.finals

		return crawl(self.alphabet, initial, final, follow).reduce()

	def __and__(self, other):
		'''
			Intersection.
			Take FSMs and AND them together. That is, return an FSM which
			accepts any sequence of symbols that is accepted by all of the original
			FSMs.
			Call using "fsm3 = fsm1 & fsm2"
		'''

		# alphabets must be equal
		assert other.alphabet == self.alphabet

		initial = (self.initial, other.initial)

		# dedicated function accepts a "superset" and returns the next "superset"
		# obtained by following this transition in the new FSM
		def follow(current, symbol):
			return (self.map[current[0]][symbol], other.map[current[1]][symbol])

		# state is final if *all* of its substates are final
		def final(state):
			return state[0] in self.finals and state[1] in other.finals

		return crawl(self.alphabet, initial, final, follow).reduce()

	def everythingbut(self):
		'''
			Return a finite state machine which will accept any string NOT
			accepted by self, and will not accept any string accepted by self.
			This is achieved very easily by flipping the "is final" property
			of each state.
		'''
		return fsm(
			self.alphabet,
			self.states,
			self.initial,
			self.states - self.finals,
			self.map
		).reduce()

	def __reversed__(self):
		'''
			Return a new FSM such that for every string that self accepts (e.g.
			"beer", the new FSM accepts the reversed string ("reeb").
		'''

		# Start from a composite "state-set" consisting of all final states.
		# If there are no final states, this set is empty and we'll find that
		# no other states get generated.
		initial = frozenset(self.finals)

		# Find every possible way to reach the current state-set
		# using this symbol.
		def follow(current, symbol):
			return frozenset([
				prev
				for prev in self.map
				for state in current
				if self.map[prev][symbol] == state
			])

		# A state-set is final if the initial state is in it.
		def final(state):
			return self.initial in state

		# Man, crawl() is the best!
		return crawl(self.alphabet, initial, final, follow)
		# Do not reduce() the result, since reduce() calls reversed() in turn

	def strings(self):
		'''
			Generate strings (lists of symbols) that this FSM accepts. Since there may
			be infinitely many of these we use a generator instead of constructing a
			static list. Strings will be sorted in order of length and then lexically.
			This procedure uses arbitrary amounts of memory but is very fast. There
			may be more efficient ways to do this, that I haven't investigated yet.
		'''

		# Many FSMs have "dead states". Once you reach a dead state, you can no
		# longer reach a final state. Since many strings may end up here, it's
		# advantageous to constrain our search to live states only.

		def islive(state):
			'''A state is "live" if a final state can be reached from it.'''
			reachable = [state]
			i = 0
			while i < len(reachable):
				current = reachable[i]
				if current in self.finals:
					return True
				for symbol in self.alphabet:
					next = self.map[current][symbol]
					if next not in reachable:
						reachable.append(next)
				i += 1
			return False

		livestates = {state for state in self.states if islive(state)}

		# We store a list of tuples. Each tuple consists of an input string and the
		# state that this input string leads to. This means we don't have to run the
		# state machine from the very beginning every time we want to check a new
		# string.
		strings = []

		# Initial entry (or possibly not, in which case this is a short one)
		cstate = self.initial
		cstring = []
		if cstate in livestates:
			if cstate in self.finals:
				yield cstring
			strings.append((cstring, cstate))

		# Fixed point calculation
		i = 0
		while i < len(strings):
			(cstring, cstate) = strings[i]
			for symbol in sorted(self.alphabet, key=str):
				nstate = self.map[cstate][symbol]
				nstring = cstring + [symbol]
				if nstate in livestates:
					if nstate in self.finals:
						yield nstring
					strings.append((nstring, nstate))
			i += 1

	def lego(self):
		'''
			This is the big kahuna of this module. Turn the present FSM into a regular
			expression object, as imported from the lego module. This is accomplished
			using the Brzozowski algebraic method.
		'''
		from .lego import nothing, charclass, emptystring, star, otherchars

		# We need a new state not already used; guess first beyond current len
		outside = len(self.states)
		while outside in self.states:
			outside += 1

		# The set of strings that would be accepted by this FSM if you started
		# at state i is represented by the regex R_i.
		# If state i has a sole transition "a" to state j, then we know R_i = a R_j.
		# If state i is final, then the empty string is also accepted by this regex.
		# And so on...

		# From this we can build a set of simultaneous equations in len(self.states)
		# variables. This system is easily solved for all variables, but we only
		# need one: R_a, where a is the starting state.

		# The first thing we need to do is organise the states into order of depth,
		# so that when we perform our back-substitutions, we can start with the
		# last (deepest) state and therefore finish with R_a.
		states = [self.initial]
		i = 0
		while i < len(states):
			current = states[i]
			for symbol in sorted(self.alphabet, key=str):
				next = self.map[current][symbol]
				if next not in states:
					states.append(next)
			i += 1

		# Our system of equations is represented like so:
		brz = {}
		for a in self.states:
			brz[a] = {}
			for b in self.states | {outside}:
				brz[a][b] = nothing

		# Populate it with some initial data.
		for a in self.map:
			for symbol in self.map[a]:
				b = self.map[a][symbol]
				if symbol == otherchars:
					brz[a][b] |= ~charclass(self.alphabet - {otherchars})
				else:
					brz[a][b] |= charclass({symbol})
			if a in self.finals:
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

		return brz[self.initial][outside]

def null(alphabet):
	'''
		An FSM accepting nothing (not even the empty string). This is
		demonstrates that this is possible, and is also extremely useful
		in some situations
	'''
	return fsm(
		alphabet = alphabet,
		states   = {0},
		initial  = 0,
		finals   = set(),
		map      = {
			0: dict([(symbol, 0) for symbol in alphabet]),
		},
	)

def epsilon(alphabet):
	'''
		Return an FSM matching an empty string, "", only.
		This is very useful in many situations
	'''
	return fsm(
		alphabet = alphabet,
		states   = {0, 1},
		initial  = 0,
		finals   = {0},
		map      = {
			0: dict([(symbol, 1) for symbol in alphabet]),
			1: dict([(symbol, 1) for symbol in alphabet]),
		},
	)

def crawl(alphabet, initial, final, follow):
	'''
		Given the above conditions and instructions, crawl a new unknown FSM,
		mapping its states, final states and transitions. Return the new FSM.
		This is a pretty powerful procedure which could potentially go on
		forever if you supply an evil version of follow().
	'''

	states = [initial]
	finals = set()
	map = {}

	# iterate over a growing list
	i = 0
	while i < len(states):
		state = states[i]

		# add to finals
		if final(state):
			finals.add(i)

		# compute map for this state
		map[i] = {}
		for symbol in sorted(alphabet, key=str):
			next = follow(state, symbol)

			try:
				j = states.index(next)
			except ValueError:
				j = len(states)
				states.append(next)

			map[i][symbol] = j

		i += 1

	return fsm(alphabet, range(len(states)), 0, finals, map)

# Unit tests.
if __name__ == "__main__":

	# Allow relative imports when executing within package directory, for running tests
	import sys, os
	sys.path.insert( 0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
	import greenery
	__package__ = str("greenery")
	del sys, os

	# Buggggs.
	abstar = fsm(
		alphabet = {'a', None, 'b'},
		states	 = {0, 1},
		initial	 = 0,
		finals	 = {0},
		map	 = {
			0: {'a': 0, None: 1, 'b': 0},
			1: {'a': 1, None: 1, 'b': 1}
		}
	)
	assert str(abstar.lego()) == "[ab]*"

	adotb = fsm(
		alphabet = {'a', None, 'b'},
		states	 = {0, 1, 2, 3, 4},
		initial	 = 0,
		finals	 = {4},
		map	 = {
			0: {'a': 2, None: 1, 'b': 1},
			1: {'a': 1, None: 1, 'b': 1},
			2: {'a': 3, None: 3, 'b': 3},
			3: {'a': 1, None: 1, 'b': 4},
			4: {'a': 1, None: 1, 'b': 1}
		}
	)
	assert str(adotb.lego()) == "a.b"

	from .lego import otherchars

	# Odd bug with fsm.__add__(), exposed by "[bc]*c"
	int5A = fsm(
		alphabet = {"a", "b", "c", otherchars},
		states   = {0, 1},
		initial  = 1,
		finals   = {1},
		map      = {
			0: {otherchars: 0, "a": 0, "b": 0, "c": 0},
			1: {otherchars: 0, "a": 0, "b": 1, "c": 1},
		}
	)
	assert int5A.accepts("")

	int5B = fsm(
		alphabet = {"a", "b", "c", otherchars},
		states   = {0, 1, 2},
		initial  = 1,
		finals   = {0},
		map      = {
			0: {otherchars: 2, "a": 2, "b": 2, "c": 2},
			1: {otherchars: 2, "a": 2, "b": 2, "c": 0},
			2: {otherchars: 2, "a": 2, "b": 2, "c": 2},
		}
	)
	assert int5B.accepts("c")

	int5C = int5A + int5B
	assert int5C.accepts("c")
	# assert int5C.initial == 0

	# fsm.lego()

	# Catch a recursion error
	assert str(fsm(
		alphabet = {"0", "1"},
		states   = {0, 1, 2, 3},
		initial  = 3,
		finals   = {1},
		map      = {
			0: {"0": 1, "1": 1},
			1: {"0": 2, "1": 2},
			2: {"0": 2, "1": 2},
			3: {"0": 0, "1": 2},
		}
	).lego()) == "0[01]"

	# built-ins testing
	assert not null("a").accepts("a")
	assert epsilon("a").accepts("")
	assert not epsilon("a").accepts("a")

	a = fsm(
		alphabet = {"a", "b"},
		states   = {0, 1, "ob"},
		initial  = 0,
		finals   = {1},
		map      = {
			0    : {"a" : 1   , "b" : "ob"},
			1    : {"a" : "ob", "b" : "ob"},
			"ob" : {"a" : "ob", "b" : "ob"},
		},
	)
	assert not a.accepts("")
	assert a.accepts("a")
	assert not a.accepts("b")

	b = fsm(
		alphabet = {"a", "b"},
		states   = {0, 1, "ob"},
		initial  = 0,
		finals   = {1},
		map      = {
			0    : {"a" : "ob", "b" : 1   },
			1    : {"a" : "ob", "b" : "ob"},
			"ob" : {"a" : "ob", "b" : "ob"},
		},
	)
	assert not b.accepts("")
	assert not b.accepts("a")
	assert b.accepts("b")

	# concatenation simple test
	concAA = a + a
	assert not concAA.accepts("")
	assert not concAA.accepts("a")
	assert concAA.accepts("aa")
	assert not concAA.accepts("aaa")

	concAA = epsilon({"a", "b"}) + a + a
	assert not concAA.accepts("")
	assert not concAA.accepts("a")
	assert concAA.accepts("aa")
	assert not concAA.accepts("aaa")

	concAB = a + b
	assert not concAB.accepts("")
	assert not concAB.accepts("a")
	assert not concAB.accepts("b")
	assert not concAB.accepts("aa")
	assert concAB.accepts("ab")
	assert not concAB.accepts("ba")
	assert not concAB.accepts("bb")

	# alternation simple test
	altA = a | null({"a", "b"})
	assert not altA.accepts("")
	assert altA.accepts("a")

	altAB = a | b
	assert not altAB.accepts("")
	assert altAB.accepts("a")
	assert altAB.accepts("b")
	assert not altAB.accepts("aa")
	assert not altAB.accepts("ab")
	assert not altAB.accepts("ba")
	assert not altAB.accepts("bb")

	# fsmstar simple test
	starA = a.star()
	assert starA.accepts("")
	assert starA.accepts("a")
	assert not starA.accepts("b")
	assert starA.accepts("aaaaaaaaa")

	# multiplication simple test
	twoA = a * 2
	assert not twoA.accepts("")
	assert not twoA.accepts("a")
	assert twoA.accepts("aa")
	assert not twoA.accepts("aaa")

	zeroA = a * 0
	assert zeroA.accepts("")
	assert not zeroA.accepts("a")

	# intersection simple test
	intAB = a & b
	assert not intAB.accepts("")
	assert not intAB.accepts("a")
	assert not intAB.accepts("b")

	everythingbutA = a.everythingbut()
	assert everythingbutA.accepts("")
	assert not everythingbutA.accepts("a")
	assert everythingbutA.accepts("b")
	assert everythingbutA.accepts("aa")
	assert everythingbutA.accepts("ab")

	# this is "0*1" in heavy disguise. crawl should resolve this duplication
	# Notice how states 2 and 3 behave identically. When resolved together,
	# states 1 and 2&3 also behave identically, so they, too should be resolved
	# (this is impossible to spot before 2 and 3 have been combined).
	merged = fsm(
		alphabet = {"0", "1"},
		states   = {1, 2, 3, 4, "oblivion"},
		initial  = 1,
		finals   = {4},
		map      = {
			1          : {"0" : 2         , "1" : 4         },
			2          : {"0" : 3         , "1" : 4         },
			3          : {"0" : 3         , "1" : 4         },
			4          : {"0" : "oblivion", "1" : "oblivion"},
			"oblivion" : {"0" : "oblivion", "1" : "oblivion"},
		}
	).reduce()
	assert len(merged.states) == 3

	# this is (a*ba)*
	starred = fsm(
		alphabet = {"a", "b"},
		states   = {0, 1, 2, "oblivion"},
		initial  = 0,
		finals   = {2},
		map      = {
			0          : {"a" : 0         , "b" : 1         },
			1          : {"a" : 2         , "b" : "oblivion"},
			2          : {"a" : "oblivion", "b" : "oblivion"},
			"oblivion" : {"a" : "oblivion", "b" : "oblivion"},
		}
	).star()
	assert starred.alphabet == frozenset(["a", "b"])
	assert starred.accepts("")
	assert not starred.accepts("a")
	assert not starred.accepts("b")
	assert not starred.accepts("aa")
	assert starred.accepts("ba")
	assert starred.accepts("aba")
	assert starred.accepts("aaba")
	assert not starred.accepts("aabb")
	assert starred.accepts("abababa")

	# reduce() behaviour test
	# FSM accepts no strings but has 3 states, needs only 1
	asdf = fsm(
		alphabet = {None},
		states   = {0, 1, 2},
		initial  = 0,
		finals   = {1},
		map = {
			0 : {None : 2},
			1 : {None : 2},
			2 : {None : 2},
		},
	)
	asdf = asdf.reduce()
	assert len(asdf.states) == 1

	# FSM reversal
	abc = fsm(
		alphabet = {"a", "b", "c"},
		states   = {0, 1, 2, 3, None},
		initial  = 0,
		finals   = {3},
		map = {
			0    : {"a" : 1   , "b" : None, "c" : None},
			1    : {"a" : None, "b" : 2   , "c" : None},
			2    : {"a" : None, "b" : None, "c" : 3   },
			3    : {"a" : None, "b" : None, "c" : None},
			None : {"a" : None, "b" : None, "c" : None},
		},
	)
	cba = reversed(abc)
	assert cba.accepts("cba")

	# This is (a|b)*a(a|b)
	brzozowski = fsm(
		alphabet = {"a", "b"},
		states = {"A", "B", "C", "D", "E"},
		initial = "A",
		finals = {"C", "E"},
		map = {
			"A" : {"a" : "B", "b" : "D"},
			"B" : {"a" : "C", "b" : "E"},
			"C" : {"a" : "C", "b" : "E"},
			"D" : {"a" : "B", "b" : "D"},
			"E" : {"a" : "B", "b" : "D"},
		},
	)
	assert brzozowski.accepts("aa")
	assert brzozowski.accepts("ab")
	assert brzozowski.accepts("aab")
	assert brzozowski.accepts("bab")
	assert brzozowski.accepts("abbbbbbbab")
	assert not brzozowski.accepts("")
	assert not brzozowski.accepts("a")
	assert not brzozowski.accepts("b")
	assert not brzozowski.accepts("ba")
	assert not brzozowski.accepts("bb")
	assert not brzozowski.accepts("bbbbbbbbbbbb")

	# So this is (a|b)a(a|b)*
	b2 = reversed(brzozowski)
	assert b2.accepts("aa")
	assert b2.accepts("ba")
	assert b2.accepts("baa")
	assert b2.accepts("bab")
	assert b2.accepts("babbbbbbba")
	assert not b2.accepts("")
	assert not b2.accepts("a")
	assert not b2.accepts("b")
	assert not b2.accepts("ab")
	assert not b2.accepts("bb")
	assert not b2.accepts("bbbbbbbbbbbb")

	# Test string generator functionality.
	gen = b2.strings()
	assert next(gen) == ["a", "a"]
	assert next(gen) == ["b", "a"]
	assert next(gen) == ["a", "a", "a"]
	assert next(gen) == ["a", "a", "b"]
	assert next(gen) == ["b", "a", "a"]
	assert next(gen) == ["b", "a", "b"]
	assert next(gen) == ["a", "a", "a", "a"]

	# epsilon reversed is epsilon
	assert reversed(epsilon("a")).accepts("")

	# Bug fix. This is a(a{2})* (i.e. accepts an odd number of "a" chars in a
	# row), but when .lego() is called, the result is "a+". Turned out to be
	# a fault in the lego.multiplier.__mul__() routine
	elesscomplex = fsm(
		alphabet = {"a"},
		states = {0, 1},
		initial = 0,
		finals = {1},
		map = {
			0 : {"a" : 1},
			1 : {"a" : 0},
		},
	)
	assert not elesscomplex.accepts("")
	assert elesscomplex.accepts("a")
	assert not elesscomplex.accepts("aa")
	assert elesscomplex.accepts("aaa")
	elesscomplex = elesscomplex.lego()
	assert str(elesscomplex) in {"a(aa)*", "(aa)*a"}
	elesscomplex = elesscomplex.fsm()
	assert not elesscomplex.accepts("")
	assert elesscomplex.accepts("a")
	assert not elesscomplex.accepts("aa")
	assert elesscomplex.accepts("aaa")
	gen = elesscomplex.strings()
	assert next(gen) == ["a"]
	assert next(gen) == ["a", "a", "a"]
	assert next(gen) == ["a", "a", "a", "a", "a"]
	assert next(gen) == ["a", "a", "a", "a", "a", "a", "a"]

	# Binary numbers divisible by 3.
	# Disallows the empty string
	# Allows "0" on its own, but not leading zeroes.
	div3 = fsm(
		alphabet = {"0", "1"},
		states = {"initial", "zero", 0, 1, 2, None},
		initial = "initial",
		finals = {"zero", 0},
		map = {
			"initial" : {"0" : "zero", "1" : 1   },
			"zero"    : {"0" : None  , "1" : None},
			0         : {"0" : 0     , "1" : 1   },
			1         : {"0" : 2     , "1" : 0   },
			2         : {"0" : 1     , "1" : 2   },
			None      : {"0" : None  , "1" : None},
		},
	)
	assert not div3.accepts("")
	assert div3.accepts("0")
	assert not div3.accepts("1")
	assert not div3.accepts("00")
	assert not div3.accepts("01")
	assert not div3.accepts("10")
	assert div3.accepts("11")
	assert not div3.accepts("000")
	assert not div3.accepts("001")
	assert not div3.accepts("010")
	assert not div3.accepts("011")
	assert not div3.accepts("100")
	assert not div3.accepts("101")
	assert div3.accepts("110")
	assert not div3.accepts("111")
	assert not div3.accepts("0000")
	assert not div3.accepts("0001")
	assert not div3.accepts("0010")
	assert not div3.accepts("0011")
	assert not div3.accepts("0100")
	assert not div3.accepts("0101")
	assert not div3.accepts("0110")
	assert not div3.accepts("0111")
	assert not div3.accepts("1000")
	assert div3.accepts("1001")
	div3 = div3.lego()
	assert str(div3) == "0|1(01*0|10*1)*10*"
	gen = div3.strings()
	assert next(gen) == "0"
	assert next(gen) == "11"
	assert next(gen) == "110"
	assert next(gen) == "1001"
	assert next(gen) == "1100"

	# Machine accepts only numbers in selected base (e.g. 2, 10) that are
	# divisible by N (e.g. 3, 7).
	# "0" alone is acceptable, but leading zeroes (e.g. "00", "07") are not
	base = 2
	N = 3
	assert base <= 10
	divN = fsm(
		alphabet = set(str(i) for i in range(base)),
		states = set(range(N)) | {"initial", "zero", None},
		initial = "initial",
		finals = {"zero", 0},
		map = dict(
			[
				("initial", dict([(str(j), j              % N) for j in range(1, base)] + [("0", "zero")])),
				("zero"   , dict([(str(j), None              ) for j in range(   base)]                  )),
				(None     , dict([(str(j), None              ) for j in range(   base)]                  )),
			] + [
				(i        , dict([(str(j), (i * base + j) % N) for j in range(   base)]                  ))
				for i in range(N)
			]
		),
	)
	gen = divN.lego().strings()
	a = next(gen)
	assert a == "0"
	for i in range(7):
		b = next(gen)
		assert int(a, base) + N == int(b, base)
		a = b

	print("OK")

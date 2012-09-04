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
		raise Exception("Can't set " + str(self) + " attribute " + str(name) + " to " + str(value))

	def __init__(self, alphabet, states, initial, finals, map):
		'''Initialise the hard way due to immutability.'''
		self.__dict__["alphabet"] = alphabet
		self.__dict__["states"  ] = set(states)
		self.__dict__["initial" ] = initial
		self.__dict__["finals"  ] = set(finals)
		self.__dict__["map"     ] = map

		# Validation. Thanks to immutability, this only needs to be carried out once.
		if self.initial not in self.states:
			raise Exception("Initial state " + str(self.initial) + " not in " + str(self.states))
		if not self.finals.issubset(self.states):
			raise Exception("Final states " + str(self.finals) + " not in " + str(self.states))
		for state in self.states:
			if state not in self.map.keys():
				raise Exception("State " + str(state) + " not in " + str(self.map.keys()))
			for symbol in self.alphabet:
				if symbol not in self.map[state]:
					raise Exception("Symbol " + str(symbol) + " not in " + str(self.map[state]))
				if self.map[state][symbol] not in self.states:
					raise Exception("State " + str(self.map[state][symbol]) + " not in " + str(self.states))

	def accepts(self, input):
		'''This is actually only used for unit testing purposes'''
		state = self.initial
		for symbol in input:
			state = self.map[state][symbol]
		return state in self.finals

	def equivalent(self, state1, state2):
		'''
			See whether two states in this state machine are
			functionally equivalent: that is, they have the same
			finality and the same transition function.
			Equivalent states appear quite commonly in automatically-
			generated FSMs, where it's equally common to merge()
			them once discovered.
			This could be broadened to 3 or more states quite simply...
		'''
		if (state1 in self.finals) != (state2 in self.finals):
			return False

		# hypothetically merge state2 into state1. What do the
		# transitions look like?
		for symbol in self.alphabet:
			next1 = self.map[state1][symbol]
			next2 = self.map[state2][symbol]
			if next1 == state2:
				next1 = state1
			if next2 == state2:
				next2 = state1
			if next1 != next2:
				return False

		return True

	def automerge(self):
		'''
			Search through our own states looking for duplicates.
			If found, merge them and repeat. If not, return
		'''

		def trymerge(self):
			'''
				Find and merge two equivalent states, return True.
				Return False if they can't be found.
			'''
			for this in self.states:
				for that in self.states:
					if that == this:
						continue
					if self.equivalent(this, that):
						return True, self.replace(this, that)
			return False, self

		# Do it until it hurts
		new = self
		again = True
		while(again):
			again, new = trymerge(new)

		return new

	def replace(self, this, that):
		'''
			Return a new FSM with that replaced with this,
			including all references in the map, etc.
			This is used when renumbering() the FSM.
		'''

		lookups = dict([
			(s, that if s == this else s)
			for s in self.states
		])

		# return new, modified
		return fsm(
			alphabet = self.alphabet,
			states   = set([lookups[s] for s in self.states]),
			initial  = lookups[self.initial],
			finals   = set([lookups[s] for s in self.finals]),
			map      = dict([
				(lookups[s], dict([
					(a, lookups[self.map[s][a]])
					for a in self.map[s].keys()
				]))
				for s in self.map.keys()
			]),
		)

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
		rows.insert(1, ["-" * colwidths[x] for x in range(len(rows[0]))])

		return "".join("".join(rows[y]) + "\n" for y in range(len(rows)))

	def __add__(self, other):
		'''
			Concatenate two finite state machines together.
			For example, if self accepts "0*" and other accepts "1+(0|1)",
			will return a finite state machine accepting "0*1+(0|1)".
			Accomplished by effectively following non-deterministically.
			Call using "fsm3 = fsm1 + fsm2"
		'''
		# alphabets must be equal
		if other.alphabet != self.alphabet:
			raise Exception("Alphabet " + str(other.alphabet) + " must be " + str(self.alphabet))

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

		return _crawl(self.alphabet, initial, final, follow)

	def star(self):
		'''
			If the present FSM accepts X, returns an FSM accepting X* (i.e. 0 or
			more Xes).

			This is NOT as simple as naively connecting the final states back to the
			initial state: see (b*ab)* for example.
			
			Instead we must create an articial "omega state" which is our only accepting
			state and which dives into the FSM and from which all exits return.
		'''

		omega = 0
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

		return _crawl(self.alphabet, initial, final, follow)

	def __mul__(self, multiplier):
		'''
			Given an FSM and a multiplier, return the multiplied FSM.
		'''
		if multiplier < 0:
			raise Exception("Can't multiply an FSM by a number less than 0.")

		if multiplier == 0:
			return epsilon(self.alphabet)

		# worked example: multiplier = 5
		output = self
		# accepts e.g. "ab"

		for i in range(multiplier - 1):
			output += self
		# now accepts e.g. "ababababab"

		return output

	def __or__(self, other):
		'''
			Alternation.
			Return a finite state machine which accepts any sequence of symbols
			that is accepted by either self or other.
			Call using "fsm3 = fsm1 | fsm2"
		'''

		# alphabets must be equal
		if other.alphabet != self.alphabet:
			raise Exception("Alphabet " + str(other.alphabet) + " must be " + str(self.alphabet))

		initial = (self.initial, other.initial)

		# dedicated function accepts a "superset" and returns the next "superset"
		# obtained by following this transition in the new FSM
		def follow(current, symbol):
			return (
				self.map[current[0]][symbol],
				other.map[current[1]][symbol]
			)

		# state is final if *any* of its internal states are final
		def final(state):
			return state[0] in self.finals \
			or state[1] in other.finals

		return _crawl(self.alphabet, initial, final, follow)

	def __and__(self, other):
		'''
			Intersection.
			Take FSMs and AND them together. That is, return an FSM which
			accepts any sequence of symbols that is accepted by all of the original
			FSMs.
			Call using "fsm3 = fsm1 & fsm2"
		'''

		# alphabets must be equal
		if other.alphabet != self.alphabet:
			raise Exception("Alphabet " + str(other.alphabet) + " must be " + str(self.alphabet))

		initial = (self.initial, other.initial)

		# dedicated function accepts a "superset" and returns the next "superset"
		# obtained by following this transition in the new FSM
		def follow(current, symbol):
			return (
				self.map[current[0]][symbol],
				other.map[current[1]][symbol],
			)

		# state is final if *all* of its substates are final
		def final(state):
			return state[0] in self.finals \
			and state[1] in other.finals

		return _crawl(self.alphabet, initial, final, follow)

	def lego(self):
		'''
			This is the big kahuna of this module.
			Turn the present FSM into a regular expression object, as imported
			from the lego module.

			This is accomplished by considering the FSM as a set of "equations"
			which relate state-sets to other state-sets using transitions.
			We start at a state-set composed of all the possible final
			states, then we find all the possible routes to that final
			state-set, using substitution.
		'''
		from lego import nothing, charclass, emptystring, star, otherchars

		# Represents a "state-set" which is totally external to the FSM.
		# This is different from the empty state-set. From the empty state-set,
		# every transition leads to the empty state-set again. But from
		# "outside", consuming the empty string puts you at the initial state
		# of the FSM.
		outside = None

		class equation:
			'''
				This is a small representation of all the strings which could be used to
				REACH the current state-set.

				E.g. if:
					'|0|33|A1|A4|B1|B75|C8'       = A
					'(|0|33)|A(1|4)|B(1|75)|C(8)' = A
					'(|0|33)|A[14]|B(1|75)|C8'    = A
				where A, B and C are state-sets and 0, 1, 3, 4, 5, 7 and 8 are symbols
				then:

				equation = {
					"lefts" : {
						None : {
							conc(),
							charclass("0"),
							mult(charclass("3"), multiplier(2, 2)),
						},
						"A" : {
							charclass("1", "4"),
						},
						"B" : {
							charclass("1"),
							conc(
								mult(charclass("7"), one),
								mult(charclass("5"), one),
							),
						),
						"C" : {
							charclass("8"),
						),
					},
					"right" : "A",
				}

				Notice how lefts is a dict of sets of lego bits. These
				can be freely combined using methods in the lego module.
			'''

			def __init__(self, right, fsm):

				# The equation needs to know what it represents.
				# This is for elimination purposes.
				self.right = right

				# A simple dictionary of (state-set, transition symbol) indicating
				# transitions leading FROM the other state-set TO the present state-set
				# under that transition.
				# Some state-sets can be reached by transitions (directly or
				# indirectly) from themselves. Initially these will be just single
				# internal transitions (e.g. f(A, 0) = A) but as backfilling continues
				# more complex loops will appear
				self.lefts = {}

				for symbol in sorted(fsm.alphabet, key=str):

					# find every possible way to reach the current state set
					# using this symbol
					left = frozenset([
						prev
						for prev in fsm.map
						for state in right
						if fsm.map[prev][symbol] == state
					])

					# "otherchars" should never actually
					# be used as a character in a charclass and throws an exception
					# when you try.
					if symbol == otherchars:
						self.addtransition(left, ~charclass(fsm.alphabet - {otherchars}))

					else:
						self.addtransition(left, charclass({symbol}))

				# initial alone can be reached via an empty string ;)
				if fsm.initial in right:
					self.addtransition(outside, emptystring)

			def addtransition(self, left, element):
				if left in self.lefts:
					self.lefts[left] |= element
				else:
					self.lefts[left] = element

			def applyloops(self):
				'''
					Remove the self-transition from an equation
					e.g. "A0 | B1 | C2 = A" becomes "B10* | C10* = A"
				'''
				if self.right not in self.lefts:
					return

				loop = self.lefts[self.right] * star
				del self.lefts[self.right]
				
				for left in self.lefts:
					self.lefts[left] = self.lefts[left] + loop

			def substitute(self, other):
				'''
					Take the equation of some other state-set and substitute it into
					this equation, cancelling out any references to the other.
				'''

				# No transition from other to self? Then no substitution is required.
				if other.right not in self.lefts:
					return

				# Now how about dynamic routes here
				for left in other.lefts:

					# If other still has self-transitions, that's a mistake.
					if left == other.right:
						raise Exception("Did you forget applyloops()?")

					# any transition from left to other.right, coupled with
					# the universal transition from other.right to *here*, counts as
					# as transition from left to here.
					self.addtransition(
						left,
						other.lefts[left] + self.lefts[other.right]
					)

				del self.lefts[other.right]

			def __str__(self):
				string = ""
				string += "lefts:\n"
				for left in self.lefts:
					string += " " + repr(left) + ": " + repr(self.lefts[left]) + "\n"
				string += "right: " + repr(self.right) + "\n"
				string += "\n"
				return string

		# Now that we have equations down, here is how we actually
		# run this thing.

		# This first equation is the critical one. We solve for
		# this.
		equations = [equation(frozenset(self.finals), self)]

		# Iterate over a growing list, generating further equations.
		i = 0;
		while i < len(equations):

			# record newly-found state-sets for future reference (no dupes)
			for right in equations[i].lefts:
				if right != outside \
				and right not in [e.right for e in equations]:
					equations.append(equation(right, self))

			i += 1

		# Next, we start at the end of our list, and substitute backwards
		# to show all possible routes.
		for i in reversed(range(len(equations))):
			equations[i].applyloops()
			for j in reversed(range(i)):
				equations[j].substitute(equations[i])

		# By this point all back-substitutions have been performed and the final
		# element in equations[] should be ready to convert into a regex.
		# Only "outside" (static transitions to final states) should be left after
		# the back-substitution is completed.
		try:
			return equations[0].lefts[outside]

		# If no such transition exists, or it is empty, then an exception arises,
		# as there are no static strings leading to the final state-set.
		# That means there's no pattern. So:
		except KeyError:
			return nothing

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

def _crawl(alphabet, initial, final, follow):
	'''
		Given the above conditions and instructions, crawl a new
		unknown FSM, mapping its states, final states and
		transitions. Return the new one.
		This is a pretty powerful procedure which could potentially go on
		forever if you supply an evil version of follow()
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

	result = fsm(alphabet, range(len(states)), 0, finals, map)
	result = result.automerge()
	# TODO: make initial 0 after automerging.
	return result

# Unit tests.
if __name__ == "__main__":
	# Buggggs.
	abstar = fsm(
		{'a', None, 'b'},
		{0, 1},
		0,
		{0},
		{
			0: {'a': 0, None: 1, 'b': 0},
			1: {'a': 1, None: 1, 'b': 1}
		}
	)
	assert str(abstar.lego()) == "[ab]*"

	adotb = fsm(
		{'a', None, 'b'},
		{0, 1, 2, 3, 4},
		0,
		{4},
		{
			0: {'a': 2, None: 1, 'b': 1},
			1: {'a': 1, None: 1, 'b': 1},
			2: {'a': 3, None: 3, 'b': 3},
			3: {'a': 1, None: 1, 'b': 4},
			4: {'a': 1, None: 1, 'b': 1}
		}
	)
	assert str(adotb.lego()) == "a.b"

	from lego import otherchars

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

	# Equivalence testing.
	mergeme = fsm(
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
		},
	)
	assert mergeme.equivalent(1, 1)
	assert not mergeme.equivalent(1, 2)
	assert not mergeme.equivalent(1, 3)
	assert not mergeme.equivalent(1, 4)
	assert not mergeme.equivalent(1, "oblivion")
	assert mergeme.equivalent(2, 2)
	assert mergeme.equivalent(2, 3) # the important one
	assert not mergeme.equivalent(2, 4)
	assert not mergeme.equivalent(2, "oblivion")
	assert mergeme.equivalent(3, 3)
	assert not mergeme.equivalent(3, 4)
	assert not mergeme.equivalent(3, "oblivion")
	assert mergeme.equivalent(4, 4)
	assert not mergeme.equivalent(4, "oblivion")
	assert mergeme.equivalent("oblivion", "oblivion")
	mergeme = mergeme.replace(3, 2)
	assert not 3 in mergeme.states
	assert mergeme.map[2]["0"] == 2 # formerly 3
	assert mergeme.equivalent(1, 2)
	mergeme = mergeme.replace(2, 1)
	assert not 2 in mergeme.states
	assert mergeme.map[1]["0"] == 1 # formerly 2

	# Slightly more advanced equivalence testing
	# (0|1)0*
	# States 2 and 3 are "equivalent" since they can be merged
	mergeme2 = fsm(
		alphabet = {"0", "1"},
		states   = {1, 2, 3, 4},
		initial  = 1,
		finals   = {2, 3},
		map      = {
			1 : {"0" : 2, "1" : 3},
			2 : {"0" : 2, "1" : 4},
			3 : {"0" : 3, "1" : 4},
			4 : {"0" : 4, "1" : 4},
		},
	)
	assert mergeme2.equivalent(1, 1)
	assert not mergeme2.equivalent(1, 2)
	assert not mergeme2.equivalent(1, 3)
	assert not mergeme2.equivalent(1, 4)
	assert mergeme2.equivalent(2, 2)
	assert mergeme2.equivalent(2, 3) # the important one
	assert not mergeme2.equivalent(2, 4)
	assert mergeme2.equivalent(3, 3)
	assert not mergeme2.equivalent(3, 4)
	assert mergeme2.equivalent(4, 4)
	mergeme2 = mergeme2.automerge()
	assert not (2 in mergeme2.states and 3 in mergeme2.states)
	assert mergeme2.map[1]["0"] == mergeme2.map[1]["1"] # formerly 2 and 3

	# replace() test
	replaceme = fsm(
		alphabet = {"0", "1"},
		states   = {0, 1, 2},
		initial  = 0,
		finals   = {0},
		map      = {
			0 : {"0" : 0, "1" : 1},
			1 : {"0" : 1, "1" : 2},
			2 : {"0" : 2, "1" : 0},
		},
	)
	replaceme = replaceme.replace(0, None)
	assert set(replaceme.states) == {None, 1, 2}
	assert replaceme.initial == None
	assert replaceme.finals == {None}
	assert 0 not in replaceme.map
	assert replaceme.map[None]["0"] == None
	assert replaceme.map[2]["1"] == None

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
		map = {
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

	# this is "0*1" in heavy disguise. _crawl should resolve this duplication
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
	).automerge()
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

	print("OK")

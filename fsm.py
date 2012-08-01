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

class BadFsmException(Exception):
	pass

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

	def __init__(self, alphabet, states, initialState, finalStates, map):
		'''Initialise the hard way due to immutability.'''
		self.__dict__["alphabet"    ] = alphabet
		self.__dict__["states"      ] = set(states)
		self.__dict__["initialState"] = initialState
		self.__dict__["finalStates" ] = set(finalStates)
		self.__dict__["map"         ] = map

		# Validation. Thanks to immutability, this only needs to be carried out once.
		if self.initialState not in self.states:
			raise BadFsmException("Initial state " + str(self.initialState) + " not in " + str(self.states))
		if not self.finalStates.issubset(self.states):
			raise BadFsmException("Final states " + str(self.finalStates) + " not in " + str(self.states))
		for state in self.states:
			if state not in self.map.keys():
				raise BadFsmException("State " + str(state) + " not in " + str(self.map.keys()))
			for symbol in self.alphabet:
				if symbol not in self.map[state]:
					raise BadFsmException("Symbol " + str(symbol) + " not in " + str(self.map[state]))
				if self.map[state][symbol] not in self.states:
					raise BadFsmException("State " + str(self.map[state][symbol]) + " not in " + str(self.states))

	def accepts(self, input):
		'''This is actually only used for unit testing purposes'''
		state = self.initialState
		for symbol in input:
			state = self.map[state][symbol]
		return state in self.finalStates

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
		if (state1 in self.finalStates) != (state2 in self.finalStates):
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

	def merge(self, state1, state2):
		'''
			Merge the second state into the first. Replace all references to
			the second state with references to the first. Return a new
			FSM with the merger carried out.
			You should check that equivalent(state1, state2) returns True
			before doing this. Nothing is actually stopping you though
		'''

		# this is counterproductive
		if state2 == state1:
			return self

		# states could contain duplicates in theory -_-
		states = [s for s in self.states if s != state2]

		# yes, the initial state could be a dupe
		initialState = state1 if self.initialState == state2 else self.initialState

		# finalStates could also contain dupes
		finalStates = [state1 if s == state2 else s for s in self.finalStates]

		# map
		map = {}
		for state in self.states:
			if state == state2:
				continue
			else:
				map[state] = {}
			for symbol in self.alphabet:
				if self.map[state][symbol] == state2:
					map[state][symbol] = state1
				else:
					map[state][symbol] = self.map[state][symbol]

		# return new, modified
		return fsm(self.alphabet, states, initialState, finalStates, map)

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
						return True, self.merge(this, that)
			return False, self

		# Do it until it hurts
		new = self
		keepMerging = True
		while(keepMerging):
			keepMerging, new = trymerge(new)

		return new

	def replace(self, state1, state2):
		'''
			Return a new FSM with state1 replaced with state2,
			including all references in the map, etc.
			This is used when renumbering() the FSM.
		'''

		# accomplish nothing in style!
		if state1 not in self.states:
			return self

		# this is counterproductive
		if state2 == state1:
			return self

		# states
		states = [s for s in self.states if s != state1] + [state2]

		# initial state
		initialState = state2 if self.initialState == state1 else self.initialState

		# finalStates could also contain dupes
		finalStates = [s for s in self.finalStates if s != state1]
		if state1 in self.finalStates:
			finalStates.append(state2)

		# map
		# This could probably be done as a one-liner but never mind
		map = {}
		for state in self.states:
			if state == state1:
				map[state2] = {}
			else:
				map[state] = {}
			for symbol in self.alphabet:
				if state == state1:
					if self.map[state][symbol] == state1:
						map[state2][symbol] = state2
					else:
						map[state2][symbol] = self.map[state][symbol]
				else:
					if self.map[state][symbol] == state1:
						map[state][symbol] = state2
					else:
						map[state][symbol] = self.map[state][symbol]

		# return new, modified
		return fsm(self.alphabet, states, initialState, finalStates, map)

	def renumber(self):
		'''
			Automatically-generated FSMs often have really strange
			states, like frozensets or tuples. These are hard to
			read in a printout. So, replace whatever states we currently
			have with numbers beginning at zero. NICE
		'''
		# TODO: make it so state 0 is the initial state
		new = self
		states = list(self.states)
		for i in range(len(states)):
			new = new.replace(states[i], i)
		return new

	def __repr__(self):
		'''This routine gets called whenever you print() a FSM'''
		rows = []

		# top row
		row = ["", "name", "isFinal"]
		row.extend(str(symbol) for symbol in sorted(self.alphabet, key=str))
		rows.append(row)

		# other rows
		for state in self.states:
			row = []
			if(state == self.initialState):
				row.append("*")
			else:
				row.append("")
			row.append(str(state))
			if state in self.finalStates:
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
		if self.initialState in self.finalStates:
			newInitialState = frozenset([
				(0, self.initialState),
				(1, other.initialState),
			])
		else:
			newInitialState = frozenset([(0, self.initialState)])

		def isFinal(currentState):
			for (fsmId, state) in currentState:
				# self
				if fsmId == 0:
					if state in self.finalStates:
						if other.initialState in other.finalStates:
							return True

				# other
				elif fsmId == 1:
					if state in other.finalStates:
						return True

				else:
					raise Exception("What")

			return False

		# dedicated function accepts a "superset" and returns the next "superset"
		# obtained by following this transition in the new FSM
		def getNext(currentState, symbol):

			nextStates = []

			for (fsmId, state) in currentState:
				if fsmId == 0:
					nextStates.append((0, self.map[state][symbol]))
					# final of self? merge with other initial
					if self.map[state][symbol] in self.finalStates:
						nextStates.append((1, other.initialState))
				elif fsmId == 1:
					nextStates.append((1, other.map[state][symbol]))
				else:
					raise Exception("Whaat")

			return frozenset(nextStates)

		return _crawl(self.alphabet, newInitialState, isFinal, getNext)

	def star(self):
		'''
			If the present FSM accepts X, returns an FSM accepting X* (i.e. 0 or
			more Xes).

			This is NOT as simple as naively connecting the final states back to the
			initial state: see (b*ab)* for example.
			
			Instead we must create an articial "omega state" which is our only accepting
			state and which dives into the FSM and from which all exits return.
		'''

		omegaState = 0
		while omegaState in self.states:
			omegaState += 1

		newInitialState = frozenset([omegaState])

		def getNext(currentState, symbol):

			nextState = []

			for state in currentState:

				# the special new starting "omegaState" behaves exactly like the
				# original starting state did
				if state == omegaState:
					state = self.initialState

				nextAState = self.map[state][symbol]
				nextState.append(nextAState)

				# loop back to beginning
				if nextAState in self.finalStates:
					nextState.append(omegaState)

			return frozenset(nextState)

		# final if currentState contains omegaState
		def isFinal(currentState):
			return omegaState in currentState

		return _crawl(self.alphabet, newInitialState, isFinal, getNext)

	def __mul__(self, multiplier):
		'''
			Given an FSM and a multiplier, return the multiplied FSM.
			The minimum has to be an integer, but the maximum may be None
			to stand for infinity.
			Call using "fsm2 = fsm1 * (0, 1)"
		'''
		min, max = multiplier

		# worked example: (min, max) = (5, 7) or (5, None)

		output = epsilon(self.alphabet)
		# accepts ""

		for i in range(min):
			output += self
		# now accepts e.g. "ababababab"

		# unlimited additional copies
		if max is None:
			output += self.star()
			# now accepts e.g. "ababababab(ab)*" = "(ab){5,}"

		# finite additional copies
		else:
			q = self | epsilon(self.alphabet)
			# accepts "(ab)?"

			for i in range(min, max):
				output += q
				# now accepts e.g. "ababababab(ab)?(ab)?" = "(ab){5,7}"

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

		newInitialState = (self.initialState, other.initialState)

		# dedicated function accepts a "superset" and returns the next "superset"
		# obtained by following this transition in the new FSM
		def getNext(currentState, symbol):
			return (
				self.map[currentState[0]][symbol],
				other.map[currentState[1]][symbol]
			)

		# currentState is final if *any* of its internal states are final
		def isFinal(currentState):
			return currentState[0] in self.finalStates \
			or currentState[1] in other.finalStates

		return _crawl(self.alphabet, newInitialState, isFinal, getNext)

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

		newInitialState = (self.initialState, other.initialState)

		# dedicated function accepts a "superset" and returns the next "superset"
		# obtained by following this transition in the new FSM
		def getNext(currentState, symbol):
			return (
				self.map[currentState[0]][symbol],
				other.map[currentState[1]][symbol]
			)

		# currentState is final if *all* of its internal states are final
		def isFinal(currentState):
			return currentState[0] in self.finalStates \
			and currentState[1] in other.finalStates

		return _crawl(self.alphabet, newInitialState, isFinal, getNext)

	def pattern(self):
		'''
			This is the big kahuna of this module.
			Turn the present FSM into a regular expression pattern object, as imported
			from the lego module.

			This is accomplished by considering the FSM as a set of "equations"
			which relate state-sets to other state-sets using transitions.
			We start at a state-set composed of all the possible final
			states, then we find all the possible routes to that final
			state-set, using substitution.
		'''
		from lego import nothing, charclass, emptystring, star

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

				# the equation needs to know what it represents
				# this is for elimination purposes
				self.right = frozenset(right)

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
					left = set()
					for state in right:
						for key in fsm.map:
							if fsm.map[key][symbol] == state:
								left.add(key)
					left = frozenset(left)

					# ignore unreachables
					if left == frozenset():
						continue

					# "None" is considered to be a stand-in for "every symbol
					# not explicitly named in the rest of our alphabet".
					# That is, [^abcd...z] or similar. "None" should never actually
					# be used as a character in a charclass and throws an exception
					# when you try.
					if symbol is None:
						self.__addTransition(left, ~charclass(fsm.alphabet - {None}))

					else:
						self.__addTransition(left, charclass({symbol}))

				# initialState alone can be reached via an empty string ;)
				if fsm.initialState in right:
					self.__addTransition(None, emptystring)

			def __addTransition(self, left, element):
				if left not in self.lefts:
					self.lefts[left] = nothing
				self.lefts[left] |= element

			# remove the self-transition from an equation
			# e.g. "A0 | B1 | C2 = A" becomes "B10* | C10* = A"
			def applyLoops(self):
				if self.right not in self.lefts:
					return

				loop = self.lefts[self.right] * star
				del self.lefts[self.right]
				
				for left in self.lefts:
					# TODO: This looks a little overengineered
					transition = self.lefts[left] + loop
					del self.lefts[left]
					self.__addTransition(left, transition)

			# take the equation of some other state-set and substitute it into
			# this equation, cancelling out any references to the other.
			def eliminate(self, other):

				# No transition from other to self? Then no substitution is required.
				if other.right not in self.lefts:
					return

				# Now how about dynamic routes here
				for otherLeft in other.lefts:

					# self-transition?
					if otherLeft == other.right:
						raise Exception("Did you forget applyLoops()?")

					# any transition from otherLeft to otherRight, coupled with
					# the universal transition from otherRight to *here*, counts as
					# as transition from otherLeft to here.
					otherTransition = other.lefts[otherLeft] + self.lefts[other.right]

					self.__addTransition(otherLeft, otherTransition)

				del self.lefts[other.right]

			def __repr__(self):
				string = ""
				string += "lefts:\n"
				for left in self.lefts:
					string += " " + str(left) + ": " + \
					str(self.lefts[left]) + "\n"
				string += "right: " + str(self.right) + "\n"
				string += "\n"
				return string

		# Now that we have the equations down, here is how we actually
		# run this thing.

		# Iterate over a growing list, generating equations
		equations = [equation(self.finalStates, self)]
		i = 0;
		while i < len(equations):

			# record newly-found state-sets for future reference (no dupes)
			for right in equations[i].lefts:

				if right is not None \
				and right not in [e.right for e in equations]:
					equations.append(equation(right, self))

			i += 1

		# Next, we start at the end of our list, and fill backwards
		# to show all possible routes.
		for i in reversed(range(len(equations))):
			equations[i].applyLoops()
			for j in reversed(range(i)):
				equations[j].eliminate(equations[i])

		# By this point all back-substitutions have been performed and the final
		# element in equations[] should be ready to convert into a regex.
		# Only "None" (static transitions to final states) should be left after
		# the back-substitution is completed.
		try:
			return equations[0].lefts[None]

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
		alphabet     = alphabet,
		states       = {0},
		initialState = 0,
		finalStates  = set(),
		map          = {
			0: dict([(symbol, 0) for symbol in alphabet]),
		},
	)

def epsilon(alphabet):
	'''
		Return an FSM matching an empty string, "", only.
		This is very useful in many situations
	'''
	return fsm(
		alphabet     = alphabet,
		states       = {0, 1},
		initialState = 0,
		finalStates  = {0},
		map          = {
			0: dict([(symbol, 1) for symbol in alphabet]),
			1: dict([(symbol, 1) for symbol in alphabet]),
		},
	)

def _crawl(alphabet, initialState, isFinal, getNext):
	'''
		Given the above conditions and instructions, crawl a new
		unknown FSM, mapping its states, final states and
		transitions. Return the new one.
		This is a pretty powerful procedure which could potentially go on
		forever if you supply an evil version of getNext()
	'''

	states = [initialState] # will convert to set later
	finalStates = set()
	map = {}

	# iterate over a growing list
	i = 0
	while i < len(states):
		state = states[i]

		# add to finalStates
		if isFinal(state):
			finalStates.add(state)

		# compute map for this state
		map[state] = {}
		for symbol in sorted(alphabet, key=str):
			nextState = getNext(state, symbol)

			if nextState not in states:
				states.append(nextState)

			map[state][symbol] = nextState

		i += 1

	result = fsm(alphabet, states, initialState, finalStates, map)
	result = result.automerge()
	result = result.renumber()
	return result

# Unit tests.
if __name__ == "__main__":

	# Odd bug with fsm.__add__(), exposed by "[bc]*c"
	int5A = fsm(
		alphabet     = {"a", "b", "c", None},
		states       = {0, 1},
		initialState = 1,
		finalStates  = {1},
		map = {
			0: {None: 0, "a": 0, "b": 0, "c": 0},
			1: {None: 0, "a": 0, "b": 1, "c": 1},
		}
	)
	assert int5A.accepts("")

	int5B = fsm(
		alphabet     = {"a", "b", "c", None},
		states       = {0, 1, 2},
		initialState = 1,
		finalStates  = {0},
		map = {
			0: {None: 2, "a": 2, "b": 2, "c": 2},
			1: {None: 2, "a": 2, "b": 2, "c": 0},
			2: {None: 2, "a": 2, "b": 2, "c": 2},
		}
	)
	assert int5B.accepts("c")

	int5C = int5A + int5B
	assert int5C.accepts("c")

	# fsm.pattern()

	# Catch a recursion error
	assert fsm(
		alphabet     = {"0", "1"},
		states       = {0, 1, 2, 3},
		initialState = 3,
		finalStates  = {1},
		map          = {
			0: {"0": 1, "1": 1},
			1: {"0": 2, "1": 2},
			2: {"0": 2, "1": 2},
			3: {"0": 0, "1": 2},
		}
	).pattern().regex() == "0[01]"

	# Check FSM validation problems.

	# initialState is not a state
	try:
		fsm(
			alphabet = set(),
			states = {0},
			initialState = 1,
			finalStates = set(),
			map = {
				0: {}
			},
		)
		print("FAIL")
	except BadFsmException as e:
		pass
	
	# finalStates aren't valid
	try:
		fsm(
			alphabet = set(),
			states = {0},
			initialState = 0,
			finalStates = {1},
			map = {
				0: {}
			},
		)
		print("FAIL")
	except BadFsmException as e:
		pass

	# map keys != states
	try:
		fsm(
			alphabet = set(),
			states = {0},
			initialState = 0,
			finalStates = set(),
			map = {},
		)
		print("FAIL")
	except BadFsmException as e:
		pass
	
	# map key keys != alphabet
	try:
		fsm(
			alphabet = {"a"},
			states = {0},
			initialState = 0,
			finalStates = set(),
			map = {
				0: {}
			},
		)
		print("FAIL")
	except BadFsmException as e:
		pass

	# Equivalence testing.
	mergeMe = fsm(
		alphabet = {"0", "1"},
		states = {1, 2, 3, 4, "oblivion"},
		initialState = 1,
		finalStates = {4},
		map = {
			1          : {"0" : 2         , "1" : 4         },
			2          : {"0" : 3         , "1" : 4         },
			3          : {"0" : 3         , "1" : 4         },
			4          : {"0" : "oblivion", "1" : "oblivion"},
			"oblivion" : {"0" : "oblivion", "1" : "oblivion"},
		},
	)
	assert mergeMe.equivalent(1, 1)
	assert not mergeMe.equivalent(1, 2)
	assert not mergeMe.equivalent(1, 3)
	assert not mergeMe.equivalent(1, 4)
	assert not mergeMe.equivalent(1, "oblivion")
	assert mergeMe.equivalent(2, 2)
	assert mergeMe.equivalent(2, 3) # the important one
	assert not mergeMe.equivalent(2, 4)
	assert not mergeMe.equivalent(2, "oblivion")
	assert mergeMe.equivalent(3, 3)
	assert not mergeMe.equivalent(3, 4)
	assert not mergeMe.equivalent(3, "oblivion")
	assert mergeMe.equivalent(4, 4)
	assert not mergeMe.equivalent(4, "oblivion")
	assert mergeMe.equivalent("oblivion", "oblivion")
	mergeMe = mergeMe.merge(2, 3)
	assert not 3 in mergeMe.states
	assert mergeMe.map[2]["0"] == 2 # formerly 3
	assert mergeMe.equivalent(1, 2)
	mergeMe = mergeMe.merge(1, 2)
	assert not 2 in mergeMe.states
	assert mergeMe.map[1]["0"] == 1 # formerly 2

	# Slightly more advanced equivalence testing
	# (0|1)0*
	# States 2 and 3 are "equivalent" since they can be merged
	mergeMe2 = fsm(
		alphabet = {"0", "1"},
		states = {1, 2, 3, 4},
		initialState = 1,
		finalStates = {2, 3},
		map = {
			1 : {"0" : 2, "1" : 3},
			2 : {"0" : 2, "1" : 4},
			3 : {"0" : 3, "1" : 4},
			4 : {"0" : 4, "1" : 4},
		},
	)
	assert mergeMe2.equivalent(1, 1)
	assert not mergeMe2.equivalent(1, 2)
	assert not mergeMe2.equivalent(1, 3)
	assert not mergeMe2.equivalent(1, 4)
	assert mergeMe2.equivalent(2, 2)
	assert mergeMe2.equivalent(2, 3) # the important one
	assert not mergeMe2.equivalent(2, 4)
	assert mergeMe2.equivalent(3, 3)
	assert not mergeMe2.equivalent(3, 4)
	assert mergeMe2.equivalent(4, 4)
	mergeMe2 = mergeMe2.automerge()
	assert not (2 in mergeMe2.states and 3 in mergeMe2.states)
	assert mergeMe2.map[1]["0"] == mergeMe2.map[1]["1"] # formerly 2 and 3

	# replace() test
	replaceMe = fsm(
		alphabet = {"0", "1"},
		states = {0, 1, 2},
		initialState = 0,
		finalStates = {0},
		map = {
			0 : {"0" : 0, "1" : 1},
			1 : {"0" : 1, "1" : 2},
			2 : {"0" : 2, "1" : 0},
		},
	)
	replaceMe = replaceMe.replace(0, None)
	assert set(replaceMe.states) == {None, 1, 2}
	assert replaceMe.initialState == None
	assert replaceMe.finalStates == {None}
	assert 0 not in replaceMe.map
	assert replaceMe.map[None]["0"] == None
	assert replaceMe.map[2]["1"] == None

	# renumber() test
	renumberMe = fsm(
		alphabet = {"0", "1"},
		states = {"marty", "biff", "doc"},
		initialState = "marty",
		finalStates = {"biff"},
		map = {
			"marty" : {"0" : "marty", "1" : "doc"  },
			"biff"  : {"0" : "biff" , "1" : "marty"},
			"doc"   : {"0" : "doc"  , "1" : "biff" },
		},
	)
	renumberMe = renumberMe.renumber()
	assert set(renumberMe.states) == {0, 1, 2}
	assert renumberMe.accepts("0100010111")
	
		# built-ins testing
	assert not null("a").accepts("a")
	assert epsilon("a").accepts("")
	assert not epsilon("a").accepts("a")

	a = fsm(
		alphabet = {"a", "b"},
		states = {0, 1, "ob"},
		initialState = 0,
		finalStates = {1},
		map = {
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
		states = {0, 1, "ob"},
		initialState = 0,
		finalStates = {1},
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
	twoA = a * (2, 2)
	assert not twoA.accepts("")
	assert not twoA.accepts("a")
	assert twoA.accepts("aa")
	assert not twoA.accepts("aaa")

	fourormoreA = a * (4, None)
	assert not fourormoreA.accepts("")
	assert not fourormoreA.accepts("a")
	assert not fourormoreA.accepts("aa")
	assert not fourormoreA.accepts("aaa")
	assert fourormoreA.accepts("aaaa")
	assert fourormoreA.accepts("aaaaa")
	assert fourormoreA.accepts("aaaaaa")
	assert fourormoreA.accepts("aaaaaaa")

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
		states = {1, 2, 3, 4, "oblivion"},
		initialState = 1,
		finalStates = {4},
		map = {
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
		states = {0, 1, 2, "oblivion"},
		initialState = 0,
		finalStates = {2},
		map = {
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

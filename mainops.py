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

import copy
from lego import lego, charclass, mult, conc, pattern, multiplier, one, star, emptystring
from fsm import fsm, epsilon, null

def fsmbuild(thisPattern, alphabet):
	'''Given a pattern (nested data structure representation of a regex),
	return a finite state machine which will recognise any string acceptable
	by the original regex.'''

	fsm1 = null(alphabet)
	for thisConc in thisPattern.concs:

		# start with a component accepting only the empty string
		fsm2 = epsilon(alphabet)
		for thisMult in thisConc.mults:

			multiplicand = thisMult.multiplicand

			# subpattern. recurse.
			if type(multiplicand) is pattern:
				fsm3 = fsmbuild(multiplicand, alphabet)

			# otherwise, assume it is a set of characters. Make a singular
			# FSM accepting only these characters
			else:
				initial, final, oblivion = 0, 1, 2

				initialMap  = {}
				finalMap    = {}
				oblivionMap = {}
				for symbol in alphabet:
					if symbol in multiplicand.chars:
						initialMap[symbol] = final
					else:
						initialMap[symbol] = oblivion
					finalMap[symbol] = oblivion
					oblivionMap[symbol] = oblivion

				fsm3 = fsm(
					alphabet     = alphabet,
					states       = {initial, final, oblivion},
					initialState = initial,
					finalStates  = {final},
					map          = {
						initial  : initialMap,
						final    : finalMap,
						oblivion : oblivionMap,
					},
				)

			# Apply multiplier.
			fsm3 *= (thisMult.multiplier.min, thisMult.multiplier.max)

			fsm2 += fsm3
		fsm1 |= fsm2
	return fsm1

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
					mult(charclass("3"), 2, 2),
				},
				"A" : {
					charclass("1", "4"),
				},
				"B" : {
					charclass("1"),
					conc(
						mult(charclass("7"), 1, 1),
						mult(charclass("5"), 1, 1),
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

			self.__addTransition(left, symbol)

		# initialState alone can be reached via an empty string ;)
		if fsm.initialState in right:
			self.__addTransition(None, emptystring)

	def __addTransition(self, left, element):
		if not isinstance(element, lego):
			raise Exception(str(element) + " is not a lego piece")
		if left not in self.lefts:
			self.lefts[left] = set()

		# Can't put a pattern in a set of what are basically alternate
		# possibilities.
		if type(element) is pattern:
			self.lefts[left].update(element.concs)

		else:
			self.lefts[left].add(element)

	# get a regex for all the possible transitions FROM the supplied term on
	# the left TO the term on the right.
	def getTransition(self, left):
		if left not in self.lefts:
			raise Exception("This state-set has no such parent")

		lefts = list(self.lefts[left])

		if len(lefts) == 0:
			raise Exception("Why does this exist if it's empty?")

		# charclass -> mult -> conc so we have a list of concs
		for i in range(len(lefts)):
			if type(lefts[i]) is charclass:
				lefts[i] = mult(lefts[i], one)

			if type(lefts[i]) is mult:
				lefts[i] = conc(lefts[i])

		return pattern(*lefts)

	# remove the self-transition from an equation
	# e.g. "A0 | B1 | C2 = A" becomes "B10* | C10* = A"
	def applyLoops(self):
		if self.right not in self.lefts:
			return
		
		loop = self.getTransition(self.right) * star
		del self.lefts[self.right]
		
		for left in self.lefts:
			transition = self.getTransition(left)
			transition = transition + loop
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

			# self-transition? skip
			if otherLeft == other.right:
				raise Exception("Did you forget applyLoops()?")

			# any transition from otherLeft to otherRight, coupled with
			# the universal transition from otherRight to *here*, counts as
			# as transition from otherLeft to here.
			otherTransition = other.getTransition(otherLeft) + self.getTransition(other.right)

			self.__addTransition(otherLeft, otherTransition)

		del self.lefts[other.right]

	def __repr__(self):
		string = ""
		string += "lefts:\n"
		for left in self.lefts:
			string += " " + str(left) + ": " + \
			str(self.getTransition(left)) + "\n"
		string += "right: " + str(self.right) + "\n"
		string += "\n"
		return string

def regexbuild(fsm):
	'''Turn any FSM back into a regex. We start at a state-set
	comprised of all the possible final states... then we find all the
	possible routes to that final state-set.'''

	# iterate over a growing list, generating equations
	equations = [equation(fsm.finalStates, fsm)]
	i = 0;
	while i < len(equations):

		# record newly-found state-sets for future reference (no dupes)
		for right in equations[i].lefts:

			if right is not None \
			and right not in [e.right for e in equations]:
				equations.append(equation(right, fsm))

		i += 1

	# Next, we start at the end of our list, and fill backwards
	# to show all possible routes.
	for i in reversed(range(len(equations))):
		equations[i].applyLoops()
		for j in reversed(range(i)):
			equations[j].eliminate(equations[i])

	# only "None" (static transitions) should be left after the back-
	# substitution is completed. Otherwise,
	# this is a problem and means the algorithm failed :-S
	if len(equations[0].lefts) > 1:
		raise Exception("Can't get regex! Not done yet!")

	# by this point all back-substitutions have been performed and the final
	# element in equations[] should be ready to convert into a regex
	try:
		return equations[0].getTransition(None).regex()

	# if no such transition exists, or it is empty, then an exception arises
	# since there are no static strings leading to the final state-set,
	# i.e. there is no regex.
	except:
		return None

if __name__ == '__main__':

	# unit tests
	regexbuild(
		fsm(
			alphabet     = {charclass("0"), charclass("123456789")},
			states       = {0, 1, 2, 3},
			initialState = 3,
			finalStates  = {1},
			map          = {
				0: {charclass("0"): 1, charclass("123456789"): 1},
				1: {charclass("0"): 2, charclass("123456789"): 2},
				2: {charclass("0"): 2, charclass("123456789"): 2},
				3: {charclass("0"): 0, charclass("123456789"): 2},
			}
		)
	)

	d2 = fsmbuild(
		pattern(conc(mult(charclass({charclass("0123456789")}), multiplier(2, 2)))),
		{charclass("0123456789"), charclass("0123456789", negateMe=True)}
	)
	assert not d2.accepts([])
	assert not d2.accepts([charclass("0123456789")])
	assert d2.accepts([charclass("0123456789"),charclass("0123456789")])
	assert not d2.accepts([charclass("0123456789"),charclass("0123456789"),charclass("0123456789")])
	print("OK")

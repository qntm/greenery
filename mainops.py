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

from lego import charclass, mult, conc, pattern
from fsm import fsm, epsilon
from legoops import legoconcatenate, legoalternate, legomultiply
from fsmops import fsmmultiply, fsmconcatenate, fsmalternate

def fsmbuild(thisPattern):
    """Given a pattern (nested data structure representation of a regex),
    return a finite state machine which will recognise any string acceptable
    by the original regex."""

    fsms = []
    for thisConc in thisPattern:

        # start with a component accepting only the empty string
        fsms2 = [epsilon]
        for thisMult in thisConc:

            multiplicand, min, max = thisMult

            # subpattern. recurse.
            if type(multiplicand) is pattern:
                fsm2 = fsmbuild(multiplicand)

            # otherwise, assume it is a set of characters. Make a singular
            # FSM accepting only these characters
            else:
                fsm2 = fsm(
                    alphabet = set(multiplicand),
                    states = {"A", "B"},
                    initialState = "A",
                    finalStates = {"B"},
                    map = {
                        "A" : dict([
                            (symbol, "B") for symbol in multiplicand
                        ]),
                        "B" : {},
                    },
                )

            # Apply multiplier.
            fsm2 = fsmmultiply(fsm2, min, max)

            fsms2.append(fsm2)
        fsms2 = fsmconcatenate(fsms2)

        fsms.append(fsms2)
    fsms = fsmalternate(fsms)

    return fsms

class equation:
    """
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
                conc(()),
                charclass({"0"}),
                mult(charclass({"3"}), 2, 2),
            },
            "A" : {
                charclass({"1", "4"}),
            },
            "B" : {
                charclass({"1"}),
                conc((
                    mult(charclass({"7"}), 1, 1),
                    mult(charclass({"5"}), 1, 1),
                )),
            ),
            "C" : {
                charclass({"8"}),
            ),
        },
        "right" : "A",
    }

    Notice how lefts is a dict of sets of lego bits. These
    can be freely combined using legoconcatenate(), legoalternate() and
    legomultiply() in the lego module.
    """

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

        for symbol in sorted(fsm.getAlphabet()):

            # find every possible way to reach the current state set
            # using this symbol
            left = set()
            for state in right:
                left.update(fsm.getPrevious(state, symbol))
            left = frozenset(left)

            # ignore unreachables
            if left == frozenset():
                continue

            self.__addTransition(left, symbol)

        # initialState alone can be reached via an empty string ;)
        if fsm.getInitialState() in right:
            self.__addTransition(None, conc(()))

    def __addTransition(self, left, element):
        if left not in self.getLefts():
            self.lefts[left] = set()

        # Can't put a pattern in a set of what are basically alternate
        # possibilities. legoalternate also can't handle full patterns anyway
        if type(element) is pattern:
            self.lefts[left].update(element)

        else:
            self.lefts[left].add(element)

    def __deleteLeft(self, left):
        del self.lefts[left]

    def getRight(self):
        return self.right

    # just return a list of left state-sets, not the transitions
    def getLefts(self):
        return self.lefts.keys()

    # get a regex for all the possible transitions FROM the supplied term on
    # the left TO the term on the right.
    def getTransition(self, left):
        if left not in self.getLefts():
            raise Exception("This state-set has no such parent")
        if len(self.lefts[left]) == 0:
            raise Exception("Why does this exist if it's empty?")
        return legoalternate(self.lefts[left])

    # remove the self-transition from an equation
    # e.g. "A0 | B1 | C2 = A" becomes "B10* | C10* = A"
    def applyLoops(self):
        right = self.getRight()
        if right not in self.getLefts():
            return
        
        loop = legomultiply(self.getTransition(right), 0, None)
        self.__deleteLeft(right)
        
        for left in self.getLefts():
            transition = self.getTransition(left)
            transition = legoconcatenate(transition, loop)
            self.__deleteLeft(left)
            self.__addTransition(left, transition)

    # take the equation of some other state-set and substitute it into
    # this equation, cancelling out any references to the other.
    def eliminate(self, other):

        otherRight = other.getRight()

        # No transition from other to self? Then no substitution is required.
        if otherRight not in self.getLefts():
            return

        # Now how about dynamic routes here
        for otherLeft in other.getLefts():

            # self-transition? skip
            if otherLeft == otherRight:
                raise Exception("Did you forget applyLoops()?")

            # any transition from otherLeft to otherRight, coupled with
            # the universal transition from otherRight to *here*, counts as
            # as transition from otherLeft to here.
            otherTransition = legoconcatenate(
                other.getTransition(otherLeft),
                self.getTransition(otherRight)
            )

            self.__addTransition(otherLeft, otherTransition)

        self.__deleteLeft(otherRight)

    def getString(self):
        string = ""
        string += "lefts:\n"
        for left in self.getLefts():
            string += " " + str(left) + ": " + \
            self.getTransition(left).getString() + "\n"
        string += "right: " + str(self.getRight()) + "\n"
        string += "\n"
        return string

def regexbuild(fsm):
    """Turn any FSM back into a regex. We start at a state-set
    comprised of all the possible final states... then we find all the
    possible routes to that final state-set."""

    # iterate over a growing list, generating equations
    equations = [equation(fsm.getFinalStates(), fsm)]
    i = 0;
    while i < len(equations):

        # record newly-found state-sets for future reference (no dupes)
        for right in equations[i].getLefts():

            if right is not None and \
            right not in [e.getRight() for e in equations]:
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
    if len(equations[0].getLefts()) > 1:
        raise Exception("Can't get regex! Not done yet!")

    # by this point all back-substitutions have been performed and the final
    # element in equations[] should be ready to convert into a regex
    try:
        return equations[0].getTransition(None).getString()

    # if no such transition exists, or it is empty, then an exception arises
    # since there are no static strings leading to the final state-set,
    # i.e. there is no regex.
    except:
        return None

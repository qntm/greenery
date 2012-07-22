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

# http://qntm.org/fsm
# http://qntm.org/greenery

class fsm:
    """An FSM has a set of states and a starting state. It also has an alphabet,
    and final states, and a map with states and symbols and results."""
    def __init__(self, alphabet, states, initialState, finalStates, map):
        self.__setAlphabet(alphabet)
        self.__setStates(states)
        self.__setInitialState(initialState)
        self.__setFinalStates(finalStates)
        self.__setMap(map)

    # alphabet must be iterable. An alphabet MAY be empty! If you want no
    # transitions, that is.
    def __setAlphabet(self, alphabet):
        if type(alphabet) != set:
            raise Exception("Alphabet is not a set!")
        self.__alphabet = alphabet

    # states must form a set
    def __setStates(self, states):
        if type(states) != set:
            raise Exception("States do not form a set!")
        self.__states = states

    # initial state must be a real state
    def __setInitialState(self, initialState):
        if initialState not in self.getStates():
            raise Exception("Initial state is not among states!")
        self.__initialState = initialState

    # final states must be a subset of the states
    def __setFinalStates(self, finalStates):
        if type(finalStates) != set:
            raise Exception("Final states don't form a set!")
        if not finalStates.issubset(self.getStates()):
            raise Exception("Final states are outside this machine!")
        self.__finalStates = finalStates

    # map must be a dict containing every state
    def __setMap(self, f):
        if type(f) != dict:
            raise Exception("Map is not a dictionary!")
        if not set(f.keys()) == self.getStates():
            raise Exception("Map has wrong states!")
        self.__map = dict()
        for (state, stateMap) in f.items():
            self.__map[state] = self.__buildStateMap(stateMap)

    # each stateMap must be a dict containing 0 or more symbols
    def __buildStateMap(self, f):
        if type(f) != dict:
            raise Exception("Map for this state is not a dictionary!")
        if not set(f.keys()).issubset(self.getAlphabet()):
            raise Exception("Map for this state has an extra symbol!")
        stateMap = dict()
        for (symbol, result) in f.items():
            stateMap[symbol] = self.__buildTransition(result)
        return stateMap

    # each transition in an FSM must result in a single valid state
    def __buildTransition(self, result):
        if result not in self.getStates():
            raise Exception("Map maps to nonexistent state!")
        return result

    def getAlphabet(self):
        return self.__alphabet

    def getStates(self):
        return self.__states

    def getInitialState(self):
        return self.__initialState

    def getFinalStates(self):
        return self.__finalStates

    def getMap(self):
        return self.__map

    def getStateMap(self, state):
        return self.getMap()[state]

    # "OBLIVION" may be omitted from the FSMs for brevity. A symbol resulting
    # in oblivion raises an exception
    def getNext(self, state, symbol):
        stateMap = self.getStateMap(state)
        if symbol not in stateMap:
            raise Exception("You just fell out of this FSM!")
        return stateMap[symbol]

    # returns a frozenset of all states which lead to this state on this
    # symbol (could be empty)
    # basically inverse of the transition function
    def getPrevious(self, state, symbol):
        previousStates = set()
        for thisState in self.getStates():
            stateMap = self.getStateMap(thisState)
            if symbol not in stateMap:
                continue
            if stateMap[symbol] != state:
                continue
            previousStates.add(thisState)
        return frozenset(previousStates)

    # this routine gets called whenever you print() a FSM
    def __repr__(self):
        width = max([len(str(symbol)) for symbol in self.getAlphabet()]) + 1
        string = ""
        string += " name isFinal "
        for symbol in sorted(self.getAlphabet()):
            string += str(symbol).ljust(width)
        string += "\n"
        string += "--------------"
        for symbol in sorted(self.getAlphabet()):
            string += "-" * width
        string += "\n"
        for state in sorted(self.getStates()):
            if state == self.getInitialState():
                string += "*"
            else:
                string += " "
            string += str(state).ljust(5)
            if state in self.getFinalStates():
                string += "True".ljust(8)
            else:
                string += "False".ljust(8)
            stateMap = self.getMap()[state]
            for symbol in sorted(self.getAlphabet()):
                if symbol in stateMap:
                    string += str(stateMap.get(symbol)).ljust(width)
                else:
                    string += " " * width
            string += "\n"
        return string

# An FSM accepting nothing (not even the empty string). This is
# not very useful, but demonstrates that this is possible.
null = fsm(
    alphabet = set(),
    states = {0},
    initialState = 0,
    finalStates = set(),
    map = {
        0: {},
    },
)

# an FSM matching an empty string, "", only
epsilon = fsm(
    alphabet = set(),
    states = {0},
    initialState = 0,
    finalStates = {0},
    map = {
        0: {},
    },
)
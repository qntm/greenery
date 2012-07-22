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

from fsm import fsm, epsilon

# TODO: _crawl should strip states which cannot reach final states
def _crawl(newAlphabet, oblivionState, newInitialState, isFinal, getNext):
    """Given the above conditions and instructions, crawl a new unknown FSM,
    mapping its states, final states and transitions. Return the new one."""

    def pruneDupes():
        """often, a finite state machine will contain several states which
        behave exactly identically. i.e. they have the same map and the
        same finality. _crawl strips out these duplicate states and replaces
        each with the "original", considered to be the first/shallowest such
        state"""

        nonlocal runningList
        nonlocal newFinalStates
        nonlocal newMap
        nonlocal equivalents

        # iterate over all states which have been seen.
        for i, thisState in enumerate(runningList):

            if thisState not in newMap.keys():
                continue

            thisStateMap = newMap[thisState]
            thisIsFinal = thisState in newFinalStates

            # now iterate over all the deeper states to
            # find all the states identical to thisState
            for j, thatState in enumerate(runningList[i + 1:], i + 1):

                if equivalents[thatState] == thisState:
                    continue

                thatStateMap = newMap[equivalents[thatState]]
                thatIsFinal = equivalents[thatState] in newFinalStates

                if (thisStateMap, thisIsFinal) == (thatStateMap, thatIsFinal):
                    equivalents[thatState] = thisState

                    del newMap[thatState]

                    # remove thatState from newFinalStates
                    newFinalStates.discard(thatState)

                    for otherState in runningList[j + 1:]:
                        if equivalents[otherState] == thatState:
                            equivalents[otherState] = thisState

        # reorient all pointers (this recursion is required sometimes, for
        # example see the mergeMe FSM)
        checkAgain = False
        for (state, stateMap) in newMap.items():
            for (symbol, result) in stateMap.items():
                if equivalents[result] != result:
                    newMap[state][symbol] = equivalents[result]
                    checkAgain = True

        if checkAgain:
            pruneDupes()

    # newAlphabet is known
    # newInitialState is known
    newFinalStates = set()
    newMap = {}
    runningList = [newInitialState]
    equivalents = {
        newInitialState : newInitialState,
    }

    # iterate over a growing list
    i = 0
    while i < len(runningList):
        thisState = runningList[i]

        # add to finalStates
        if isFinal(thisState):
            newFinalStates.add(thisState)

        # add to newMap
        thisStateMap = {}
        for symbol in sorted(newAlphabet):
            nextState = getNext(thisState, symbol)

            # by convention, oblivion is omitted from maps and FSMs
            if nextState == oblivionState:
                continue

            # runningList is used to ensure that we encounter no dupes
            # (and also pass through the whole list in good time)
            if nextState in runningList:

                # any state in runningList also has an entry in equivalents.
                # if it has an entry in equivalents then it might actually be
                # a duplicate of some other state. We don't want to propagate
                # that problem, so point this map towards the original, instead
                # of the duplicate
                nextState = equivalents[nextState]

            else:

                # otherwise this is the first time this state has been referred
                # to and for all we know it is itself alone. We will find out
                # if this is not the case in due course.
                runningList.append(nextState)
                equivalents[nextState] = nextState

            thisStateMap[symbol] = nextState
        newMap[thisState] = thisStateMap

        i += 1

    # now we have a map for every state, prune duplicates
    pruneDupes()

    # use list indices as state names

    # substitute new letters for older, more verbose state names
    newStates = set([
        runningList.index(state) for state in equivalents.values()
    ])
    newInitialState = runningList.index(newInitialState)
    newFinalStates = set([
        runningList.index(state) for state in newFinalStates
    ])
    newMap = dict([
        (
            runningList.index(state),
            dict([
                (
                    symbol,
                    runningList.index(result),
                ) for (symbol, result) in stateMap.items()
            ]),
        ) for (state, stateMap) in newMap.items()
    ])

    return fsm(
        alphabet = newAlphabet,
        states = newStates,
        initialState = newInitialState,
        finalStates = newFinalStates,
        map = newMap,
    )

def fsmconcatenate(fsms):
    """Take finite state machines. Concatenate them.
    For example, if the first finite state machine accepts "0*"
    and the other accepts "1+(0|1)", will return a finite state
    machine accepting "0*1+(0|1)".
    """

    if len(fsms) < 1:
        raise Exception("Can't concatenate no FSMs!")

    # alphabet: just unify them all
    newAlphabet = set()
    for fsm in fsms:
        newAlphabet.update(fsm.getAlphabet())

    # the oblivion state signals that we've fallen out of the new FSM somehow
    oblivionState = frozenset()

    newInitialState = frozenset([(0, fsms[0].getInitialState())])

    def isFinal(currentState):
        nonlocal fsms

        for (fsmId, state) in currentState:

            thisFsm = fsms[fsmId]

            # a state cannot be final in the combined FSM if it isn't final
            # in its "home" FSM
            if state not in thisFsm.getFinalStates():
                continue

            # the last FSM in the chain:
            if fsmId == len(fsms)-1:
                # every final state of the last FSM is
                # a final state of the combined FSM
                return True

            # not the last FSM in the chain: COULD still be final.
            # The real question is about
            # the INITIAL state of the NEXT machine
            nextFsmId = fsmId + 1
            nextFsm = fsms[nextFsmId]
            nextInitialState = nextFsm.getInitialState()
            if not isFinal(
                frozenset([
                    (nextFsmId, nextInitialState)
                ])
            ):
                continue
            return True

        return False

    # dedicated function accepts a "superset" and returns the next "superset"
    # obtained by following this transition in the new FSM
    def getNext(currentState, symbol):
        nonlocal fsms

        nextStates = set()

        for (fsmId, state) in currentState:

            thisFsmId = fsmId
            thisFsm = fsms[thisFsmId]
            thisState = state

            try:
                nextState = thisFsm.getNext(thisState, symbol)
                nextStates.add((thisFsmId, nextState))
            except:
                pass

            # final state of a nonfinal machine?
            # then merge with initial state of next one
            while thisFsmId < len(fsms)-1 \
            and thisState in thisFsm.getFinalStates():
                thisFsmId += 1
                thisFsm = fsms[thisFsmId]
                thisState = thisFsm.getInitialState()

                try:
                    nextState = thisFsm.getNext(thisState, symbol)
                    nextStates.add((thisFsmId, nextState))
                except:
                    pass

        return frozenset(nextStates)

    return _crawl(
        newAlphabet,
        oblivionState,
        newInitialState,
        isFinal,
        getNext,
    )

def fsmintersect(fsms):
    """Take FSMs and AND them together. That is, return an FSM which
    accepts any sequence of symbols that is accepted by all of the original
    FSMs.
    
    The superstate is (state in A, state in B, ...). If any FSM falls into
    oblivion then so does the resulting FSM.
    """

    if len(fsms) < 1:
        raise Exception("Can't intersect no FSMs!")

    # alphabet: just unify them all
    newAlphabet = set()
    for fsm in fsms:
        newAlphabet.update(fsm.getAlphabet())

    oblivionState = None

    newInitialState = tuple([fsm.getInitialState() for fsm in fsms])

    # dedicated function accepts a "superset" and returns the next "superset"
    # obtained by following this transition in the new FSM
    def getNext(currentState, symbol):
        nonlocal fsms

        nextStates = []

        # follow transition in every individual FSM
        for fsmId, thisFsm in enumerate(fsms):

            thisState = currentState[fsmId]

            try:
                nextState = thisFsm.getNext(thisState, symbol)
            except:
                return oblivionState

            nextStates.append(nextState)

        return tuple(nextStates)

    def isFinal(currentState):
        nonlocal fsms

        for fsmId, thisFsm in enumerate(fsms):

            thisState = currentState[fsmId]

            if thisState not in thisFsm.getFinalStates():
                return False

        return True

    return _crawl(
        newAlphabet,
        oblivionState,
        newInitialState,
        isFinal,
        getNext
    )

def fsmalternate(fsms):
    """Take list of FSMs and OR them together. That is, return a finite state
    machine which accepts any sequence of symbols that is accepted by
    any of the original FSMs.

    The "superstates"/"statesets" used to make the new FSM has states of the
    form "frozenset([
        (fsmId, state in that FSM that you would be in right now),
        (fsmId, state in that FSM that you would be in right now),
        ...
    ])"
    
    If you drop out of one FSM entirely then the frozenset will consist
    only of states from the other FSM. Drop out of both and naturally you are
    in oblivion.
    """

    if len(fsms) < 1:
        raise Exception("Can't alternate no FSMs!")

    # alphabet: just unify them all
    newAlphabet = set()
    for fsm in fsms:
        newAlphabet.update(fsm.getAlphabet())

    oblivionState = frozenset()

    newInitialState = frozenset([
        (fsmId, thisFsm.getInitialState()) for fsmId, thisFsm in enumerate(fsms)
    ])

    # dedicated function accepts a "superset" and returns the next "superset"
    # obtained by following this transition in the new FSM
    def getNext(currentState, symbol):
        nonlocal fsms

        nextStates = set()

        for (fsmId, state) in currentState:
            thisFsm = fsms[fsmId]

            # find the next state for this machine (if we're still in it!)
            try:
                nextState = thisFsm.getNext(state, symbol)
                nextStates.add((fsmId, nextState))
            except:
                pass

        return frozenset(nextStates)

    # test to see if currentState is final
    def isFinal(currentState):
        nonlocal fsms

        for (fsmId, state) in currentState:
            thisFsm = fsms[fsmId]

            if state in thisFsm.getFinalStates():
                return True

        return False

    return _crawl(
        newAlphabet,
        oblivionState,
        newInitialState,
        isFinal,
        getNext
    )

def fsmstar(fsm):
    '''Take an FSM accepting X and returns an FSM accepting X* (i.e. 0 or
    more Xes).

    This is NOT as simple as naively connecting the final states back to the
    initial state: see (b*ab)* for example.
    
    Instead we must create an articial "omega state" which is our only accepting
    state and which dives into the FSM and from which all exits return.
    '''

    newAlphabet = fsm.getAlphabet()
    
    omegaState = "OMEGA"

    # TODO: autogenerate a unique name, oh well
    if omegaState in fsm.getStates():
        raise Exception("Can't add new state '" + omegaState + "' to FSM")
    
    newInitialState = frozenset([omegaState])
    oblivionState = frozenset()

    def getNext(currentState, symbol):
        nonlocal fsm
        nonlocal omegaState

        nextState = set()

        for state in currentState:

            # the special new starting "omegaState" behaves exactly like the
            # original starting state did
            if state == omegaState:
                state = fsm.getInitialState()

            try:
                nextAState = fsm.getNext(state, symbol)
            except:
                continue
            nextState.add(nextAState)

            # loop back to beginning
            if nextAState in fsm.getFinalStates():
                nextState.add(omegaState)

        return frozenset(nextState)

    def isFinal(currentState):
        nonlocal fsm
        nonlocal omegaState

        # final if currentState contains omegaState
        if omegaState in currentState:
            return True
        return False

    return _crawl(
        newAlphabet,
        oblivionState,
        newInitialState,
        isFinal,
        getNext,
    )

def fsmmultiply(input, min, max):
    """Given an FSM and a multiplier, return the multiplied FSM."""

    # worked example: (min, max) = (5, 7) or (5, None)

    output = [epsilon]
    # accepts ""

    for i in range(min):
        output.append(input)
    # now accepts e.g. "ababababab"

    # unlimited additional copies
    if max is None:
        output.append(fsmstar(input))
        # now accepts e.g. "ababababab(ab)*" = "(ab){5,}"

    # finite additional copies
    else:
        q = fsmalternate([epsilon, input])
        # accepts "(ab)?"

        for i in range(min, max):
            output.append(q)
        # now accepts e.g. "ababababab(ab)?(ab)?" = "(ab){5,7}"

    return fsmconcatenate(output)

if __name__ == "__main__":
    # this is "0*1" in heavy disguise. _crawl should resolve this duplication
    # Notice how states 2 and 3 behave identically. When resolved together,
    # states 1 and 2&3 also behave identically, so they, too should be resolved
    # (this is impossible to spot before 2 and 3 have been combined).
    mergeMe = fsm(
        alphabet = {"0", "1"},
        states = {1, 2, 3, 4},
        initialState = 1,
        finalStates = {4},
        map = {
            1 : {"0" : 2, "1" : 4},
            2 : {"0" : 3, "1" : 4},
            3 : {"0" : 3, "1" : 4},
            4 : {},
        }
    )
    print(mergeMe)

    mergeMe = fsmconcatenate([mergeMe])
    print(mergeMe)
    assert len(mergeMe.getStates()) == 2
        
    # this is a*ba
    starMe = fsm(
        alphabet = {"a", "b"},
        states = {0, 1, 2},
        initialState = 0,
        finalStates = {2},
        map = {
            0 : {"a" : 0, "b" : 1},
            1 : {"a" : 2},
            2 : {},
        }
    )
    print(starMe)
    
    starMe = fsmstar(starMe)
    print(starMe)
    assert starMe.getAlphabet() == frozenset(["a", "b"])
    assert starMe.getStates() == frozenset([0, 1, 2])
    assert starMe.getInitialState() == 0
    assert starMe.getFinalStates() == {0}
    assert starMe.getMap() == {
        0 : {"a" : 1, "b" : 2},
        1 : {"a" : 1, "b" : 2},
        2 : {"a" : 0},
    }
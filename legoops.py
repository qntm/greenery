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

# http://qntm.org/lego
# http://qntm.org/greenery

from lego import charclass, mult, conc, pattern
from lego import unescapes, allSpecial, classSpecial, allowableRanges
from lego import w, W, d, D, s, S

def classnegate(A):
    """Replace a charclass with its negation"""

    return charclass(A, negateMe=not A.negated)

def classunion(A, B):
    """Find the classunion of charclasses, returning a charclass."""

    # two regular classes unify in the regular sense.
    if not A.negated and not B.negated:
        result = charclass(A.union(B))

    # classunioning a charclass and a negated one is a little more complex
    # A OR ¬B = ¬(B - A)
    elif not A.negated and B.negated:
        result = charclass(B.difference(A), negateMe=True)

    # ¬A OR B = ¬(A - B)
    elif A.negated and not B.negated:
        result = charclass(A.difference(B), negateMe=True)

    # ¬A OR ¬B = ¬(A AND B)
    elif A.negated and B.negated:
        result = charclass(A.intersection(B), negateMe=True)

    else:
        raise Exception("Invalid types!")

    return result

def classdifference(A, B):
    """Subtract others from self"""

    # A - B = A - B
    if not A.negated and not B.negated:
        result = charclass(A.difference(B))

    # A - ¬B = A AND B
    elif not A.negated and B.negated:
        result = charclass(A.intersection(B))

    # ¬A - B = ¬(A OR B)
    elif A.negated and not B.negated:
        result = charclass(A.union(B), negateMe=True)

    # ¬A - ¬B = B - A
    elif A.negated and B.negated:
        result = charclass(B.difference(A))

    return result

def classintersection(A, B):
    """Find the classintersection of two charclasses, returning a charclass."""

    # two regular classes intersect in the regular sense.
    if not A.negated and not B.negated:
        result = charclass(A.intersection(B))

    # other combinations are more complex
    # A AND ¬B = A - B
    elif not A.negated and B.negated:
        result = charclass(A.difference(B))

    # ¬A AND B = B - A
    elif A.negated and not B.negated:
        result = charclass(B.difference(A))

    # ¬A AND ¬B = ¬(A OR B)
    elif A.negated and B.negated:
        result = charclass(A.union(B), negateMe=True)

    else:
        raise Exception("Invalid types!")

    return result

def classissubset(A, B):
    """Find out if A is a subset of B"""

    # A < B
    if not A.negated and not B.negated:
        result = A.issubset(B)

    # A < ¬B if A n B = 0
    elif not A.negated and B.negated:
        result = A.isdisjoint(B)

    # ¬A < B is impossible
    elif A.negated and not B.negated:
        result = False

    # ¬A < ¬B if B < A
    elif A.negated and B.negated:
        result = B.issubset(A)

    else:
        raise Exception("Whaaa")

    return result

def legoconcatenate(A, B):
    """Magical function for the concatenation of any two pieces of lego. All
    calls are redirected into the specific (conc, conc) case and then reduced
    afterwards if possible."""
 
    # charclass -> mult
    if type(A) is charclass:
        A = mult((A, 1, 1))
    if type(B) is charclass:
        B = mult((B, 1, 1))

    # mult -> conc
    if type(A) is mult:
        A = conc((A,))
    if type(B) is mult:
        B = conc((B,))

    # pattern -> conc
    if type(A) is pattern:
        A = _concsuffix(A)
    if type(B) is pattern:
        B = _concprefix(B)

    newA = list(A)
    newB = list(B)

    if len(newA) == len(newB) == 0:
        result = conc(())
    elif len(newA) == 0:
        result = conc(newB)
    elif len(newB) == 0:
        result = conc(newA)
    else:

        # get the last mult in A, first mult in B
        newA0 = newA.pop()
        newB0 = newB.pop(0)

        (multiplicandA, minA, maxA) = newA0

        (multiplicandB, minB, maxB) = newB0

        # if the multiplicands in each mult are identical, then
        # we can just add the multiplicands
        if multiplicandA == multiplicandB:
            multiplicandC = multiplicandA
            minC = minA + minB
            if maxA is None or maxB is None:
                maxC = None
            else:
                maxC = maxA + maxB
            newC0 = mult((multiplicandC, minC, maxC))
            newC = [newC0]
        else:
            newC = [newA0, newB0]

        # now newC is a list just like newA and newB

        # concatenate the lists
        result = conc(newA + newC + newB)

    result = _legoreduce(result)
    return result

def legoalternate(elements):

    elements = list(elements)

    if len(elements) == 0:
        raise Exception("Can't alternate no elements!")

    # no point in clever shenanigans if there's only one thing
    if len(elements) == 1:
        result = elements[0]

    # if the empty string is a possibility then we need to reserve that
    # for future reference.
    # this will leave at least one element behind though
    else:
        #print("Alternating:")
        #for element in elements:
            #print("", element)

        if conc(()) in elements:
            qm = True
            elements.remove(conc(()))
        else:
            qm = False

        # charclass -> mult -> conc so we have a list of concs
        for i, element in enumerate(elements):
            if type(element) is charclass:
                element = mult((element, 1, 1))

            if type(element) is mult:
                element = conc((element,))

            elements[i] = list(element)

        newA = _concprefix(elements)
        # newA is now a list of mults, the last of which is *possibly* a pattern
        # containing everything that could not be split up into a prefix.

        if len(newA) > 0:
            # examine the final element of newA.
            newB = newA.pop()

            # if it's a subpattern, we can now also take a common suffix
            multiplierB, minB, maxB = newB
            if type(multiplierB) is pattern and minB == maxB == 1:
                newA.extend(_concsuffix(multiplierB))

            # otherwise no dice
            else:
                newA.append(newB)

        result = conc(())
        while len(newA) > 0:
            result = legoconcatenate(result, newA.pop(0))

        if qm is True:
            result = legomultiply(result, 0, 1)

        result = _legoreduce(result)

    return result

def legomultiply(A, minB, maxB):
    newA = _legoreduce(A)

    if minB == maxB == 1:
        return newA

    # compound multiplication! woooo
    if type(newA) is mult:
        (multiplicandA, minA, maxA) = newA
        minA *= minB
        if maxA is None or maxB is None:
            maxA = None
        else:
            maxA *= maxB
        return mult((multiplicandA, minA, maxA))

    # conc -> pattern
    if type(newA) is conc:
        newA = pattern([newA])

    # pattern -> mult
    if type(newA) in {charclass, pattern}:
        return mult((newA, minB, maxB))

    raise Exception("Type unknown")

def _legoreduce(A):
    """Given a possibly unnecessarily complicated lego brick, reduce it."""

    newA = A

    # no point alternating among one possibility
    if type(newA) is pattern and len(newA) == 1:
        # pattern -> conc
        newA = [e for e in newA][0]

    # no point concatenating one thing (note: concatenating nothing is
    # entirely valid)
    if type(newA) is conc and len(newA) == 1:
        # conc -> mult
        newA = newA[0]

    # no point multiplying in the singular
    if type(newA) is mult:
        multiplicandA, minA, maxA = newA
        if minA == maxA == 1:
            # mult -> pattern or charclass
            newA = multiplicandA

    # charclasses are maximally reduced already

    # if a reduction occurred, possibly further reduction would be fruitful
    if newA != A:
        newA = _legoreduce(newA)

    return newA

def _concprefix(oldConcList):
    """
    Compare a set of concs, to find the longest possible common prefix, if any.
    Accepts: a list of concs.
    Returns:
        A list of mults.
        The first mults will form the common prefix if there was one.
        After that the final mult will contain everything left over once the
        common prefix has been truncated from the front of each conc. This will
        probably take the form of a pattern. In the event that nothing is left
        after this truncation, an empty conc is returned.
    """

    if len(oldConcList) == 0:
        return []

    multLists = [list(oldConc) for oldConc in oldConcList]

    result = list()
    while min(len(multList) for multList in multLists) > 0:

        # compare vertically, all the mults at the (moving) start of the list
        firstMults = [multList[0] for multList in multLists]
        (commonPrefix, leftovers) = _commonmultpart(firstMults)

        # no common prefix, we're done.
        if commonPrefix is None:
            break

        # success! log that common prefix
        result.append(commonPrefix)

        # replace each leading mult with what was left over after
        # the common prefix was removed
        for j in range(len(multLists)):
            multLists[j][0] = leftovers[j]

        # so, have we COMPLETELY removed that leading mult from EVERY conc?
        allGone = True
        for multList in multLists:
            if multList[0] is not None:
                allGone = False

        # (remove those leading Nones, they are not required)
        for j in range(len(multLists)):
            if multLists[j][0] is None:
                multLists[j].pop(0)

        if allGone is False:
            break

    # now what is left of newConcs is going to form the pattern which is left
    # over.
    newConcList = [conc(multList) for multList in multLists]

    newConcList = _leftover(newConcList)
    if newConcList is not None:
        result.append(newConcList)

    return result

def _concsuffix(concs):
    """
    Compare a set of concs, to find the longest possible common suffix, if any.
    Accepts: a list of concs.
    Returns:
        A list of mults.
        The last mults will form the common suffix if there was one.
        Before that the first mult will contain a pattern, itself containing
        all the concs which are left over once the
        common prefix has been truncated from the end of each conc. In the
        event that nothing is left after this truncation, an empty conc is
        returned.
    """

    if len(concs) == 0:
        return []

    newConcs = list(list(thisConc) for thisConc in concs)

    result = list()
    while min(len(thisConc) for thisConc in newConcs) > 0:

        # compare the mults at position i in the list.
        mults = [thisConc[-1] for thisConc in newConcs]

        # otherwise, there is still an outside possibility that some subset
        # of the mults have a common feature
        (suffix, newMults) = _commonmultpart(mults) # returns a mult or None

        # failure
        if suffix is None:
            break

        # success!
        continueFlag = True
        result.insert(0, suffix)
        for j in range(len(mults)):
            if newMults[j] is None:
                newConcs[j].pop()
            else:
                # partial success, stop now
                newConcs[j][-1] = newMults[j]
                continueFlag = False

        if continueFlag is False:
            break

    # now what is left of newConcs is going to form the pattern left over.
    newConcs = [conc(thisConc) for thisConc in newConcs]

    lastMult = _leftover(newConcs)
    if lastMult is not None:
        result.insert(0, lastMult)

    return result

def allSingletons(newConcs):
    for newConc in newConcs:
        if len(newConc) != 1:
            return False
    return True

def allCharclasses(newMults):
    for newMult in newMults:
        multiplicand, _, _ = newMult
        if type(multiplicand) is not charclass:
            return False
    return True

def qmIfy(oldMults):
    newMults = []
    for oldMult in oldMults:
        multiplicand, _, max = oldMult
        newMult = mult((multiplicand, 0, max))
        newMults.append(newMult)
    return newMults

def getCommonMultiplier(newMults):
    if len(newMults) == 0:
        raise Exception("Well, this should be impossible")
    _, minA, maxA = newMults[0]
    for newMult in newMults[1:]:
        _, minB, maxB = newMult
        if minB != minA or maxB != maxA:
            raise Exception("These mults don't have a common multiplier")
    return minA, maxA

def unifyMultiplicands(newMults):
    multiplicandA = charclass()
    for newMult in newMults:
        multiplicandB, _, _ = newMult
        multiplicandA = classunion(multiplicandA, multiplicandB)
    return multiplicandA

def _leftover(newConcs):
    """
    This is basically magic.
    Accepts a list of concs. Returns a final mult which alternates between
    all those concs.
    """

    # if there's an empty conc in this list, then the whole pattern is optional.
    # and, um, remove it
    # newConcs may now be empty
    if conc(()) in newConcs:
        qm = True
        newConcs.remove(conc(()))
    else:
        qm = False

    # this can only happen if the empty conc was ALL that was left in the list.
    # so there's nothing left to append
    if len(newConcs) == 0:
        A = None
    
    # so there is at least one conc remaining, and all remaining concs are
    # of length 1 or greater.
    else:

        try:
            # there's a chance that every single conc contains a single mult
            # and each mult has the same multiplier. If so, a major shortcut
            # is possible

            if not allSingletons(newConcs):
                raise Exception("These concs aren't all singletons...")

            newMults = [newConc[0] for newConc in newConcs]

            # if every mult has a charclass at the beginning, it 
            if not allCharclasses(newMults):
                raise Exception("These mults aren't all single charclasses...")

            # append "?"
            if qm:
                newMults = qmIfy(newMults)

            minA, maxA = getCommonMultiplier(newMults)

            multiplicandA = unifyMultiplicands(newMults)

            A = mult((multiplicandA, minA, maxA))

        # gambit has failed! bad luck
        except:
            A = pattern(newConcs)
            if qm:
                A = mult((A, 0, 1))
            else:
                A = mult((A, 1, 1))

    return A

def _commonmultpart(mults):
    """
    Compare a list of mults to find the common part, if any.
    
    Accepts: a list of mults.
    Returns:
        If there is a common part, the first argument is this common part
        in the form of a mult. If there is no common part, the first argument
        is None.
        The second argument is the list of mults which is left over once each
        mult has had the common part removed. If nothing is left after removing
        the common part from a given mult, None is returned in its place.
    """

    newMults = list(mults)

    if len(newMults) == 0:
        raise Exception("What are you playing at!")

    for thisMult in newMults:
        if type(thisMult) is not mult:
            raise Exception("Sorry!")

    # if the multiplicand is not the same in each mult, we have nothing.
    multiplicandC, _, _ = newMults[0]
    for multiplicandB, minB, maxB in newMults:
        if multiplicandB != multiplicandC:
            return None, mults

    # check each mult in turn to establish our lower bound

    # find the bare minimum.
    minC = min(minA for _, minA, _ in newMults)
    maxC = minC

    # strip this lower bound from each mult
    for i in range(len(newMults)):
        multiplicandA, minA, maxA = newMults[i]
        minA -= minC
        if maxA is not None:
            maxA -= minC
        newMults[i] = mult((multiplicandA, minA, maxA))

    # have we removed the ENTIRE definite part from EVERY mult?
    flag = True
    for i in range(len(newMults)):
        _, minA, _ = newMults[i]
        if minA != 0:
            flag = False

    # then we need to consider the optional part as well.
    if flag is True:

        # find the minimal upper bound as well
        j = None
        for i in range(len(newMults)):
            _, _, maxA = newMults[i]
            if maxA is None:
                pass
            else:
                if j is None:
                    j = maxA
                else:
                    j = min(j, maxA)

        # remove this from each A
        for i in range(len(newMults)):
            multiplicandA, minA, maxA = newMults[i]
            if j is None:
                maxA = 0
            else:
                maxA -= j
            newMults[i] = mult((multiplicandA, minA, maxA))

        # add it to C
        if j is None:
            maxC = None
        else:
            maxC += j

    # failure
    if maxC == 0:
        return None, mults

    # eliminate zeroes
    for i in range(len(newMults)):
        _, _, maxA = newMults[i]
        if maxA == 0:
            newMults[i] = None

    C = mult((multiplicandC, minC, maxC))

    # success
    return C, newMults

def legobuild(string):
    """Turn a regex from the form of a string into a pattern object."""

    def appendCharclass(x):
        """Append a charclass to the list of mults. This may contain a single
        element or many."""

        nonlocal stack

        if type(x) is not charclass:
            raise Exception("That's not a charclass!")

        # append to the latest conc in the current pattern
        stack[-1].append(x)
        return

    def applyMultiplier(min, max):
        """Assign a multiplier to the previous charclass or subpattern in this
        pattern."""

        nonlocal stack

        if len(stack[-1]) == 0:
            raise Exception("Can't have a multiplier first thing")

        x = stack[-1].pop()

        # whitelist method
        if type(x) is mult:
            raise Exception("Can't have a multiplier here")

        if min is None:
            raise Exception("Must have SOMETHING")

        stack[-1].append(mult((x, min, max)))

        return

    mode = "outdoors"
    i = 0

    # the stack always has an even number of elements in it.
    stack = []

    # begin pattern, begin conc
    stack.append([])
    stack.append([])

    while i < len(string):
        char = string[i]

        # "outdoors" mode is when we are just looking over regular
        # characters
        if mode == "outdoors":

            # begin pattern, begin conc
            if char == "(":
                stack.append([])
                stack.append([])

            # end conc, end pattern
            elif char == ")":
                if len(stack) < 3:
                    raise Exception("Closed an unopened subpattern")

                thisConc = stack.pop()

                # eliminate raw charclasses and patterns from the listing.
                for j in range(len(thisConc)):
                    if type(thisConc[j]) is not mult:
                        thisConc[j] = mult((thisConc[j], 1, 1))

                stack[-1].append(conc(thisConc))

                thisPattern = stack.pop()
                stack[-1].append(pattern(thisPattern))

            # end conc, begin another conc
            elif char == "|":
                if len(stack) < 2:
                    raise Exception("Whaaat how did that happen")

                thisConc = stack.pop()

                # eliminate raw charclasses and patterns from the listing.
                for j in range(len(thisConc)):
                    if type(thisConc[j]) is not mult:
                        thisConc[j] = mult((thisConc[j], 1, 1))

                stack[-1].append(conc(thisConc))

                stack.append([])

            # a square bracket opens a character class.
            # this event can be skipped (below) if this bracket is escaped
            elif char == "[":

                # the string "[^" indicates the beginning of a negated
                # character class.
                if i < len(string) - 1 and string[i + 1] == "^":
                    i += 1
                    classnegateAfterwards = True

                # otherwise we have a regular character class
                else:
                    classnegateAfterwards = False

                # in either case the logic for dealing with later characters is
                # the same
                pointOne = i + 1
                mode = "class"
                incomplete = charclass()

            # right brackets always have to be escaped even though they're
            # unambiguous
            elif char == "]":
                raise Exception("Can't have a right bracket there!")

            # open an explicit multiplier e.g. "{2,4}" or "{3}"
            elif char == "{":
                mode = "min"
                min = ""
                max = ""

            # right braces always have to be escaped even though they're
            # unambiguous
            elif char == "}":
                raise Exception("Can't have a right brace there!")

            # some other multipliers
            elif char == "?":
                applyMultiplier(0, 1)
            elif char == "*":
                applyMultiplier(0, None)
            elif char == "+":
                applyMultiplier(1, None)

            # backslashes can be used to escape essentially anything
            # in most cases, escaping stuff results in an "I don't know what
            # you just escaped" error
            elif char == "\\":

                # skip the backslash itself and process the next character
                i += 1

                # a backslash at the end of the string is dumb
                if i >= len(string):
                    raise Exception("Backslash at the end of the regex?")

                char = string[i]

                # significant characters which must be escaped to take their
                # literal meanings
                if char in allSpecial:
                    appendCharclass(charclass(char))

                # e.g. "t" becomes "\t"
                elif char in unescapes.keys():
                    appendCharclass(
                        charclass(unescapes[char])
                    )

                # word character class
                elif char == "w":
                    appendCharclass(w)

                # non-word character class
                elif char == "W":
                    appendCharclass(W)

                # digit character class
                elif char == "d":
                    appendCharclass(d)

                # non-digit character class
                elif char == "D":
                    appendCharclass(D)

                # whitespace character class
                elif char == "s":
                    appendCharclass(s)

                # non-whitespace character class
                elif char == "S":
                    appendCharclass(S)

                else:
                    raise Exception("Escaping something you can't here")

            # a dot can stand for any character. An negated empty class is our
            # rough hack for expressing this ("Any character that is not in the
            # empty class" = "any character")
            elif char == ".":
                appendCharclass(charclass(negateMe=True))

            # all other characters just count as single-character classes
            else:
                appendCharclass(charclass(char))

        # a "class" is a set of characters delimited by square brackets.
        # [abcd] is shorthand for (a|b|c|d) and much more convenient shorthands
        # are also available, e.g. [^a], [a-d], [\w], etc.
        elif mode == "class":

            if char == "]":

                # if a square bracket is the first character in a class,
                # then we add it to the class
                if i == pointOne:
                    incomplete = classunion(incomplete, charclass(char))

                # otherwise it closes the class
                else:

                    # this is where the possible initial ^ becomes significant
                    if classnegateAfterwards:
                        incomplete = classnegate(incomplete)

                    appendCharclass(incomplete)
                    mode = "outdoors"

            elif char == "-":

                # if a hyphen is the first character in a class, then we add
                # it to the class
                if i == pointOne:
                    incomplete = classunion(incomplete, charclass(char))

                # if it is the last character in a class then likewise
                elif i + 1 < len(string) and string[i + 1] == "]":
                    incomplete = classunion(incomplete, charclass(char))

                # otherwise it signals the middle of a range
                else:
                    # skip the hyphen itself
                    i += 1

                    if i >= len(string):
                        raise Exception("String terminates mid-range?")

                    firstChar = string[i - 2]
                    lastChar = string[i]

                    charRange = None
                    for allowableRange in allowableRanges:
                        if firstChar in allowableRange:
                            # first and last must be in the same character
                            # range
                            if lastChar not in allowableRange:
                                raise Exception("Invalid range!")

                            firstIndex = allowableRange.index(firstChar)
                            lastIndex = allowableRange.index(lastChar)

                            # and in order i.e. a < b
                            if firstIndex >= lastIndex:
                                raise Exception("Disordered range (a !< b)")

                            # OK
                            charRange = charclass(
                                allowableRange[firstIndex:lastIndex + 1]
                            )
                            break

                    # not found?
                    if charRange is None:
                        raise Exception("Range starter not allowed")

                    incomplete = classunion(incomplete, charRange)

            # inside [], backslashes can be used to escape different stuff
            # from when "outside"
            elif char == "\\":

                # skip the backslash itself
                i += 1

                # a backslash at the end of the string is dumb
                if i >= len(string):
                    raise Exception("Backslash at the end of the regex?")

                char = string[i]

                # these characters must be escaped inside classes
                if char in classSpecial:
                    incomplete = classunion(incomplete, charclass(char))

                # e.g. "t" becomes "\t"
                elif char in unescapes.keys():
                    incomplete = classunion(
                        incomplete,
                        charclass(unescapes[char]),
                    )

                # word character class
                elif char == "w":
                    incomplete = classunion(incomplete, w)

                # non-word character class
                elif char == "W":
                    incomplete = classunion(incomplete, W)

                # digit character class
                elif char == "d":
                    incomplete = classunion(incomplete, d)

                # non-digit character class
                elif char == "D":
                    incomplete = classunion(incomplete, D)

                # whitespace character class
                elif char == "s":
                    incomplete = classunion(incomplete, s)

                # non-whitespace characters
                elif char == "S":
                    incomplete = classunion(incomplete, S)

                else:
                    raise Exception("Can't escape that character in a [class]")

            # all other characters
            else:
                incomplete = classunion(incomplete, charclass(char))

        # the first half of a "{2,3}"- or "{3}"-like multiplier
        elif mode == "min":

            # these characters alone are okay inside a multiplier
            if char in d:
                min += char

            # lower bound has been covered; fix that and move to the next bit
            elif char == ",":

                # an empty lower bound is zero
                if min == "":
                    min = "0"

                # freeze and save as an integer
                min = int(min)

                mode = "max"

            # a right brace closes a multiplier immediately with identical
            # lower and upper bounds
            elif char == "}":

                # this is basically what you get if you type "{}"
                if min == "":
                    raise Exception("Cannot have an empty multiplier")

                # freeze and save as an integer
                min = int(min)

                # {A} = {A,A}
                max = min

                # close
                applyMultiplier(min, max)

                mode = "outdoors"

            # all other characters
            else:
                raise Exception("Can't put this here inside this multiplier")

        # the second half of a "{1,3}"- or "{1}"-like multiplier
        elif mode == "max":

            # these characters alone are okay inside a multiplier
            if char in d:
                max += char

            # a right brace closes a multiplier
            elif char == "}":

                # an empty upper bound is None (infinite)
                if max == "":
                    max = None

                # freeze and save as an integer
                else:
                    max = int(max)

                # close
                applyMultiplier(min, max)

                mode = "outdoors"

            # all other characters
            else:
                raise Exception("Can't put this inside a multiplier")

        else:
            raise Exception("What? Impossible mode!")

        # character processing complete: next!
        i += 1

    if mode != "outdoors":
        raise Exception("Unfinished square bracketage or multiplier")

    # there should be one last thing to complete
    if len(stack) > 2:
        raise Exception("Unfinished subpattern")

    # end conc, "end pattern"
    thisConc = stack.pop()

    # eliminate raw charclasses and patterns from the listing.
    for j in range(len(thisConc)):
        if type(thisConc[j]) is not mult:
            thisConc[j] = mult((thisConc[j], 1, 1))

    stack[-1].append(conc(thisConc))

    thisPattern = stack.pop()

    return pattern(thisPattern)

def transliterate(thisPattern, lookups):
    """Replace character classes inside a nested data structure with their
    equivalents from a lookup table."""
    newPattern = set()
    for thisConc in thisPattern:
        newConc = list()
        for thisMult in thisConc:
            thisMultiplicand, thisMin, thisMax = thisMult

            if type(thisMultiplicand) is charclass:
                thisMultiplicand = lookups[thisMultiplicand]

            else:
                thisMultiplicand = transliterate(thisMultiplicand, lookups)

            thisMult = mult((thisMultiplicand, thisMin, thisMax))
            newConc.append(thisMult)
        newConc = conc(newConc)
        newPattern.add(newConc)
    newPattern = pattern(newPattern)
    return newPattern

if __name__ == '__main__':
    for actual, expected in [
        # classnegate()
        (
            # ¬[ab] = [^ab]
            classnegate(charclass("ab")),
            charclass("ab", negateMe=True),
        ),
        (
            # ¬[^ab] = [ab]
            classnegate(charclass("ab", negateMe=True)),
            charclass("ab"),
        ),

        # classunion()
        (
            # [ab] u [bc] = [abc]
            classunion(charclass("ab"), charclass("bc")),
            charclass("abc"),
        ),
        (
            # [ab] u [^bc] = [^c]
            classunion(charclass("ab"), charclass("bc", negateMe=True)),
            charclass("c", negateMe=True),
        ),
        (
            # [^a] u [bc] = [^a]
            classunion(charclass("ab", negateMe=True), charclass("bc")),
            charclass("a", negateMe=True),
        ),
        (
            # [^ab] u [^bc] = [^b]
            classunion(
                charclass("ab", negateMe=True),
                charclass("bc", negateMe=True),
            ),
            charclass("b", negateMe=True),
        ),

        # classdifference()
        (
            # [ab] - [bc] = [a]
            classdifference(charclass("ab"), charclass("bc")),
            charclass("a"),
        ),
        (
            # [ab] - [^bc] = [b]
            classdifference(charclass("ab"), charclass("bc", negateMe=True)),
            charclass("b"),
        ),
        (
            # [^ab] - [bc] = [^abc]
            classdifference(charclass("ab", negateMe=True), charclass("bc")),
            charclass("abc", negateMe=True),
        ),
        (
            # [^ab] - [^bc] = [c]
            classdifference(
                charclass("ab", negateMe=True),
                charclass("bc", negateMe=True),
            ),
            charclass("c"),
        ),

        # classintersection
        (
            # [ab] n [bc] = [b]
            classintersection(charclass("ab"), charclass("bc")),
            charclass("b"),
        ),
        (
            # [ab] n [^bc] = [a]
            classintersection(charclass("ab"), charclass("bc", negateMe=True)),
            charclass("a"),
        ),
        (
            # [^ab] n [bc] = [c]
            classintersection(charclass("ab", negateMe=True), charclass("bc")),
            charclass("c"),
        ),
        (
            # [^ab] n [^bc] = [^abc]
            classintersection(
                charclass("ab", negateMe=True),
                charclass("bc", negateMe=True),
            ),
            charclass("abc", negateMe=True),
        ),

        # classissubset()
        (
            # [a] < [ab] = True
            classissubset(charclass("a"), charclass("ab")),
            True,
        ),
        (
            # [c] < [^ab] = True
            classissubset(charclass("c"), charclass("ab", negateMe=True)),
            True,
        ),
        (
            # [^c] < [ab] = False
            classissubset(charclass("c", negateMe=True), charclass("ab")),
            False,
        ),
        (
            # [^ab] < [^a] = True
            classissubset(
                charclass("ab", negateMe=True),
                charclass("a", negateMe=True),
            ),
            True,
        ),

    ]:
        if actual == expected:
            print("OK")
        else:
            print("Expected '" + str(expected) + "', actual '" + \
            str(actual) + "'")

    print("legoconcatenate():")
    for (A, B, expected) in [
        # charclass + charclass
        (
            # a + b = ab
            charclass("a"),
            charclass("b"),
            conc((
                mult((charclass("a"), 1, 1)),
                mult((charclass("b"), 1, 1)),
            ))
        ),
        (
            # a + a = a{2}
            charclass("a"),
            charclass("a"),
            mult((charclass("a"), 2, 2))
        ),

        # charclass + mult
        (
            # a + a = a{2}
            charclass("a"),
            mult((charclass("a"), 1, 1)),
            mult((charclass("a"), 2, 2))
        ),
        (
            # a + a{2,} = a{3,}
            charclass("a"),
            mult((charclass("a"), 2, None)),
            mult((charclass("a"), 3, None))
        ),
        (
            # a + a{,8} = a{1,9}
            charclass("a"),
            mult((charclass("a"), 0, 8)),
            mult((charclass("a"), 1, 9))
        ),
        (
            # a + b{,8} = ab{,8}
            charclass("a"), 
            mult((charclass("b"), 0, 8)),
            conc((
                mult((charclass("a"), 1, 1)),
                mult((charclass("b"), 0, 8)),
            ))
        ),

        # mult + charclass
        (
            # b + b = b{2}
            mult((charclass("b"), 1, 1)),
            charclass("b"),
            mult((charclass("b"), 2, 2))
        ),
        (
            # b* + b = b+
            mult((charclass("b"), 0, None)),
            charclass("b"),
            mult((charclass("b"), 1, None)),
        ),
        (
             # b{,8} + b = b{1,9}
            mult((charclass("b"), 0, 8)),
            charclass("b"),
            mult((charclass("b"), 1, 9)),
        ),
        (
            # b{,8} + c = b{,8}c
            mult((charclass("b"), 0, 8)),
            charclass("c"),
            conc((
                mult((charclass("b"), 0, 8)),
                mult((charclass("c"), 1, 1)),
            )),
        ),

        # charclass + conc
        (
            # a + nothing = a
            charclass("a"),
            conc(()),
            charclass("a"),
        ),
        (
            # a + bc = abc
            charclass("a"),
            conc((
                mult((charclass("b"), 1, 1)),
                mult((charclass("c"), 1, 1)),
            )),
            conc((
                mult((charclass("a"), 1, 1)),
                mult((charclass("b"), 1, 1)),
                mult((charclass("c"), 1, 1)),
            )),
        ),
        (
            # a + ab = a{2}b
            charclass("a"),
            conc((
                mult((charclass("a"), 1, 1)),
                mult((charclass("b"), 1, 1)),
            )),
            conc((
                mult((charclass("a"), 2, 2)),
                mult((charclass("b"), 1, 1)),
            )),
        ),

        # conc + charclass
        (
            # nothing + a = a
            conc(()),
            charclass("a"),
            charclass("a"),
        ),
        (
            # ab + c = abc
            conc((
                mult((charclass("a"), 1, 1)),
                mult((charclass("b"), 1, 1)),
            )),
            charclass("c"),
            conc((
                mult((charclass("a"), 1, 1)),
                mult((charclass("b"), 1, 1)),
                mult((charclass("c"), 1, 1)),
            )),
        ),
        (
            # ab + b = ab{2}
            conc((
                mult((charclass("a"), 1, 1)),
                mult((charclass("b"), 1, 1)),
            )),
            charclass("b"),
            conc((
                mult((charclass("a"), 1, 1)),
                mult((charclass("b"), 2, 2)),
            )),
        ),

        # pattern + charclass
        (
            # (a|bd) + c = (a|bd)c
            pattern({
                conc((
                    mult((charclass("a"), 1, 1)),
                )),
                conc((
                    mult((charclass("b"), 1, 1)),
                    mult((charclass("d"), 1, 1)),
                )),
            }),
            charclass("c"),
            conc((
                mult((
                    pattern({
                        conc((
                            mult((charclass("a"), 1, 1)),
                        )),
                        conc((
                            mult((charclass("b"), 1, 1)),
                            mult((charclass("d"), 1, 1)),
                        )),
                    }), 1, 1
                )),
                mult((charclass("c"), 1, 1)),
            )),
        ),
        (
            # (ac{2}|bc+) + c = (ac|bc*)c{2}
            pattern({
                conc((
                    mult((charclass("a"), 1, 1)),
                    mult((charclass("c"), 2, 2)),
                )),
                conc((
                    mult((charclass("b"), 1, 1)),
                    mult((charclass("c"), 1, None)),
                )),
            }),
            charclass("c"),
            conc((
                mult((
                    pattern({
                        conc((
                            mult((charclass("a"), 1, 1)),
                            mult((charclass("c"), 1, 1)),
                        )),
                        conc((
                            mult((charclass("b"), 1, 1)),
                            mult((charclass("c"), 0, None)),
                        )),
                    }), 1, 1
                )),
                mult((charclass("c"), 2, 2)),
            )),
        ),

        # charclass + pattern
        (
            # a + (b|cd) = a(b|cd)
            charclass("a"),
            pattern({
                conc((
                    mult((charclass("b"), 1, 1)),
                )),
                conc((
                    mult((charclass("c"), 1, 1)),
                    mult((charclass("d"), 1, 1)),
                )),
            }),
            conc((
                mult((charclass("a"), 1, 1)),
                mult((
                    pattern({
                        conc((
                            mult((charclass("b"), 1, 1)),
                        )),
                        conc((
                            mult((charclass("c"), 1, 1)),
                            mult((charclass("d"), 1, 1)),
                        )),
                    }), 1, 1
                ))
            )),
        ),
        (
            # a + (a{2}b|a+c) = a{2}(ab|a*c)
            charclass("a"),
            pattern({
                conc((
                    mult((charclass("a"), 2, 2)),
                    mult((charclass("b"), 1, 1)),
                )),
                conc((
                    mult((charclass("a"), 1, None)),
                    mult((charclass("c"), 1, 1)),
                )),
            }),
            conc((
                mult((charclass("a"), 2, 2)),
                mult((
                    pattern({
                        conc((
                            mult((charclass("a"), 1, 1)),
                            mult((charclass("b"), 1, 1)),
                        )),
                        conc((
                            mult((charclass("a"), 0, None)),
                            mult((charclass("c"), 1, 1)),
                        )),
                    }), 1, 1
                )),
            )),
        ),

        # mult + mult
        (
            # a{3,4} + b? = a{3,4}b?
            mult((charclass("a"), 3, 4)),
            mult((charclass("b"), 0, 1)),
            conc((
                mult((charclass("a"), 3, 4)),
                mult((charclass("b"), 0, 1)),
            ))
        ),
        (
            # a* + a{2} = a{2,}
            mult((charclass("a"), 0, None)),
            mult((charclass("a"), 2, 2)),
            mult((charclass("a"), 2, None)),
        ),

        # mult + conc
        (
            # a{2} + bc = a{2}bc
            mult((charclass("a"), 2, 2)),
            conc((
                mult((charclass("b"), 1, 1)),
                mult((charclass("c"), 1, 1)),
            )),
            conc((
                mult((charclass("a"), 2, 2)),
                mult((charclass("b"), 1, 1)),
                mult((charclass("c"), 1, 1)),
            )),
        ),
        (
            # a? + ab = a{1,2}b
            mult((charclass("a"), 0, 1)),
            conc((
                mult((charclass("a"), 1, 1)),
                mult((charclass("b"), 1, 1)),
            )),
            conc((
                mult((charclass("a"), 1, 2)),
                mult((charclass("b"), 1, 1)),
            )),
        ),

        # conc + mult
        (
            # ab + c* = abc*
            conc((
                mult((charclass("a"), 1, 1)),
                mult((charclass("b"), 1, 1)),
            )),
            mult((charclass("c"), 0, None)),
            conc((
                mult((charclass("a"), 1, 1)),
                mult((charclass("b"), 1, 1)),
                mult((charclass("c"), 0, None)),
            )),
        ),
        (
            # ab + b* = ab+
            conc((
                mult((charclass("a"), 1, 1)),
                mult((charclass("b"), 1, 1)),
            )),
            mult((charclass("b"), 0, None)),
            conc((
                mult((charclass("a"), 1, 1)),
                mult((charclass("b"), 1, None)),
            )),
        ),

        # mult + pattern
        (
            # a{2,3} + (b|cd) = a{2,3}(b|cd)
            mult((charclass("a"), 2, 3)),
            pattern({
                conc((
                    mult((charclass("b"), 1, 1)),
                )),
                conc((
                    mult((charclass("c"), 1, 1)),
                    mult((charclass("d"), 1, 1)),
                )),
            }),
            conc((
                mult((charclass("a"), 2, 3)),
                mult((
                    pattern({
                        conc((
                            mult((charclass("b"), 1, 1)),
                        )),
                        conc((
                            mult((charclass("c"), 1, 1)),
                            mult((charclass("d"), 1, 1)),
                        )),
                    }), 1, 1
                ))
            )),
        ),
        (
            # a{2,3} + (a{2}b|a+c) = a{3,4}(ab|a*c)
            mult((charclass("a"), 2, 3)),
            pattern({
                conc((
                    mult((charclass("a"), 2, 2)),
                    mult((charclass("b"), 1, 1)),
                )),
                conc((
                    mult((charclass("a"), 1, None)),
                    mult((charclass("c"), 1, 1)),
                )),
            }),
            conc((
                mult((charclass("a"), 3, 4)),
                mult((
                    pattern({
                        conc((
                            mult((charclass("a"), 1, 1)),
                            mult((charclass("b"), 1, 1)),
                        )),
                        conc((
                            mult((charclass("a"), 0, None)),
                            mult((charclass("c"), 1, 1)),
                        )),
                    }), 1, 1
                )),
            )),
        ),

        # pattern + mult
        (
            # (b|cd) + a{2,3} = (b|cd)a{2,3}
            pattern({
                conc((
                    mult((charclass("b"), 1, 1)),
                )),
                conc((
                    mult((charclass("c"), 1, 1)),
                    mult((charclass("d"), 1, 1)),
                )),
            }),
            mult((charclass("a"), 2, 3)),
            conc((
                mult((
                    pattern({
                        conc((
                            mult((charclass("b"), 1, 1)),
                        )),
                        conc((
                            mult((charclass("c"), 1, 1)),
                            mult((charclass("d"), 1, 1)),
                        )),
                    }), 1, 1
                )),
                mult((charclass("a"), 2, 3)),
            )),
        ),
        (
            # (ba{2}|ca+) + a{2,3} = (ba|ca*)a{3,4}
            pattern({
                conc((
                    mult((charclass("b"), 1, 1)),
                    mult((charclass("a"), 2, 2)),
                )),
                conc((
                    mult((charclass("c"), 1, 1)),
                    mult((charclass("a"), 1, None)),
                )),
            }),
            mult((charclass("a"), 2, 3)),
            conc((
                mult((
                    pattern({
                        conc((
                            mult((charclass("b"), 1, 1)),
                            mult((charclass("a"), 1, 1)),
                        )),
                        conc((
                            mult((charclass("c"), 1, 1)),
                            mult((charclass("a"), 0, None)),
                        )),
                    }), 1, 1
                )),
                mult((charclass("a"), 3, 4)),
            )),
        ),

        # conc + conc
        (
            # ab + cd = abcd
            conc((
                mult((charclass("a"), 1, 1)),
                mult((charclass("b"), 1, 1)),
            )),
            conc((
                mult((charclass("c"), 1, 1)),
                mult((charclass("d"), 1, 1)),
            )),
            conc((
                mult((charclass("a"), 1, 1)),
                mult((charclass("b"), 1, 1)),
                mult((charclass("c"), 1, 1)),
                mult((charclass("d"), 1, 1)),
            )),
        ),
        (
            # ab + bc = ab{2}c
            conc((
                mult((charclass("a"), 1, 1)),
                mult((charclass("b"), 1, 1)),
            )),
            conc((
                mult((charclass("b"), 1, 1)),
                mult((charclass("c"), 1, 1)),
            )),
            conc((
                mult((charclass("a"), 1, 1)),
                mult((charclass("b"), 2, 2)),
                mult((charclass("c"), 1, 1)),
            )),
        ),

        # conc + pattern
        (
            # za{2,3} + (b|cd) = za{2,3}(b|cd)
            conc((
                mult((charclass("z"), 1, 1)),
                mult((charclass("a"), 2, 3)),
            )),
            pattern({
                conc((
                    mult((charclass("b"), 1, 1)),
                )),
                conc((
                    mult((charclass("c"), 1, 1)),
                    mult((charclass("d"), 1, 1)),
                )),
            }),
            conc((
                mult((charclass("z"), 1, 1)),
                mult((charclass("a"), 2, 3)),
                mult((
                    pattern({
                        conc((
                            mult((charclass("b"), 1, 1)),
                        )),
                        conc((
                            mult((charclass("c"), 1, 1)),
                            mult((charclass("d"), 1, 1)),
                        )),
                    }), 1, 1,
                ))
            )),
        ),
        (
            # za{2,3} + (a{2}b|a+c) = za{3,4}(ab|a*c)
            conc((
                mult((charclass("z"), 1, 1)),
                mult((charclass("a"), 2, 3)),
            )),
            pattern({
                conc((
                    mult((charclass("a"), 2, 2)),
                    mult((charclass("b"), 1, 1)),
                )),
                conc((
                    mult((charclass("a"), 1, None)),
                    mult((charclass("c"), 1, 1)),
                )),
            }),
            conc((
                mult((charclass("z"), 1, 1)),
                mult((charclass("a"), 3, 4)),
                mult((
                    pattern({
                        conc((
                            mult((charclass("a"), 1, 1)),
                            mult((charclass("b"), 1, 1)),
                        )),
                        conc((
                            mult((charclass("a"), 0, None)),
                            mult((charclass("c"), 1, 1)),
                        )),
                    }), 1, 1
                )),
            )),
        ),

        # pattern + conc
        (
            # (b|cd) + za{2,3} = (b|cd)za{2,3}
            pattern({
                conc((
                    mult((charclass("b"), 1, 1)),
                )),
                conc((
                    mult((charclass("c"), 1, 1)),
                    mult((charclass("d"), 1, 1)),
                )),
            }),
            conc((
                mult((charclass("z"), 1, 1)),
                mult((charclass("a"), 2, 3)),
            )),
            conc((
                mult((
                    pattern({
                        conc((
                            mult((charclass("b"), 1, 1)),
                        )),
                        conc((
                            mult((charclass("c"), 1, 1)),
                            mult((charclass("d"), 1, 1)),
                        )),
                    }), 1, 1
                )),
                mult((charclass("z"), 1, 1)),
                mult((charclass("a"), 2, 3)),
            )),
        ),
        (
            # (ba{2}|ca+) + a{2,3}z = (ba|ca*)a{3,4}z
            pattern({
                conc((
                    mult((charclass("b"), 1, 1)),
                    mult((charclass("a"), 2, 2)),
                )),
                conc((
                    mult((charclass("c"), 1, 1)),
                    mult((charclass("a"), 1, None)),
                )),
            }),
            conc((
                mult((charclass("a"), 2, 3)),
                mult((charclass("z"), 1, 1)),
            )),
            conc((
                mult((
                    pattern({
                        conc((
                            mult((charclass("b"), 1, 1)),
                            mult((charclass("a"), 1, 1)),
                        )),
                        conc((
                            mult((charclass("c"), 1, 1)),
                            mult((charclass("a"), 0, None)),
                        )),
                    }), 1, 1
                )),
                mult((charclass("a"), 3, 4)),
                mult((charclass("z"), 1, 1)),
            )),
        ),

        # pattern + pattern
        (
            # (a|bc) + (c|de) = (a|bc)(c|de)
            pattern({
                conc((
                    mult((charclass("a"), 1, 1)),
                )),
                conc((
                    mult((charclass("b"), 1, 1)),
                    mult((charclass("c"), 1, 1)),
                )),
            }),
            pattern({
                conc((
                    mult((charclass("c"), 1, 1)),
                )),
                conc((
                    mult((charclass("d"), 1, 1)),
                    mult((charclass("e"), 1, 1)),
                )),
            }),
            conc((
                mult((
                    pattern({
                        conc((
                            mult((charclass("a"), 1, 1)),
                        )),
                        conc((
                            mult((charclass("b"), 1, 1)),
                            mult((charclass("c"), 1, 1)),
                        )),
                    }), 1, 1
                )),
                mult((
                    pattern({
                        conc((
                            mult((charclass("c"), 1, 1)),
                        )),
                        conc((
                            mult((charclass("d"), 1, 1)),
                            mult((charclass("e"), 1, 1)),
                        )),
                    }), 1, 1
                )),
            )),
        ),
        (
            # (a|bc) + (a|bc) = (a|b){2}
            pattern({
                conc((
                    mult((charclass("a"), 1, 1)),
                )),
                conc((
                    mult((charclass("b"), 1, 1)),
                    mult((charclass("c"), 1, 1)),
                )),
            }),
            pattern({
                conc((
                    mult((charclass("a"), 1, 1)),
                )),
                conc((
                    mult((charclass("b"), 1, 1)),
                    mult((charclass("c"), 1, 1)),
                )),
            }),
            mult((
                pattern({
                    conc((
                        mult((charclass("a"), 1, 1)),
                    )),
                    conc((
                        mult((charclass("b"), 1, 1)),
                        mult((charclass("c"), 1, 1)),
                    )),
                }), 2, 2
            )),
        )
    ]:
        actual = legoconcatenate(A, B)
        if actual == expected:
            print("OK")
        else:
            print("Concatenated '" + str(A) + "' and '" + \
            str(B) + "', expected '" + str(expected) + "', actual '" + \
            str(actual) + "'")

    print("legoalternate():")
    for input, expected in [

        # charclass | conc
        (
            # f | nothing = f?
            [
                charclass("f"),
                conc(()),
            ],
            mult((charclass("f"), 0, 1)),
        ),
        (
            # f|fghi = f(ghi)?
            [
                charclass("f"),
                conc((
                    mult((charclass("f"), 1, 1)),
                    mult((charclass("g"), 1, 1)),
                    mult((charclass("h"), 1, 1)),
                    mult((charclass("i"), 1, 1)),
                ))
            ],
            conc((
                mult((charclass("f"), 1, 1)),
                mult((
                    pattern((
                        conc((
                            mult((charclass("g"), 1, 1)),
                            mult((charclass("h"), 1, 1)),
                            mult((charclass("i"), 1, 1)),
                        )),
                    )), 0, 1
                ))
            )),
        ),

        # conc | conc
        (
            # fa | fbc = f(a|bc)
            [
                conc((
                    mult((charclass("f"), 1, 1)),
                    mult((charclass("a"), 1, 1)),
                )),
                conc((
                    mult((charclass("f"), 1, 1)),
                    mult((charclass("b"), 1, 1)),
                    mult((charclass("c"), 1, 1)),
                )),
            ],
            conc((
                mult((charclass("f"), 1, 1)),
                mult((
                    pattern({
                        conc((
                            mult((charclass("a"), 1, 1)),
                        )),
                        conc((
                            mult((charclass("b"), 1, 1)),
                            mult((charclass("c"), 1, 1)),
                        )),
                    }), 1, 1
                )),
            )),
        ),
    ]:
        actual = legoalternate(input)
        if actual == expected:
            print("OK")
        else:
            print("Alternated '" + str(input) + "', expected '" + \
            str(expected) + "', actual '" + str(actual) + "'")

    # _legoreduce()
    for (A, expected) in [

        # mult -> mult
        (
            mult((charclass("a"), 0, 1)),
            mult((charclass("a"), 0, 1)),
        ),

        # mult -> charclass
        (
            mult((charclass("a"), 1, 1)),
            charclass("a"),
        ),

        # conc -> conc
        (
            conc((
                mult((charclass("a"), 1, 1)),
                mult((charclass("b"), 1, 1)),
            )),
            conc((
                mult((charclass("a"), 1, 1)),
                mult((charclass("b"), 1, 1)),
            )),
        ),
        
        # conc -> mult
        (
            conc((
                mult((charclass("a"), 3, 4)),
            )),
            mult((charclass("a"), 3, 4)),
        ),
        
        # conc -> charclass
        (
            conc((
                mult((charclass("a"), 1, 1)),
            )),
            charclass("a"),
        ),

        # pattern -> pattern
        (
            pattern({
                conc((
                    mult((charclass("a"), 2, 2)),
                    mult((charclass("b"), 2, 2)),
                )),
                conc((
                    mult((charclass("c"), 2, 2)),
                    mult((charclass("d"), 2, 2)),
                )),
            }),
            pattern({
                conc((
                    mult((charclass("a"), 2, 2)),
                    mult((charclass("b"), 2, 2)),
                )),
                conc((
                    mult((charclass("c"), 2, 2)),
                    mult((charclass("d"), 2, 2)),
                )),
            }),
        ),
        
        # pattern -> conc
        (
            pattern({
                conc((
                    mult((charclass("a"), 2, 2)),
                    mult((charclass("b"), 2, 2)),
                )),
            }),
            conc((
                mult((charclass("a"), 2, 2)),
                mult((charclass("b"), 2, 2)),
            )),
        ),
        
        # pattern -> mult
        (
            pattern({
                conc((
                    mult((charclass("a"), 2, 2)),
                )),
            }),
            mult((charclass("a"), 2, 2)),
        ),
        
        # pattern -> charclass
        (
            pattern({
                conc((
                    mult((charclass("a"), 1, 1)),
                )),
            }),
            charclass("a"),
        ),
    ]:
        actual = _legoreduce(A)
        if expected == actual:
            print("OK")
        else:
            print("Reduced '" + str(A) + "', expected '" + \
            str(expected) + "', actual '" + str(actual) + "'")

    # legomultiply()
    for (A, minA, maxA, expected) in [
        # charclass
        (
            # a * 1 = a
            charclass("a"),
            1,
            1,
            charclass("a"),
        ),
        (
            # a * {1,3} = a{1,3}
            charclass("a"),
            1,
            3,
            mult((charclass("a"), 1, 3))
        ),
        (
            # a * {4,} = a{4,}
            charclass("a"),
            4,
            None,
            mult((charclass("a"), 4, None))
        ),

        # mult
        (
            # a{2,3} * 1 = a{2,3}
            mult((charclass("a"), 2, 3)),
            1,
            1,
            mult((charclass("a"), 2, 3)),
        ),
        (
            # a{2,3} * {4,5} = a{8,15}
            mult((charclass("a"), 2, 3)),
            4,
            5,
            mult((charclass("a"), 8, 15)),
        ),
        (
            # a{2,} * {2,None} = a{4,}
            mult((charclass("a"), 2, None)),
            2,
            None,
            mult((charclass("a"), 4, None)),
        ),

        # conc
        (
            # ab? * {0,1} = (ab?)?
            conc((
                mult((charclass("a"), 1, 1)),
                mult((charclass("b"), 0, 1)),
            )),
            0,
            1,
            mult((
                pattern({
                    conc((
                        mult((charclass("a"), 1, 1)),
                        mult((charclass("b"), 0, 1)),
                    )),
                }), 0, 1
            ))
        ),

        # pattern
        (
            # (ab?|ba?) * {2,3} = (ab?|ba?){2,3}
            pattern({
                conc((
                    mult((charclass("a"), 1, 1)),
                    mult((charclass("b"), 0, 1)),
                )),
                conc((
                    mult((charclass("b"), 1, 1)),
                    mult((charclass("a"), 0, 1)),
                )),
            }),
            2,
            3,
            mult((
                pattern({
                    conc((
                        mult((charclass("a"), 1, 1)),
                        mult((charclass("b"), 0, 1)),
                    )),
                    conc((
                        mult((charclass("b"), 1, 1)),
                        mult((charclass("a"), 0, 1)),
                    )),
                }), 2, 3
            ))
        ),
    ]:
        actual = legomultiply(A, minA, maxA)
        if actual == expected:
            print("OK")
        else:
            print("Multiplied '" + str(A) + "' by {" + \
            str(minA) + "," + str(maxA) + \
            "}, expected '" + str(expected) + "', actual '" + \
            str(actual) + "'")

    # _commonmultpart()
    print("_commonmultpart():")
    for A, expected in [

        # mult, mult
        (
            # a, b -> None
            [
                mult((charclass("a"), 1, 1)),
                mult((charclass("b"), 1, 1)),
            ],
            (
                None,
                [
                    mult((charclass("a"), 1, 1)),
                    mult((charclass("b"), 1, 1)),
                ],
            ),
        ),
        (
            # a, a -> a
            [
                mult((charclass("a"), 1, 1)),
                mult((charclass("a"), 1, 1)),
            ],
            (
                mult((charclass("a"), 1, 1)),
                [
                    None,
                    None,
                ],
            ),
        ),
        (
            # a{3,4}, a{2,5} -> a{2}
            [
                mult((charclass("a"), 3, 4)),
                mult((charclass("a"), 2, 5)),
            ],
            (
                mult((charclass("a"), 2, 2)),
                [
                    mult((charclass("a"), 1, 2)),
                    mult((charclass("a"), 0, 3)),
                ],
            ),
        ),
        (
            # a{2,}, a{1,5} -> a{1}
            [
                mult((charclass("a"), 2, None)),
                mult((charclass("a"), 1, 5)),
            ],
            (
                mult((charclass("a"), 1, 1)),
                [
                    mult((charclass("a"), 1, None)),
                    mult((charclass("a"), 0, 4)),
                ],
            ),
        ),
        (
            # a{3,}, a{2,} -> a{2,2}
            [
                mult((charclass("a"), 3, None)),
                mult((charclass("a"), 2, None)),
            ],
            (
                mult((charclass("a"), 2, 2)),
                [
                    mult((charclass("a"), 1, None)),
                    mult((charclass("a"), 0, None)),
                ],
            ),
        ),
        (
            # a{3,}, a{3,} -> a{3,}
            [
                mult((charclass("a"), 3, None)),
                mult((charclass("a"), 3, None)),
            ],
            (
                mult((charclass("a"), 3, None)),
                [
                    None,
                    None,
                ]
            ),
        ),
    ]:
        actual = _commonmultpart(A)
        if actual == expected:
            print("OK")
        else:
            print("Took _commonmultpart of '" + str(A) + "', expected '" \
            + str(expected) + "', actual '" + str(actual) + "'")

    print("_concprefix():")
    for A, expected in [
        (
            # nothing -> nothing
            pattern(set()),
            [],
        ),
        (
            # e | nothing = e?
            pattern({
                conc(()),
                conc((
                    mult((charclass("e"), 1, 1)),
                )),
            }),
            [
                mult((charclass("e"), 0, 1)),
            ],
        ),
        (
            # a, bc -> None
            pattern({
                conc((
                    mult((charclass("a"), 1, 1)),
                )),
                conc((
                    mult((charclass("b"), 1, 1)),
                    mult((charclass("c"), 1, 1)),
                )),
            }),
            [
                mult((
                    pattern({
                        conc((
                            mult((charclass("a"), 1, 1)),
                        )),
                        conc((
                            mult((charclass("b"), 1, 1)),
                            mult((charclass("c"), 1, 1)),
                        )),
                    }), 1, 1
                )),
            ],
        ),
        (
            # aa, aa -> aa
            pattern({
                conc((
                    mult((charclass("a"), 1, 1)),
                    mult((charclass("a"), 1, 1)),
                )),
                conc((
                    mult((charclass("a"), 1, 1)),
                    mult((charclass("a"), 1, 1)),
                )),
            }),
            [
                mult((charclass("a"), 1, 1)),
                mult((charclass("a"), 1, 1)),
            ],
        ),
        (
            # abc, aa -> a(a|bc)
            pattern({
                conc((
                    mult((charclass("a"), 1, 1)),
                    mult((charclass("b"), 1, 1)),
                    mult((charclass("c"), 1, 1)),
                )),
                conc((
                    mult((charclass("a"), 1, 1)),
                    mult((charclass("a"), 1, 1)),
                )),
            }),
            [
                mult((charclass("a"), 1, 1)),
                mult((
                    pattern({
                        conc((
                            mult((charclass("a"), 1, 1)),
                        )),
                        conc((
                            mult((charclass("b"), 1, 1)),
                            mult((charclass("c"), 1, 1)),
                        )),
                    }), 1, 1,
                )),
            ],
        ),
        (
            # cf{1,2}, cf -> cff?
            pattern({
                conc((
                    mult((charclass("c"), 1, 1)),
                    mult((charclass("f"), 1, 2)),
                )),
                conc((
                    mult((charclass("c"), 1, 1)),
                    mult((charclass("f"), 1, 1)),
                )),
            }),
            [
                mult((charclass("c"), 1, 1)),
                mult((charclass("f"), 1, 1)),
                mult((charclass("f"), 0, 1)),
            ],
        ),
    ]:
        actual = _concprefix(A)
        if actual == expected:
            print("OK")
        else:
            print("Took _concprefix of '" + str(A) + "', expected '" + \
            str(expected) + "', actual '" + str(actual) + "'")

    print("_concsuffix():")
    for A, expected in [
        (
            # nothing | e = e?
            pattern({
                conc(()),
                conc((
                    mult((charclass("e"), 1, 1)),
                )),
            }),
            [
                mult((charclass("e"), 0, 1)),
            ],
        ),
        (
            # a | bc -> a|bc
            pattern({
                conc((
                    mult((charclass("a"), 1, 1)),
                )),
                conc((
                    mult((charclass("b"), 1, 1)),
                    mult((charclass("c"), 1, 1)),
                )),
            }),
            [
                mult((
                    pattern({
                        conc((
                            mult((charclass("a"), 1, 1)),
                        )),
                        conc((
                            mult((charclass("b"), 1, 1)),
                            mult((charclass("c"), 1, 1)),
                        )),
                    }), 1, 1
                )),
            ],
        ),
        (
            # aa | aa -> aa
            pattern({
                conc((
                    mult((charclass("a"), 1, 1)),
                    mult((charclass("a"), 1, 1)),
                )),
                conc((
                    mult((charclass("a"), 1, 1)),
                    mult((charclass("a"), 1, 1)),
                )),
            }),
            [
                mult((charclass("a"), 1, 1)),
                mult((charclass("a"), 1, 1)),
            ],
        ),
        (
            # aa, bca -> (a|bc)a
            pattern({
                conc((
                    mult((charclass("a"), 1, 1)),
                    mult((charclass("a"), 1, 1)),
                )),
                conc((
                    mult((charclass("b"), 1, 1)),
                    mult((charclass("c"), 1, 1)),
                    mult((charclass("a"), 1, 1)),
                )),
            }),
            [
                mult((
                    pattern({
                        conc((
                            mult((charclass("a"), 1, 1)),
                        )),
                        conc((
                            mult((charclass("b"), 1, 1)),
                            mult((charclass("c"), 1, 1)),
                        )),
                    }), 1, 1,
                )),
                mult((charclass("a"), 1, 1)),
            ],
        ),
        (
            # f{1,2}c, fc -> f?fc
            pattern({
                conc((
                    mult((charclass("f"), 1, 2)),
                    mult((charclass("c"), 1, 1)),
                )),
                conc((
                    mult((charclass("f"), 1, 1)),
                    mult((charclass("c"), 1, 1)),
                )),
            }),
            [
                mult((charclass("f"), 0, 1)),
                mult((charclass("f"), 1, 1)),
                mult((charclass("c"), 1, 1)),
            ],
        ),
    ]:
        actual = _concsuffix(A)
        if actual == expected:
            print("OK")
        else:
            print("Took _concsuffix of '" + str(A) + "', expected '" +
            str(expected) + "', actual '" + str(actual) + "'")

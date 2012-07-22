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
from lego import charclass
from legoops import transliterate, legobuild, classdifference, \
classintersection, classissubset
from fsmops import fsmintersect
from mainops import fsmbuild, regexbuild

strings = sys.argv[1:]

# turn each string into a pattern
patterns = [legobuild(string) for string in strings]

# The set of Unicode characters contains 1,114,112 characters, but we are not
# worried about accounting for all of them in our alphabet. A typical list of
# regexes will include a very small subset of those characters, but we are not
# even worried about those: if a regex only includes references to [a-zA-Z] and
# [a-zA-Z0-9], then we only really need *two* symbols: "[a-zA-Z]" and "[0-9]".
# [a-zA-Z0-9] is equivalent to "[a-zA-Z]|[0-9]".

# In order to construct this alphabet of symbols of this form, first we need to
# identify every charclass which appears in our regexes.
allCharclasses = set()
for thisPattern in patterns:
    allCharclasses.update(thisPattern.getCharclasses())

# next we partition the space of Unicode characters into an "alphabet" of
# "symbols", each symbol representing one or more Unicode characters, in such
# a way that all of the charclasses seen above can be constructed from these
# symbols in a unique way.
# A sample alphabet is [ab], [c], [^abc]
alphabet = {charclass(negateMe=True)}
for Z in allCharclasses:

    # add Z to the alphabet.

    newAlphabet = set()
    for A in alphabet:
        # add A - Z, A + Z.
        for newClass in [classintersection(A, Z), classdifference(A, Z)]:
            if newClass != charclass():
                newAlphabet.add(newClass)

    # add Z - (everything else)
    for A in alphabet:
        Z = classdifference(Z, A)
    if Z != charclass():
        newAlphabet.add(Z)

    alphabet = newAlphabet

# Next we explicitly deconstruct each charclass we have seen into the form of
# a union of disjoint symbols (unicode character sets) taken from our alphabet.
# e.g. if our alphabet is [ab], [c], [^abc], then [^ab] is constructed as
# [c]|[^abc]
lookups = {}
for Z in allCharclasses:
    key = Z
    symbols = set()
    for symbol in alphabet:
        if classissubset(symbol, Z):
            symbols.add(symbol)
            Z = classdifference(Z, symbol)

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
fsms = [fsmbuild(thisPattern) for thisPattern in patterns]

# and combine them
combinedFsm = fsmintersect(fsms)

# and get a regex back
print(regexbuild(combinedFsm))
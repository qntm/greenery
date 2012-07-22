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

class charclass(frozenset):
    """A charclass is basically a frozenset of symbols. The reason for the
    charclass object instead of using frozenset directly is to allow us to
    set a "negated" flag. A charclass with the negation flag set is assumed
    to contain every symbol that is in the alphabet of all symbols but not
    explicitly listed inside the frozenset. e.g. [^a]. This is very handy
    if the full alphabet is extremely large, but also requires dedicated
    combination functions like classunion and classissubset.

    There is no restriction on the precise nature of a "symbol". A symbol can
    be basically anything. However, it's only practical to print a charclass
    out if its symbols are all individual characters.
    """

    def __init__(self, *args, negateMe=False):
        # not shown: frozenset constructor, called implicitly with *args
        self.negated = negateMe
        pass

    def __eq__(self, other):
        if type(self) != type(other):
            return False
        if self.negated != other.negated:
            return False
        return frozenset(self) == frozenset(other)

    def __ne__(self, other):
        if type(self) != type(other):
            return True
        if self.negated != other.negated:
            return True
        return frozenset(self) != frozenset(other)

    __hash__ = frozenset.__hash__

    def getString(self):

        if self.negated:

            # match EVERYTHING
            if len(self) == 0:
                return "."

            # word chars
            if self == W:
                return "\\W"

            # digit chars
            if self == D:
                return "\\D"

            # space chars
            if self == S:
                return "\\S"

            # multiple characters
            return "[^" + self.escape() + "]"

        else:

            # word chars
            if self == w:
                return "\\w"

            # digit chars
            if self == d:
                return "\\d"

            # space chars
            if self == s:
                return "\\s"

            # single character, not contained inside square brackets.
            if len(self) == 1:
                # Python lacks the Axiom of Choice
                char = "".join(self)

                if char in allSpecial:
                    return "\\" + char

                # e.g. if char is "\t", return "\\t"
                if char in escapes.keys():
                    return "\\" + escapes[char]

                return char

            # multiple characters
            return "[" + self.escape() + "]"

    def escape(self):

        def escapeChar(char):
            if char in classSpecial:
                return "\\" + char
            if char in escapes.keys():
                return "\\" + escapes[char]
            return char

        def recordRange():
            nonlocal currentRange
            nonlocal output

            # there's no point in putting a range when the whole thing is
            # 3 characters or fewer.
            if len(currentRange) in {0, 1, 2, 3}:
                output += "".join(escapeChar(char) for char in currentRange)
            else :
                output += escapeChar(currentRange[0]) + "-" + \
                escapeChar(currentRange[-1])

            currentRange = ""

        unused = set(self)
        output = ""

        # if there's a right bracket in the range, put it first
        # this is easier than escaping it
        if "]" in unused:
            output += "]"
            unused.remove("]")

        # if there's a hyphen in the range, put it last
        # this is easier than escaping it
        hyphen = False
        if "-" in unused:
            hyphen = True
            unused.remove("-")

        # use shorthand for known character ranges
        # note the nested processing order. DO NOT process \d before processing
        # \w. if more character class constants arise which do not nest nicely,
        # a problem will arise because there is no clear ordering to use...

        # \w
        if w.issubset(unused):
            output += "\\w"
            unused -= w

        # \d
        if d.issubset(unused):
            output += "\\d"
            unused -= d

        # \s
        if s.issubset(unused):
            output += "\\s"
            unused -= s

        # look for ranges
        currentRange = ""
        for char in sorted(unused):

            # range is not empty: new char must fit after previous one
            if len(currentRange) > 0:

                # find out if this character appears in any of the
                # allowableRanges listed above.
                superRange = None
                for allowableRange in allowableRanges:
                    if char in allowableRange:
                        superRange = allowableRange

                if superRange is None:
                    # if this character doesn't appear above, then any existing
                    # currentRange should be sorted and filed now
                    # if there is one
                    recordRange()

                else:
                    index = superRange.index(char)

                    # char doesn't fit old range: restart
                    if index == 0 or superRange[index-1] != currentRange[-1]:
                        recordRange()

            currentRange += char

        recordRange()

        # if there's a hyphen in the range, put it last
        if hyphen == True:
            output += "-"

        return output

# some useful constants
w = charclass("0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz")
d = charclass("0123456789")
s = charclass(" \f\n\r\t\v")
W = charclass(w, negateMe=True)
D = charclass(d, negateMe=True)
S = charclass(s, negateMe=True)

escapes = {
    "\f" : "f", # form feed
    "\n" : "n", # line feed
    "\r" : "r", # carriage return
    "\t" : "t", # tab
    "\v" : "v", # vertical tab
}

unescapes = {
    "f" : "\f", # form feed
    "n" : "\n", # line feed
    "r" : "\r", # carriage return
    "t" : "\t", # tab
    "v" : "\v", # vertical tab
}

# these are the characters carrying special meanings when they appear "outdoors"
# within a regular expression. To be interpreted literally, they must be
# escaped with a backslash.
allSpecial = set("\\[]|().?*+{}")

# these are the characters carrying special meanings when they appear INSIDE a
# character class (delimited by square brackets) within a regular expression.
# To be interpreted literally, they must be escaped with a backslash.
# Notice how much smaller this class is than the one above; note also that the
# hyphen does NOT appear above.
classSpecial = set("\\[]^-")

# these are the character ranges which can be used inside square brackets e.g.
# "[a-z]", "[F-J]". These ranges should be disjoint, and sorted in order.
# In future the functionality to have arbitrary ranges of unicode characters
# may be added.
allowableRanges = {
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ",
    "abcdefghijklmnopqrstuvwxyz",
    "0123456789",
}

class mult(tuple):
    """
    A mult is a combination of a multiplicand (a charclass or subpattern) with
    a multiplier (a min and a max). The vast majority of characters in regular
    expressions occur without a specific multiplier, which is implicitly
    equivalent to a min of 1 and a max of 1, but many more have explicit
    multipliers like "*" (min = 0, max = infinity) and so on. We use None to
    stand for infinity in the value of max.

    e.g. a, b{2}, c?, d*, [efg]{2,5}, f{2,}, (anysubpattern)+, .*, and so on
    """

    def __init__(self, tuple):
        multiplicand, min, max = tuple

        if type(multiplicand) not in {charclass, pattern}:
            raise Exception("Wrong type for multiplicand")

        if type(min) != int:
            raise Exception("min must be an integer")
        if min < 0:
            raise Exception("min must be > 0")

        if max is None:
            return
        if type(max) != int:
            raise Exception("max must be None or an integer")
        if max < min:
            raise Exception("max must match or exceed min")

    def getCharclasses(self):
        (multiplicand, min, max) = self

        # recurse into subpattern
        if type(multiplicand) is pattern:
            return multiplicand.getCharclasses()

        else:
            return {multiplicand}

        raise Exception("Huh?")

    def getString(self):
        """Return a string representation of the mult represented here."""

        (multiplicand, min, max) = self

        output = ""

        # recurse into subpattern
        if type(multiplicand) is pattern:
            output += "(" + multiplicand.getString() + ")"

        else: 
            output += multiplicand.getString()

        # try this with "a?b*c+d{1}e{1,2}f{3,}g{,5}h{8,8}"
        if (min, max) == (0, 1):
            output += "?"

        elif (min, max) == (0, None):
            output += "*"

        elif (min, max) == (1, 1):
            pass

        elif (min, max) == (1, None):
            output += "+"

        elif max is None:
            output += "{" + str(min) + ",}"

        elif min == max:
            suffix = "{" + str(min) + "}"

            # Pick whatever is shorter/more comprehensible.
            # e.g. "aa" beats "a{2}", "ababab" beats "(ab){3}"
            if len(output) * min <= len(output) + len(suffix):
                output += str(output) * (min - 1)
            else:
                output += suffix
        
        elif min == 0:
            output += "{," + str(max) + "}"

        else:
            output += "{" + str(min) + "," + str(max) + "}"

        return output

class conc(tuple):
    """A conc (short for "concatenation") is a tuple of mults i.e. an unbroken
    string of mults occurring one after the other.

    e.g. abcde[^fg]*h{4}[a-z]+(subpattern)(subpattern2)

    To express the empty string, use an empty conc, conc(()).
    """

    def __init__(self, tuple):
        for x in tuple:
            if type(x) is not mult:
                print(type(x))
                raise Exception("That's no mult! That's a space station")

    def getCharclasses(self):
        charclasses = set()
        for x in self:
            charclasses.update(x.getCharclasses())
        return charclasses

    def getString(self):
        """Return a string representation of regex which this conc
        represents."""
        return "".join(thisMult.getString() for thisMult in self)

class pattern(frozenset):
    """A pattern (also known as an "alt", short for "alternation") is a
    frozenset of concs. The simplest pattern contains a single conc, but it
    is also possible for a pattern to contain multiple alternate possibilities.
    When written out as a regex, these would separated by pipes. A pattern
    containing no possibilities should be impossible.
    
    e.g. "abc|def(ghi|jkl)" is an alt containing two concs: "abc" and
    "def(ghi|jkl)". The latter is a conc containing four mults: "d", "e", "f"
    and "(ghi|jkl)". The latter in turn is a mult consisting of an upper bound
    1, a lower bound 1, and a multiplicand which is a new subpattern, "ghi|jkl".
    This new subpattern again consists of two concs: "ghi" and "jkl".
    """

    def getCharclasses(self):
        charclasses = set()
        for x in self:
            charclasses.update(x.getCharclasses())
        return charclasses

    def getString(self):
        """Return the string representation of the regex which this pattern
        represents."""

        # take the alternation of the input collection of regular expressions.
        # i.e. jam "|" between each element

        # nothing to orify
        if len(self) == 0:
            raise Exception("Can't take alternation of nothing")

        # 1+ elements.
        return "|".join(sorted(x.getString() for x in self))

# unit tests
if __name__ == '__main__':
    for actual, expected in [

        (
            # [a] != [^a]
            (charclass("a", negateMe=True) == charclass("a")),
            False
        ),

    ]:
        if actual == expected:
            print("OK")
        else:
            print("Expected '" + str(expected) + "', actual '" + \
            str(actual) + "'")

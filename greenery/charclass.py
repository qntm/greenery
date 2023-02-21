from __future__ import annotations

__all__ = (
    "Charclass",
    "DIGIT",
    "DOT",
    "NONDIGITCHAR",
    "NONSPACECHAR",
    "NONWORDCHAR",
    "NULLCHARCLASS",
    "SPACECHAR",
    "WORDCHAR",
    "escapes",
    "shorthand",
)

from dataclasses import dataclass

from .fsm import ANYTHING_ELSE, Fsm


@dataclass(frozen=True)
class Charclass:
    '''
        A `Charclass` is basically a `frozenset` of symbols.
        A `Charclass` with the `negated` flag set is assumed
        to contain every symbol that is in the alphabet of all symbols but not
        explicitly listed inside the frozenset. e.g. [^a]. This is very handy
        if the full alphabet is extremely large, but also requires dedicated
        combination functions.
    '''
    chars: frozenset[str] | str
    negated: bool = False

    def __post_init__(self):
        object.__setattr__(self, "chars", frozenset(self.chars))
        # chars should consist only of chars
        if ANYTHING_ELSE in self.chars:
            raise Exception(
                f"Can't put {repr(ANYTHING_ELSE)} in a `Charclass`"
            )

    def __eq__(self, other):
        return isinstance(other, Charclass) \
               and self.chars == other.chars \
               and self.negated == other.negated

    def __hash__(self):
        return hash((self.chars, self.negated))

    # These are the characters carrying special meanings when they appear
    # "outdoors" within a regular expression. To be interpreted literally, they
    # must be escaped with a backslash.
    allSpecial = set("\\[]|().?*+{}")

    # These are the characters carrying special meanings when they appear
    # INSIDE a character class (delimited by square brackets) within a regular
    # expression. To be interpreted literally, they must be escaped with a
    # backslash. Notice how much smaller this class is than the one above; note
    # also that the hyphen and caret do NOT appear above.
    classSpecial = set("\\[]^-")

    # Shorthand codes for use inside `Charclass`es e.g. [abc\d]
    w = frozenset(
        "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz"
    )
    d = frozenset("0123456789")
    s = frozenset("\t\n\v\f\r ")

    shorthand = {
        w: "\\w",
        d: "\\d",
        s: "\\s",
    }

    negated_shorthand = {
        w: "\\W",
        d: "\\D",
        s: "\\S",
    }

    def __str__(self):
        # e.g. \w
        if self in shorthand.keys():
            return shorthand[self]

        # e.g. [^a]
        if self.negated:
            return "[^" + self.escape() + "]"

        # single character, not contained inside square brackets.
        if len(self.chars) == 1:
            # Python lacks the Axiom of Choice
            char = "".join(self.chars)

            # e.g. if char is "\t", return "\\t"
            if char in escapes.keys():
                return escapes[char]

            if char in Charclass.allSpecial:
                return "\\" + char

            # If char is an ASCII control character, don't print it directly,
            # return a hex escape sequence e.g. "\\x00". Note that this
            # includes tab and other characters already handled above
            if 0 <= ord(char) <= 0x1F or ord(char) == 0x7f:
                return "\\x" + "{0:02x}".format(ord(char))

            return char

        # multiple characters (or possibly 0 characters)
        return "[" + self.escape() + "]"

    def escape(self):
        def escapeChar(char):
            if char in Charclass.classSpecial:
                return "\\" + char
            if char in escapes.keys():
                return escapes[char]

            # If char is an ASCII control character, don't print it directly,
            # return a hex escape sequence e.g. "\\x00". Note that this
            # includes tab and other characters already handled above
            if 0 <= ord(char) <= 0x1F or ord(char) == 0x7f:
                return "\\x" + "{0:02x}".format(ord(char))

            return char

        def recordRange():
            # there's no point in putting a range when the whole thing is
            # 3 characters or fewer. "abc" -> "abc" but "abcd" -> "a-d"
            strs = [
                # "ab" or "abc" or "abcd"
                "".join(escapeChar(char) for char in currentRange),
                # "a-b" or "a-c" or "a-d"
                (
                    escapeChar(currentRange[0]) +
                    "-" +
                    escapeChar(currentRange[-1])
                ),
            ]
            return sorted(strs, key=lambda str: len(str))[0]

        output = ""

        # use shorthand for known character ranges
        # note the nested processing order. DO NOT process \d before processing
        # \w. if more character class constants arise which do not nest nicely,
        # a problem will arise because there is no clear ordering to use...

        # look for ranges
        currentRange = ""
        for char in sorted(self.chars, key=ord):

            # range is not empty: new char must fit after previous one
            if len(currentRange) > 0:

                i = ord(char)

                # char doesn't fit old range: restart
                if i != ord(currentRange[-1]) + 1:
                    output += recordRange()
                    currentRange = ""

            currentRange += char

        if len(currentRange) > 0:
            output += recordRange()

        return output

    def to_fsm(self, alphabet=None):
        if alphabet is None:
            alphabet = self.alphabet()

        # 0 is initial, 1 is final

        # If negated, make a singular FSM accepting any other characters
        if self.negated:
            map = {
                0: dict([(symbol, 1) for symbol in alphabet - self.chars]),
            }

        # If normal, make a singular FSM accepting only these characters
        else:
            map = {
                0: dict([(symbol, 1) for symbol in self.chars]),
            }

        return Fsm(
            alphabet=alphabet,
            states={0, 1},
            initial=0,
            finals={1},
            map=map,
        )

    def __repr__(self):
        string = ""
        if self.negated is True:
            string += "~"
        string += "Charclass("
        string += repr("".join(
            str(char) for char in sorted(self.chars, key=str)
        ))
        string += ")"
        return string

    def reduce(self):
        # `Charclass`es cannot be reduced.
        return self

    def alphabet(self):
        return {ANYTHING_ELSE} | self.chars

    def empty(self):
        return len(self.chars) == 0 and not self.negated

    # set operations
    def negate(self):
        '''
            Negate the current `Charclass`. e.g. [ab] becomes [^ab]. Call
            using "charclass2 = ~charclass1"
        '''
        return Charclass(self.chars, negated=not self.negated)

    def __invert__(self):
        return self.negate()

    def reversed(self):
        return self

    def __or__(self, other):
        # ¬A OR ¬B = ¬(A AND B)
        # ¬A OR B = ¬(A - B)
        # A OR ¬B = ¬(B - A)
        # A OR B
        if self.negated:
            if other.negated:
                return ~Charclass(self.chars & other.chars)
            else:
                return ~Charclass(self.chars - other.chars)
        else:
            if other.negated:
                return ~Charclass(other.chars - self.chars)
            else:
                return Charclass(self.chars | other.chars)


# Standard character classes
WORDCHAR = Charclass(
    "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz"
)
DIGIT = Charclass("0123456789")
SPACECHAR = Charclass("\t\n\v\f\r ")

# This `Charclass` expresses "no possibilities at all"
# and can never match anything.
NULLCHARCLASS = Charclass("")

NONWORDCHAR = ~WORDCHAR
NONDIGITCHAR = ~DIGIT
NONSPACECHAR = ~SPACECHAR
DOT = ~NULLCHARCLASS

# Textual representations of standard character classes
shorthand = {
    WORDCHAR: "\\w",
    DIGIT: "\\d",
    SPACECHAR: "\\s",
    NONWORDCHAR: "\\W",
    NONDIGITCHAR: "\\D",
    NONSPACECHAR: "\\S",
    DOT: ".",
}

# Characters which users may escape in a regex instead of inserting them
# literally. In ASCII order:
escapes = {
    "\t": "\\t",  # tab
    "\n": "\\n",  # line feed
    "\v": "\\v",  # vertical tab
    "\f": "\\f",  # form feed
    "\r": "\\r",  # carriage return
}

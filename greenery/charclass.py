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
from typing import ClassVar, Iterable, Mapping

from .fsm import ANYTHING_ELSE, AnythingElse, Fsm


@dataclass(frozen=True, init=False)
class Charclass:
    """
    A `Charclass` is basically a `frozenset` of symbols.
    A `Charclass` with the `negated` flag set is assumed
    to contain every symbol that is in the alphabet of all symbols but not
    explicitly listed inside the frozenset. e.g. [^a]. This is very handy
    if the full alphabet is extremely large, but also requires dedicated
    combination functions.
    """

    chars: frozenset[str]
    negated: bool

    def __init__(self, chars: Iterable[str] = (), negated: bool = False):
        chars = frozenset(chars)
        # chars should consist only of chars
        for c in chars:
            if not isinstance(c, str):
                raise TypeError(f"Can't put {c!r} in a `Charclass`", c)
            if len(c) != 1:
                raise ValueError("`Charclass` can only contain single chars", c)

        object.__setattr__(self, "chars", chars)
        object.__setattr__(self, "negated", negated)

    def __eq__(self, other: object, /) -> bool:
        return (
            isinstance(other, Charclass)
            and self.chars == other.chars
            and self.negated == other.negated
        )

    def __hash__(self, /) -> int:
        return hash((self.chars, self.negated))

    # These are the characters carrying special meanings when they appear
    # "outdoors" within a regular expression. To be interpreted literally, they
    # must be escaped with a backslash.
    allSpecial: ClassVar[frozenset[str]] = frozenset("\\[]|().?*+{}")

    # These are the characters carrying special meanings when they appear
    # INSIDE a character class (delimited by square brackets) within a regular
    # expression. To be interpreted literally, they must be escaped with a
    # backslash. Notice how much smaller this class is than the one above; note
    # also that the hyphen and caret do NOT appear above.
    classSpecial: ClassVar[frozenset[str]] = frozenset("\\[]^-")

    # Shorthand codes for use inside `Charclass`es e.g. [abc\d]
    w: ClassVar[frozenset[str]] = frozenset(
        "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz"
    )
    d: ClassVar[frozenset[str]] = frozenset("0123456789")
    s: ClassVar[frozenset[str]] = frozenset("\t\n\v\f\r ")

    shorthand: ClassVar[Mapping[frozenset[str], str]] = {
        w: "\\w",
        d: "\\d",
        s: "\\s",
    }

    negated_shorthand: ClassVar[Mapping[frozenset[str], str]] = {
        w: "\\W",
        d: "\\D",
        s: "\\S",
    }

    def __str__(self, /) -> str:
        # pylint: disable=too-many-return-statements

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
            if 0 <= ord(char) <= 0x1F or ord(char) == 0x7F:
                return "\\x" + "{0:02x}".format(ord(char))

            return char

        # multiple characters (or possibly 0 characters)
        return "[" + self.escape() + "]"

    def escape(self, /) -> str:
        def escapeChar(char: str, /) -> str:
            if char in Charclass.classSpecial:
                return "\\" + char
            if char in escapes.keys():
                return escapes[char]

            # If char is an ASCII control character, don't print it directly,
            # return a hex escape sequence e.g. "\\x00". Note that this
            # includes tab and other characters already handled above
            if 0 <= ord(char) <= 0x1F or ord(char) == 0x7F:
                return "\\x" + "{0:02x}".format(ord(char))

            return char

        def recordRange() -> str:
            # there's no point in putting a range when the whole thing is
            # 3 characters or fewer. "abc" -> "abc" but "abcd" -> "a-d"
            strs = [
                # "ab" or "abc" or "abcd"
                "".join(escapeChar(char) for char in currentRange),
                # "a-b" or "a-c" or "a-d"
                (escapeChar(currentRange[0]) + "-" + escapeChar(currentRange[-1])),
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

    def to_fsm(
        self,
        /,
        alphabet: Iterable[str | AnythingElse] | None = None,
    ) -> Fsm:
        alphabet = self.alphabet() if alphabet is None else frozenset(alphabet)

        map: dict[int | str | None, dict[str | AnythingElse, int | str | None]]

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
            alphabet=set(alphabet),
            states={0, 1},
            initial=0,
            finals={1},
            map=map,
        )

    def __repr__(self, /) -> str:
        string = ""
        if self.negated:
            string += "~"
        string += "Charclass("
        string += repr("".join(str(char) for char in sorted(self.chars, key=str)))
        string += ")"
        return string

    def reduce(self, /) -> Charclass:
        # `Charclass`es cannot be reduced.
        return self

    def alphabet(self, /) -> frozenset[str | AnythingElse]:
        return self.chars | {ANYTHING_ELSE}

    def empty(self, /) -> bool:
        return len(self.chars) == 0 and not self.negated

    # set operations
    def negate(self, /) -> Charclass:
        """
        Negate the current `Charclass`. e.g. [ab] becomes [^ab]. Call
        using "charclass2 = ~charclass1"
        """
        return Charclass(self.chars, negated=not self.negated)

    def __invert__(self, /) -> Charclass:
        return self.negate()

    def reversed(self, /) -> Charclass:
        return self

    def union(*predicates: Charclass) -> Charclass:
        closed_sets = [cc.chars for cc in predicates if not cc.negated]
        include = frozenset.union(*closed_sets) if closed_sets else frozenset()

        open_sets = [cc.chars for cc in predicates if cc.negated]
        exclude = frozenset.intersection(*open_sets) if open_sets else frozenset()

        is_open = bool(open_sets)
        chars = (exclude - include) if is_open else (include - exclude)

        return Charclass(chars, negated=is_open)

    __or__ = union

    def intersection(*predicates: Charclass) -> Charclass:
        # ¬A AND ¬B = ¬(A OR B)
        # ¬A AND B = B - A
        # A AND ¬B = A - B
        # A AND B
        return ~Charclass.union(*(~cc for cc in predicates))

    __and__ = intersection


# Standard character classes
WORDCHAR = Charclass("0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz")
DIGIT = Charclass("0123456789")
SPACECHAR = Charclass("\t\n\v\f\r ")

# This `Charclass` expresses "no possibilities at all"
# and can never match anything.
NULLCHARCLASS = Charclass()

NONWORDCHAR = ~WORDCHAR
NONDIGITCHAR = ~DIGIT
NONSPACECHAR = ~SPACECHAR
DOT = ~NULLCHARCLASS

# Textual representations of standard character classes
shorthand: Mapping[Charclass, str] = {
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
escapes: Mapping[str, str] = {
    "\t": "\\t",  # tab
    "\n": "\\n",  # line feed
    "\v": "\\v",  # vertical tab
    "\f": "\\f",  # form feed
    "\r": "\\r",  # carriage return
}

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
    "repartition",
)

from dataclasses import dataclass
from typing import ClassVar, Iterable, Mapping


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

    def __lt__(self, other):
        if self.negated < other.negated:
            return True
        if self.negated == other.negated and \
            min(ord(char) for char in self.chars) < min(ord(char) for char in other.chars):
            return True
        return False

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
        if self in shorthand:
            return shorthand[self]

        # e.g. [^a]
        if self.negated:
            return f"[^{self.escape()}]"

        # single character, not contained inside square brackets.
        if len(self.chars) == 1:
            # Python lacks the Axiom of Choice
            char = "".join(self.chars)

            # e.g. if char is "\t", return "\\t"
            if char in escapes:
                return escapes[char]

            if char in Charclass.allSpecial:
                return f"\\{char}"

            # If char is an ASCII control character, don't print it directly,
            # return a hex escape sequence e.g. "\\x00". Note that this
            # includes tab and other characters already handled above
            if 0 <= ord(char) <= 0x1F or ord(char) == 0x7F:
                return f"\\x{ord(char):02x}"

            return char

        # multiple characters (or possibly 0 characters)
        return f"[{self.escape()}]"

    def escape(self, /) -> str:
        def escape_char(char: str, /) -> str:
            if char in Charclass.classSpecial:
                return f"\\{char}"
            if char in escapes:
                return escapes[char]

            # If char is an ASCII control character, don't print it directly,
            # return a hex escape sequence e.g. "\\x00". Note that this
            # includes tab and other characters already handled above
            if 0 <= ord(char) <= 0x1F or ord(char) == 0x7F:
                return f"\\x{ord(char):02x}"

            return char

        def record_range() -> str:
            # there's no point in putting a range when the whole thing is
            # 3 characters or fewer. "abc" -> "abc" but "abcd" -> "a-d"
            strs = [
                # "ab" or "abc" or "abcd"
                "".join(escape_char(c) for c in current_range),
                # "a-b" or "a-c" or "a-d"
                (escape_char(current_range[0]) + "-" + escape_char(current_range[-1])),
            ]
            return sorted(strs, key=len)[0]

        output = ""

        # use shorthand for known character ranges
        # note the nested processing order. DO NOT process \d before processing
        # \w. if more character class constants arise which do not nest nicely,
        # a problem will arise because there is no clear ordering to use...

        # look for ranges
        current_range = ""
        for char in sorted(self.chars, key=ord):
            # range is not empty: new char must fit after previous one
            if current_range:
                i = ord(char)

                # char doesn't fit old range: restart
                if i != ord(current_range[-1]) + 1:
                    output += record_range()
                    current_range = ""

            current_range += char

        if current_range:
            output += record_range()

        return output

    def __repr__(self, /) -> str:
        sign = "~" if self.negated else ""
        chars = "".join(sorted(self.chars))
        return f"{sign}Charclass({chars!r})"

    def reduce(self, /) -> Charclass:
        # `Charclass`es cannot be reduced.
        return self

    def alphabet(self):
        return self.chars

    def empty(self, /) -> bool:
        return not self.chars and not self.negated

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

def repartition(charclasses):
    """
    Accept an iterable of `Charclass`es which may overlap somewhat.
    Construct a minimal collection of `Charclass`es which partition the space
    of all possible characters and can be combined to create all of the
    originals.
    Return a map from each original `Charclass` to its constituent pieces.
    """
    alphabet = set()
    for charclass in charclasses:
        for char in charclass.chars:
            alphabet.add(char)

    # Group all of the possible characters by "signature".
    # A signature is a tuple of Booleans telling us which character classes
    # a particular character is mentioned in.
    signatures = {}
    for char in sorted(alphabet):
        signature = tuple(char in charclass.chars for charclass in charclasses)
        if signature not in signatures:
            signatures[signature] = []
        signatures[signature].append(char)

    newcharclasses = list(map(lambda chars: Charclass(chars), signatures.values()))

    # And one last thing
    newcharclasses.append(~Charclass(alphabet))

    # Now compute the breakdowns
    partition = {}
    for charclass in charclasses:
        partition[charclass] = []
        for newcharclass in newcharclasses:
            if newcharclass & charclass == newcharclass:
                partition[charclass].append(newcharclass)

    return partition

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
from typing import ClassVar, Dict, Iterable, List, Mapping, Tuple


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

    ranges: Tuple[Tuple[str, str], ...]
    negated: bool

    def __init__(self, ranges: Tuple[Tuple[str, str]] = (), negated: bool = False):
        if not isinstance(ranges, tuple):
            raise TypeError(f"Bad ranges: {ranges!r}")
        for range in ranges:
            if range[0] != range[1]:
                raise Exception('Bad range')
            for char in range:
                if not isinstance(char, str):
                    raise TypeError(f"Can't put {char!r} in a `Charclass`", char)
                if len(char) != 1:
                    raise ValueError("`Charclass` can only contain single chars", char)

        object.__setattr__(self, "ranges", tuple(sorted(ranges)))
        object.__setattr__(self, "negated", negated)

    def __lt__(self, other: Charclass, /) -> bool:
        if self.negated < other.negated:
            return True
        if self.negated == other.negated:
            self_min = min(ord(range[0]) for range in self.ranges)
            other_min = min(ord(range[0]) for range in other.ranges)
            if self_min < other_min:
                return True
        return False

    def __eq__(self, other: object, /) -> bool:
        return (
            isinstance(other, Charclass)
            and self.ranges == other.ranges
            and self.negated == other.negated
        )

    def __hash__(self, /) -> int:
        return hash((self.ranges, self.negated))

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
        if len(self.ranges) == 1 and self.ranges[0][0] == self.ranges[0][1]:
            char = self.ranges[0][0]

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
        for char in sorted((range[0] for range in self.ranges), key=ord):
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

    def num_chars(self, /) -> int:
        num = 0
        for range in self.ranges:
            num += ord(range[1]) - ord(range[0]) + 1
        return num

    def has_char(self, char, /) -> Boolean:
        for range in self.ranges:
            if range[0] <= char <= range[1]:
                return True
        return False

    def __repr__(self, /) -> str:
        sign = "~" if self.negated else ""
        return f"{sign}Charclass({self.ranges!r})"

    def reduce(self, /) -> Charclass:
        # `Charclass`es cannot be reduced.
        return self

    def empty(self, /) -> bool:
        return not self.ranges and not self.negated

    # set operations
    def negate(self, /) -> Charclass:
        """
        Negate the current `Charclass`. e.g. [ab] becomes [^ab]. Call
        using "charclass2 = ~charclass1"
        """
        return Charclass(self.ranges, negated=not self.negated)

    def __invert__(self, /) -> Charclass:
        return self.negate()

    def reversed(self, /) -> Charclass:
        return self

    def union(*predicates: Charclass) -> Charclass:
        # TODO
        closed_sets = [frozenset({range[0] for range in cc.ranges}) for cc in predicates if not cc.negated]
        include = frozenset.union(*closed_sets) if closed_sets else frozenset()

        open_sets = [frozenset({range[0] for range in cc.ranges}) for cc in predicates if cc.negated]
        exclude = frozenset.intersection(*open_sets) if open_sets else frozenset()

        is_open = bool(open_sets)
        chars = (exclude - include) if is_open else (include - exclude)

        return Charclass(tuple((char, char) for char in chars), negated=is_open)

    __or__ = union

    def intersection(*predicates: Charclass) -> Charclass:
        # ¬A AND ¬B = ¬(A OR B)
        # ¬A AND B = B - A
        # A AND ¬B = A - B
        # A AND B
        return ~Charclass.union(*(~cc for cc in predicates))

    __and__ = intersection


# Standard character classes
WORDCHAR = Charclass(
    tuple((c, c) for c in "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz")
)
DIGIT = Charclass(
    tuple((c, c) for c in "0123456789")
)
SPACECHAR = Charclass(
    tuple((c, c) for c in "\t\n\v\f\r ")
)

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


def repartition(
    charclasses: Iterable[Charclass],
) -> Mapping[Charclass, Iterable[Charclass]]:
    """
    Accept an iterable of `Charclass`es which may overlap somewhat.
    Construct a minimal collection of `Charclass`es which partition the space
    of all possible characters and can be combined to create all of the
    originals.
    Return a map from each original `Charclass` to its constituent pieces.
    """
    alphabet = set()
    for charclass in charclasses:
        for range in charclass.ranges:
            alphabet.add(range[0])

    # Group all of the possible characters by "signature".
    # A signature is a tuple of Booleans telling us which character classes
    # a particular character is mentioned in.
    signatures: Dict[Tuple[bool, ...], List[str]] = {}
    for char in sorted(alphabet):
        signature = tuple(
            char in [range[0] for range in charclass.ranges]
            for charclass in charclasses
        )
        if signature not in signatures:
            signatures[signature] = []
        signatures[signature].append(char)

    newcharclasses = [Charclass(
        tuple((char, char) for char in chars)
    ) for chars in signatures.values()]

    # And one last thing
    newcharclasses.append(~Charclass((
        tuple((char, char) for char in alphabet)
    )))

    # Now compute the breakdowns
    partition: Dict[Charclass, List[Charclass]] = {}
    for charclass in charclasses:
        partition[charclass] = []
        for newcharclass in newcharclasses:
            if newcharclass & charclass == newcharclass:
                partition[charclass].append(newcharclass)

    return partition

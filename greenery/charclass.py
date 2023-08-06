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
from typing import ClassVar, Dict, Iterable, Iterator, List, Mapping, Tuple

NUM_UNICODE_CHARS = (1 << 20) + (1 << 16)


def negate(ord_ranges: List[Tuple[int, int]]) -> List[Tuple[int, int]]:
    u = 0
    negated = []
    for ord_range in ord_ranges:
        if u < ord_range[0]:
            negated.append((u, ord_range[0] - 1))
        u = ord_range[1] + 1
    if u < NUM_UNICODE_CHARS - 1:
        negated.append((u, NUM_UNICODE_CHARS - 1))
    return negated


def add_ord_range(
    ord_ranges: List[Tuple[int, int]], new_ord_range: Tuple[int, int]
) -> List[Tuple[int, int]]:
    """
    Assume all existing ord ranges are sorted, and also disjoint
    So no cases of [[12, 17], [2, 3]] or [[4, 6], [7, 8]].
    Potentially some performance enhancement is possible here, stop
    cloning `ord_ranges` over and over?
    """
    # All ranges before this index
    # fit strictly before the newcomer
    start = 0

    # All ranges with this index or larger
    # fit strictly after the newcomer
    end = len(ord_ranges)

    for i, ord_range in enumerate(ord_ranges):
        if ord_range[1] + 1 < new_ord_range[0]:
            start = i + 1
        if new_ord_range[1] + 1 < ord_range[0]:
            end = i
            break

    # Ranges between those indices will be spliced out and replaced.
    if start < end:
        new_ord_range = (
            min(new_ord_range[0], ord_ranges[start][0]),
            max(new_ord_range[1], ord_ranges[end - 1][1]),
        )
    return ord_ranges[:start] + [new_ord_range] + ord_ranges[end:]


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

    ord_ranges: List[Tuple[int, int]]
    negated: bool

    def __init__(
        self, ranges: str | Tuple[Tuple[str, str], ...] = "", negated: bool = False
    ):
        if isinstance(ranges, str):
            ranges = tuple((char, char) for char in ranges)
        if not isinstance(ranges, tuple):
            raise TypeError(f"Bad ranges: {ranges!r}")
        for r in ranges:
            if len(r) != 2 or r[0] > r[1]:
                raise ValueError(f"Bad range: {r!r}")
            for char in r:
                if not isinstance(char, str):
                    raise TypeError(f"Can't put {char!r} in a `Charclass`", char)
                if len(char) != 1:
                    raise ValueError("`Charclass` can only contain single chars", char)

        # Rebalance ranges!
        ord_ranges: List[Tuple[int, int]] = []
        for first, last in ranges:
            ord_ranges = add_ord_range(ord_ranges, (ord(first), ord(last)))

        object.__setattr__(self, "ord_ranges", tuple(ord_ranges))
        object.__setattr__(self, "negated", negated)

    def __lt__(self, other: Charclass, /) -> bool:
        if self.negated < other.negated:
            return True
        if (
            self.negated == other.negated
            and self.ord_ranges[0][0] < other.ord_ranges[0][0]
        ):
            return True
        return False

    def __eq__(self, other: object, /) -> bool:
        return (
            isinstance(other, Charclass)
            and self.ord_ranges == other.ord_ranges
            and self.negated == other.negated
        )

    def __hash__(self, /) -> int:
        return hash((self.ord_ranges, self.negated))

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

    def __str__(self, /) -> str:
        # pylint: disable=too-many-return-statements

        # e.g. \w
        if self in shorthand:
            return shorthand[self]

        # e.g. [^a]
        if self.negated:
            return f"[^{self.escape()}]"

        # single character, not contained inside square brackets.
        if len(self.ord_ranges) == 1 and self.ord_ranges[0][0] == self.ord_ranges[0][1]:
            u = self.ord_ranges[0][0]
            char = chr(u)

            # e.g. if char is "\t", return "\\t"
            if char in escapes:
                return escapes[char]

            if char in Charclass.allSpecial:
                return f"\\{char}"

            # If char is an ASCII control character, don't print it directly,
            # return a hex escape sequence e.g. "\\x00". Note that this
            # includes tab and other characters already handled above
            if 0 <= u <= 0x1F or u == 0x7F:
                return f"\\x{u:02x}"

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

        output = ""

        for first_u, last_u in self.ord_ranges:
            # there's no point in putting a range when the whole thing is
            # 3 characters or fewer. "abc" -> "abc" but "abcd" -> "a-d"
            if last_u <= first_u + 2:
                # "a" or "ab" or "abc" or "abcd"
                for u in range(first_u, last_u + 1):
                    output += escape_char(chr(u))
            else:
                # "a-b" or "a-c" or "a-d"
                output += escape_char(chr(first_u)) + "-" + escape_char(chr(last_u))

        return output

    def get_chars(self, /) -> Iterator[str]:
        for first_u, last_u in self.ord_ranges:
            for u in range(first_u, last_u + 1):
                yield chr(u)

    def num_chars(self, /) -> int:
        num = 0
        for first_u, last_u in self.ord_ranges:
            num += last_u + 1 - first_u
        return NUM_UNICODE_CHARS - num if self.negated else num

    def accepts(self, char: str, /) -> bool:
        u = ord(char)
        for first_u, last_u in self.ord_ranges:
            if first_u <= u <= last_u:
                return not self.negated
        return self.negated

    def __repr__(self, /) -> str:
        sign = "~" if self.negated else ""
        ranges = tuple(
            (chr(first_u), chr(last_u)) for (first_u, last_u) in self.ord_ranges
        )
        return f"{sign}Charclass({ranges!r})"

    def reduce(self, /) -> Charclass:
        # `Charclass`es cannot be reduced.
        return self

    def empty(self, /) -> bool:
        return not self.ord_ranges and not self.negated

    # set operations
    def negate(self, /) -> Charclass:
        """
        Negate the current `Charclass`. e.g. [ab] becomes [^ab]. Call
        using "charclass2 = ~charclass1"
        """
        ranges = tuple(
            (chr(first_u), chr(last_u)) for (first_u, last_u) in self.ord_ranges
        )
        return Charclass(ranges, negated=not self.negated)

    def __invert__(self, /) -> Charclass:
        return self.negate()

    def reversed(self, /) -> Charclass:
        return self

    def issubset(self, other: Charclass, /) -> bool:
        # TODO: performance
        if self.negated:
            if other.negated:
                return not any(self.accepts(char) for char in other.get_chars())
            # Technically this isn't completely impossible but B would have
            # to be gigantic. TODO: maybe ditch the negation dealie?
            return False
        return all(other.accepts(char) for char in self.get_chars())

    def __or__(self, other: Charclass, /) -> Charclass:
        self_ord_ranges = list(self.ord_ranges)
        if self.negated:
            self_ord_ranges = negate(self_ord_ranges)

        other_ord_ranges = list(other.ord_ranges)
        if other.negated:
            other_ord_ranges = negate(other_ord_ranges)

        new_ord_ranges = self_ord_ranges
        for ord_range in other_ord_ranges:
            new_ord_ranges = add_ord_range(new_ord_ranges, ord_range)

        new_negated = self.negated or other.negated
        if new_negated:
            new_ord_ranges = negate(new_ord_ranges)
        new_ranges = tuple(
            (chr(first_u), chr(last_u)) for (first_u, last_u) in new_ord_ranges
        )
        return Charclass(new_ranges, new_negated)


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


def repartition(
    charclasses: Iterable[Charclass],
) -> Mapping[Charclass, Iterable[Charclass]]:
    """
    Accept an iterable of `Charclass`es which may overlap somewhat.
    Construct a minimal collection of `Charclass`es which partition the space
    of all possible characters and can be combined to create all of the
    originals.
    Return a map from each original `Charclass` to its constituent pieces.
    TODO: performance improvements in the case where there are >1000000
    possible characters in the alphabet?
    """
    alphabet = set()
    for charclass in charclasses:
        for first_u, last_u in charclass.ord_ranges:
            for u in range(first_u, last_u + 1):
                alphabet.add(chr(u))

    # Group all of the possible characters by "signature".
    # A signature is a tuple of Booleans telling us which character classes
    # a particular character is mentioned in.
    signatures: Dict[Tuple[bool, ...], List[str]] = {}
    for char in sorted(alphabet):
        signature = tuple(charclass.accepts(char) for charclass in charclasses)
        if signature not in signatures:
            signatures[signature] = []
        signatures[signature].append(char)

    newcharclasses = [
        Charclass(tuple((char, char) for char in chars))
        for chars in signatures.values()
    ]

    # And one last thing
    newcharclasses.append(~Charclass((tuple((char, char) for char in alphabet))))

    # Now compute the breakdowns
    partition: Dict[Charclass, List[Charclass]] = {}
    for charclass in charclasses:
        partition[charclass] = []
        for newcharclass in newcharclasses:
            if newcharclass.issubset(charclass):
                partition[charclass].append(newcharclass)

    return partition

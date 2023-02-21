from __future__ import annotations

__all__ = (
    "parse",
    "NoMatch",
)

from .bound import INF, Bound
from .charclass import Charclass, escapes, shorthand
from .multiplier import Multiplier, symbolic
from .rxelems import Conc, Mult, Pattern


class NoMatch(Exception):
    '''
        Thrown when parsing fails.
        Almost always caught and almost never fatal
    '''
    pass


def read_until(string: str, i: int, stop_char: str) -> tuple[int, str]:
    start = i
    while True:
        if i >= len(string):
            raise NoMatch
        if string[i] == stop_char:
            break
        i += 1
    return i + 1, string[start:i]


def static(string, i, static):
    j = i + len(static)
    if string[i:j] == static:
        return j
    raise NoMatch


def select_static(string, i, *statics):
    for st in statics:
        j = i + len(st)
        if string[i:j] == st:
            return j, st
    raise NoMatch


def unescape_hex(string, i):
    '''Turn e.g. "\\x40" into "@". Exactly two hex digits'''
    hex_digits = "0123456789AaBbCcDdEeFf"

    j = static(string, i, "\\x")

    hex1 = string[j]  # e.g. "4"
    if hex1 not in hex_digits:
        raise NoMatch
    j += len(hex1)

    hex2 = string[j]  # e.g. "0"
    if hex2 not in hex_digits:
        raise NoMatch
    j += len(hex2)

    codepoint = int(hex1 + hex2, 16)  # e.g. 64
    char = chr(codepoint)  # "@"
    return char, j


def match_internal_char(string, i):
    # e.g. if we see "\\t", return "\t"
    for key in escapes.keys():
        try:
            return key, static(string, i, escapes[key])
        except NoMatch:
            pass

    # special chars e.g. "\\-" returns "-"
    for char in Charclass.classSpecial:
        try:
            return char, static(string, i, "\\" + char)
        except NoMatch:
            pass

    # hex escape e.g. "\\x40" returns "@"
    try:
        return unescape_hex(string, i)
    except NoMatch:
        pass

    # single non-special character, not contained
    # inside square brackets
    char, j = string[i], i+1
    if char in Charclass.classSpecial:
        raise NoMatch

    return char, j


def match_class_interior_1(string, i):
    # Attempt 1: shorthand e.g. "\w"
    for chars, cc_shorthand in Charclass.shorthand.items():
        try:
            return chars, False, static(string, i, cc_shorthand)
        except NoMatch:
            pass

    # Attempt 1B: shorthand e.g. "\W"
    for chars, cc_shorthand in Charclass.negated_shorthand.items():
        try:
            return chars, True, static(string, i, cc_shorthand)
        except NoMatch:
            pass

    # Attempt 2: a range e.g. "d-h"
    try:
        first, j = match_internal_char(string, i)  # `first` is "d"
        k = static(string, j, "-")
        last, k = match_internal_char(string, k)  # `last` is "h"

        firstIndex = ord(first)  # 100
        lastIndex = ord(last)  # 104

        # Be strict here, "d-d" is not allowed
        if firstIndex >= lastIndex:
            raise NoMatch(f"Range '{first}' to '{last}' not allowed")

        chars = frozenset(chr(i) for i in range(firstIndex, lastIndex + 1))
        return chars, False, k
    except NoMatch:
        pass

    # Attempt 3: just a character on its own
    (char, j) = match_internal_char(string, i)
    return frozenset(char), False, j


def match_class_interior(string, i):
    predicates = []
    try:
        while True:
            # Match an internal character, range, or other charclass predicate.
            internal, internal_negated, i = match_class_interior_1(string, i)
            predicates.append((internal, internal_negated))
    except NoMatch:
        pass

    closed_sets = [chars for chars, negated in predicates if not negated]
    include = frozenset.union(*closed_sets) if closed_sets else frozenset()

    open_sets = [chars for chars, negated in predicates if negated]
    exclude = frozenset.intersection(*open_sets) if open_sets else frozenset()

    is_open = bool(open_sets)
    chars = (exclude - include) if is_open else (include - exclude)

    return chars, is_open, i


def match_charclass(string: str, i):
    if i >= len(string):
        raise NoMatch

    # wildcard ".", "\\w", "\\d", etc.
    for key in shorthand.keys():
        try:
            return key, static(string, i, shorthand[key])
        except NoMatch:
            pass

    # "[^dsgsdg]"
    try:
        j = static(string, i, "[^")
        chars, negated, j = match_class_interior(string, j)
        j = static(string, j, "]")
        return Charclass(chars, not negated), j
    except NoMatch:
        pass

    # "[sdfsf]"
    try:
        j = static(string, i, "[")
        chars, negated, j = match_class_interior(string, j)
        j = static(string, j, "]")
        return Charclass(chars, negated), j
    except NoMatch:
        pass

    # e.g. if seeing "\\t", return "\t"
    for key in escapes.keys():
        try:
            return Charclass(key), static(string, i, escapes[key])
        except NoMatch:
            pass

    # e.g. if seeing "\\{", return "{"
    for char in Charclass.allSpecial:
        try:
            return Charclass(char), static(string, i, "\\" + char)
        except NoMatch:
            pass

    # e.g. if seeing "\\x40", return "@"
    try:
        char, j = unescape_hex(string, i)
        return Charclass(char), j
    except NoMatch:
        pass

    # single non-special character, not contained inside square brackets
    char, i = string[i], i+1
    if char in Charclass.allSpecial:
        raise NoMatch

    return Charclass(char), i


def match_multiplicand(string, i):
    # explicitly non-capturing "(?:...)" syntax. No special significance
    try:
        j = static(string, i, "(?")
        j, st = select_static(string, j, ':', 'P<')
        if st == 'P<':
            j, group_name = read_until(string, j, '>')
        pattern, j = match_pattern(string, j)
        j = static(string, j, ")")
        return pattern, j
    except NoMatch:
        pass

    # normal "(...)" syntax
    try:
        j = static(string, i, "(")
        pattern, j = match_pattern(string, j)
        j = static(string, j, ")")
        return pattern, j
    except NoMatch:
        pass

    # Just a `Charclass` on its own
    charclass, j = match_charclass(string, i)
    return charclass, j


def match_any_of(string, i, collection):
    for char in collection:
        try:
            return char, static(string, i, char)
        except NoMatch:
            pass
    raise NoMatch


def match_bound(string: str, i):
    # "0"
    try:
        return Bound(0), static(string, i, "0")
    except NoMatch:
        pass

    # "1", etc.
    try:
        digit, j = match_any_of(string, i, "123456789")
        integer = int(digit)
        try:
            while True:
                digit, j = match_any_of(string, j, "0123456789")
                integer *= 10
                integer += int(digit)
        except NoMatch:
            return Bound(integer), j
    except NoMatch:
        pass

    # "" empty string = infinite bound as in "{4,}"
    return INF, i


def match_multiplier(string: str, i):
    # {2,3} or {2,}
    try:
        j = static(string, i, "{")
        min, j = match_bound(string, j)
        j = static(string, j, ",")
        max, j = match_bound(string, j)
        j = static(string, j, "}")
        return Multiplier(min, max), j
    except NoMatch:
        pass

    # {2}
    try:
        j = static(string, i, "{")
        min, j = match_bound(string, j)
        j = static(string, j, "}")
        return Multiplier(min, min), j
    except NoMatch:
        pass

    # "?"/"*"/"+"/""
    # we do these in reverse order of symbol length, because
    # that forces "" to be done last
    for key in sorted(symbolic, key=lambda key: -len(symbolic[key])):
        try:
            return key, static(string, i, symbolic[key])
        except NoMatch:
            pass

    raise NoMatch


def match_mult(string: str, i):
    multiplicand, j = match_multiplicand(string, i)
    multiplier, j = match_multiplier(string, j)
    return Mult(multiplicand, multiplier), j


def match_conc(string: str, i):
    mults = list()
    try:
        while True:
            m, i = match_mult(string, i)
            mults.append(m)
    except NoMatch:
        pass
    return Conc(*mults), i


def match_pattern(string: str, i):
    concs = list()

    # first one
    c, i = match_conc(string, i)
    concs.append(c)

    # the rest
    while True:
        try:
            i = static(string, i, "|")
            c, i = match_conc(string, i)
            concs.append(c)
        except NoMatch:
            return Pattern(*concs), i


def parse(string: str):
    '''
        Parse a full string and return a `Pattern` object. Fail if
        the whole string wasn't parsed
    '''
    obj, i = match_pattern(string, 0)
    if i != len(string):
        raise Exception(
            f"Could not parse '{string}' beyond index {str(i)}"
        )
    return obj

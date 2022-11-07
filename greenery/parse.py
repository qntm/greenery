# -*- coding: utf-8 -*-

from typing import Tuple

from .bound import Bound, INF
from .charclass import Charclass, shorthand, escapes
from .multiplier import Multiplier, symbolic
from .rxelems import Pattern, Conc, Mult

class nomatch(Exception):
    '''Thrown when parsing fails. Almost always caught and almost never fatal'''
    pass

def read_until(string: str, i: int, stop_char: str) -> Tuple[int, str]:
    start = i
    while True:
        if i >= len(string):
            raise nomatch
        if string[i] == stop_char:
            break
        i += 1
    return i + 1, string[start:i]

def static(string, i, static):
    j = i + len(static)
    if string[i:j] == static:
        return j
    raise nomatch

def select_static(string, i, *statics):
    for st in statics:
        j = i+len(st)
        if string[i:j] == st:
            return j, st
    raise nomatch

# Turn e.g. "\\x40" into "@". Exactly two hex digits
def unescape_hex(string, i):
    hex_digits = "0123456789AaBbCcDdEeFf"

    j = static(string, i, "\\x")

    hex1 = string[j] # e.g. "4"
    if not hex1 in hex_digits:
        raise nomatch
    j += len(hex1)

    hex2 = string[j] # e.g. "0"
    if not hex2 in hex_digits:
        raise nomatch
    j += len(hex2)

    codepoint = int(hex1 + hex2, 16) # e.g. 64
    char = chr(codepoint) # "@"
    return char, j

def match_internal_char(string, i):
    # e.g. if we see "\\t", return "\t"
    for key in escapes.keys():
        try:
            return key, static(string, i, escapes[key])
        except nomatch:
            pass

    # special chars e.g. "\\-" returns "-"
    for char in Charclass.classSpecial:
        try:
            return char, static(string, i, "\\" + char)
        except nomatch:
            pass

    # hex escape e.g. "\\x40" returns "@"
    try:
        return unescape_hex(string, i)
    except nomatch:
        pass

    # single non-special character, not contained
    # inside square brackets
    char, j = string[i], i+1
    if char in Charclass.classSpecial:
        raise nomatch

    return char, j

def match_class_interior_1(string, i):
    # Attempt 1: shorthand e.g. "\w"
    for key in Charclass.shorthand:
        try:
            return key, static(string, i, Charclass.shorthand[key])
        except nomatch:
            pass

    # Attempt 2: a range e.g. "d-h"
    try:
        first, j = match_internal_char(string, i) # `first` is "d"
        k = static(string, j, "-")
        last, k = match_internal_char(string, k) # `last` is "h"

        firstIndex = ord(first) # 100
        lastIndex = ord(last) # 104

        # Be strict here, "d-d" is not allowed
        if firstIndex >= lastIndex:
            raise nomatch("Range '" + first + "' to '" + last + "' not allowed")

        chars = "".join([
            chr(i) for i in range(firstIndex, lastIndex + 1)
        ])
        return chars, k
    except nomatch:
        pass

    # Attempt 3: just a character on its own
    return match_internal_char(string, i)

def match_class_interior(string, i):
    internals = ""
    try:
        while True:
            internal, i = match_class_interior_1(string, i)
            internals += internal
    except nomatch:
        pass
    return internals, i

def match_charclass(string: str, i = 0):
    if i >= len(string):
        raise nomatch

    # wildcard ".", "\\w", "\\d", etc.
    for key in shorthand.keys():
        try:
            return key, static(string, i, shorthand[key])
        except nomatch:
            pass

    # "[^dsgsdg]"
    try:
        j = static(string, i, "[^")
        chars, j = match_class_interior(string, j)
        j = static(string, j, "]")
        return ~Charclass(chars), j
    except nomatch:
        pass

    # "[sdfsf]"
    try:
        j = static(string, i, "[")
        chars, j = match_class_interior(string, j)
        j = static(string, j, "]")
        return Charclass(chars), j
    except nomatch:
        pass

    # e.g. if seeing "\\t", return "\t"
    for key in escapes.keys():
        try:
            return Charclass(key), static(string, i, escapes[key])
        except nomatch:
            pass

    # e.g. if seeing "\\{", return "{"
    for char in Charclass.allSpecial:
        try:
            return Charclass(char), static(string, i, "\\" + char)
        except nomatch:
            pass

    # e.g. if seeing "\\x40", return "@"
    try:
        char, j = unescape_hex(string, i)
        return Charclass(char), j
    except nomatch:
        pass

    # single non-special character, not contained inside square brackets
    char, i = string[i], i+1
    if char in Charclass.allSpecial:
        raise nomatch

    return Charclass(char), i

def match_multiplicand(string, i):
    # explicitly non-capturing "(?:...)" syntax. No special significance
    try:
        j = static(string, i, "(?")
        j, st = select_static(string, j, ':', 'P<')
        if st == 'P<':
            j, group_name = read_until(string, j, '>')
        multiplicand, j = match_pattern(string, j)
        j = static(string, j, ")")
        return multiplicand, j
    except nomatch:
        pass

    # normal "(...)" syntax
    try:
        j = static(string, i, "(")
        multiplicand, j = match_pattern(string, j)
        j = static(string, j, ")")
        return multiplicand, j
    except nomatch:
        pass

    # Just a `Charclass` on its own
    return match_charclass(string, i)

def match_any_of(string, i, collection):
    for char in collection:
        try:
            return char, static(string, i, char)
        except nomatch:
            pass
    raise nomatch

def match_bound(string: str, i = 0):
    # "0"
    try:
        return Bound(0), static(string, i, "0")
    except nomatch:
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
        except nomatch:
            return Bound(integer), j
    except nomatch:
        pass

    # "" empty string = infinite bound as in "{4,}"
    return INF, i

def match_multiplier(string: str, i = 0):
    # {2,3} or {2,}
    try:
        j = static(string, i, "{")
        min, j = match_bound(string, j)
        j = static(string, j, ",")
        max, j = match_bound(string, j)
        j = static(string, j, "}")
        return Multiplier(min, max), j
    except nomatch:
        pass

    # {2}
    try:
        j = static(string, i, "{")
        min, j = match_bound(string, j)
        j = static(string, j, "}")
        return Multiplier(min, min), j
    except nomatch:
        pass

    # "?"/"*"/"+"/""
    # we do these in reverse order of symbol length, because
    # that forces "" to be done last
    for key in sorted(symbolic, key=lambda key: -len(symbolic[key])):
        try:
            return key, static(string, i, symbolic[key])
        except nomatch:
            pass

    raise nomatch

def match_mult(string: str, i = 0):
    multiplicand, j = match_multiplicand(string, i)
    multiplier, j = match_multiplier(string, j)
    return Mult(multiplicand, multiplier), j

def match_conc(string: str, i = 0):
    mults = list()
    try:
        while True:
            m, i = match_mult(string, i)
            mults.append(m)
    except nomatch:
        pass
    return Conc(*mults), i

def match_pattern(string: str, i = 0):
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
        except nomatch:
            return Pattern(*concs), i

def parse(string: str):
    '''
        Parse a full string and return a `Pattern` object. Fail if
        the whole string wasn't parsed
    '''
    obj, i = match_pattern(string, 0)
    if i != len(string):
        raise Exception("Could not parse '" + string + "' beyond index " + str(i))
    return obj

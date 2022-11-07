# -*- coding: utf-8 -*-

from greenery.lego import inf, \
    charclass, bound, multiplier, mult, conc, pattern
from greenery.rxelems.charclass import shorthand, escapes
from .rxelems.multiplier import symbolic
from typing import Tuple

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
def unescapeHex(string, i):
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

def matchInternalChar(string, i):
    # e.g. if we see "\\t", return "\t"
    for key in escapes.keys():
        try:
            return key, static(string, i, escapes[key])
        except nomatch:
            pass

    # special chars e.g. "\\-" returns "-"
    for char in charclass.classSpecial:
        try:
            return char, static(string, i, "\\" + char)
        except nomatch:
            pass

    # hex escape e.g. "\\x40" returns "@"
    try:
        return unescapeHex(string, i)
    except nomatch:
        pass

    # single non-special character, not contained
    # inside square brackets
    char, j = string[i], i+1
    if char in charclass.classSpecial:
        raise nomatch

    return char, j

def matchClassInterior1(string, i):
    # Attempt 1: shorthand e.g. "\w"
    for key in charclass.shorthand:
        try:
            return key, static(string, i, charclass.shorthand[key])
        except nomatch:
            pass

    # Attempt 2: a range e.g. "d-h"
    try:
        first, j = matchInternalChar(string, i) # `first` is "d"
        k = static(string, j, "-")
        last, k = matchInternalChar(string, k) # `last` is "h"

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
    return matchInternalChar(string, i)

def matchClassInterior(string, i):
    internals = ""
    try:
        while True:
            internal, i = matchClassInterior1(string, i)
            internals += internal
    except nomatch:
        pass
    return internals, i

def matchCharclass(string: str, i = 0):
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
        chars, j = matchClassInterior(string, j)
        j = static(string, j, "]")
        return ~charclass(chars), j
    except nomatch:
        pass

    # "[sdfsf]"
    try:
        j = static(string, i, "[")
        chars, j = matchClassInterior(string, j)
        j = static(string, j, "]")
        return charclass(chars), j
    except nomatch:
        pass

    # e.g. if seeing "\\t", return "\t"
    for key in escapes.keys():
        try:
            return charclass(key), static(string, i, escapes[key])
        except nomatch:
            pass

    # e.g. if seeing "\\{", return "{"
    for char in charclass.allSpecial:
        try:
            return charclass(char), static(string, i, "\\" + char)
        except nomatch:
            pass

    # e.g. if seeing "\\x40", return "@"
    try:
        char, j = unescapeHex(string, i)
        return charclass(char), j
    except nomatch:
        pass

    # single non-special character, not contained inside square brackets
    char, i = string[i], i+1
    if char in charclass.allSpecial:
        raise nomatch

    return charclass(char), i

def matchMultiplicand(string, i):
    # explicitly non-capturing "(?:...)" syntax. No special significance
    try:
        j = static(string, i, "(?")
        j, st = select_static(string, j, ':', 'P<')
        if st == 'P<':
            j, group_name = read_until(string, j, '>')
        multiplicand, j = matchPattern(string, j)
        j = static(string, j, ")")
        return multiplicand, j
    except nomatch:
        pass

    # normal "(...)" syntax
    try:
        j = static(string, i, "(")
        multiplicand, j = matchPattern(string, j)
        j = static(string, j, ")")
        return multiplicand, j
    except nomatch:
        pass

    # Just a charclass on its own
    return matchCharclass(string, i)

def matchAnyOf(string, i, collection):
    for char in collection:
        try:
            return char, static(string, i, char)
        except nomatch:
            pass
    raise nomatch

def matchBound(string: str, i = 0):
    # "0"
    try:
        return bound(0), static(string, i, "0")
    except nomatch:
        pass

    # "1", etc.
    try:
        digit, j = matchAnyOf(string, i, "123456789")
        integer = int(digit)
        try:
            while True:
                digit, j = matchAnyOf(string, j, "0123456789")
                integer *= 10
                integer += int(digit)
        except nomatch:
            return bound(integer), j
    except nomatch:
        pass

    # "" empty string = infinite bound as in "{4,}"
    return inf, i

def matchMultiplier(string: str, i = 0):
    # {2,3} or {2,}
    try:
        j = static(string, i, "{")
        min, j = matchBound(string, j)
        j = static(string, j, ",")
        max, j = matchBound(string, j)
        j = static(string, j, "}")
        return multiplier(min, max), j
    except nomatch:
        pass

    # {2}
    try:
        j = static(string, i, "{")
        min, j = matchBound(string, j)
        j = static(string, j, "}")
        return multiplier(min, min), j
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

def matchMult(string: str, i = 0):
    multiplicand, j = matchMultiplicand(string, i)
    multiplier_, j = matchMultiplier(string, j)
    return mult(multiplicand, multiplier_), j

def matchConc(string: str, i = 0):
    mults = list()
    try:
        while True:
            m, i = matchMult(string, i)
            mults.append(m)
    except nomatch:
        pass
    return conc(*mults), i

def matchPattern(string: str, i = 0):
    concs = list()

    # first one
    c, i = matchConc(string, i)
    concs.append(c)

    # the rest
    while True:
        try:
            i = static(string, i, "|")
            c, i = matchConc(string, i)
            concs.append(c)
        except nomatch:
            return pattern(*concs), i

def parse(string: str):
    '''
        Parse a full string and return a pattern object. Fail if
        the whole string wasn't parsed
    '''
    obj, i = matchPattern(string, 0)
    if i != len(string):
        raise Exception("Could not parse '" + string + "' beyond index " + str(i))
    return obj

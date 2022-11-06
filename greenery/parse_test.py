# -*- coding: utf-8 -*-

if __name__ == "__main__":
    raise Exception("Test files can't be run directly. Use `python -m pytest greenery`")

from greenery.lego import conc, mult, charclass, pattern, d, multiplier, bound, dot, inf
from .rxelems.multiplier import one, star, plus, qm
from greenery.parse import nomatch, matchCharclass, parse, matchMult

def test_charclass_matching():
    assert matchCharclass("a", 0) == (charclass("a"), 1)
    assert matchCharclass("aa", 1) == (charclass("a"), 2)
    assert matchCharclass("a$", 1) == (charclass("$"), 2)
    assert matchCharclass(".", 0) == (dot, 1)
    try:
        matchCharclass("[", 0)
        assert False
    except IndexError:
        pass
    try:
        matchCharclass("a", 1)
        assert False
    except nomatch:
        pass

def test_mult_matching():
    assert matchMult("abcde[^fg]*", 5) == (
        mult(~charclass("fg"), star),
        11
    )
    assert matchMult("abcde[^fg]*h{5}[a-z]+", 11) == (
        mult(charclass("h"), multiplier(bound(5), bound(5))),
        15
    )
    assert matchMult("abcde[^fg]*h{5}[a-z]+T{1,}", 15) == (
        mult(charclass("abcdefghijklmnopqrstuvwxyz"), plus),
        21
    )
    assert matchMult("abcde[^fg]*h{5}[a-z]+T{2,}", 21) == (
        mult(charclass("T"), multiplier(bound(2), inf)),
        26
    )

def test_charclass_ranges():
    # Should accept arbitrary ranges of characters in charclasses. No longer
    # limited to alphanumerics. (User beware...)
    assert parse("[z{|}~]") == parse("[z-~]")
    assert parse("[\\w:;<=>?@\\[\\\\\\]\\^`]") == parse("[0-z]")

def test_hex_escapes():
    # Should be able to parse e.g. "\\x40"
    assert parse("\\x00") == parse("\x00")
    assert parse("\\x40") == parse("@")
    assert parse("[\\x40]") == parse("[@]")
    assert parse("[\\x41-\\x5a]") == parse("[A-Z]")

def test_w_d_s():
    # Allow "\w", "\d" and "\s" in charclasses
    assert parse("\\w") == parse("[0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz]")
    assert parse("[\\w~]") == parse("[0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz~]")
    assert parse("[\\da]") == parse("[0123456789a]")
    assert parse("[\\s]") == parse("[\t\n\r\f\v ]")

def test_mult_parsing():
    assert parse("[a-g]+") == pattern(conc(mult(charclass("abcdefg"), plus)))
    assert parse("[a-g0-8$%]+") == pattern(conc(mult(charclass("abcdefg012345678$%"), plus)))
    assert parse("[a-g0-8$%\\^]+") == pattern(conc(mult(charclass("abcdefg012345678$%^"), plus)))

def test_conc_parsing():
    assert parse("abcde[^fg]*h{5}[a-z]+") == pattern(
        conc(
            mult(charclass("a"), one),
            mult(charclass("b"), one),
            mult(charclass("c"), one),
            mult(charclass("d"), one),
            mult(charclass("e"), one),
            mult(~charclass("fg"), star),
            mult(charclass("h"), multiplier(bound(5), bound(5))),
            mult(charclass("abcdefghijklmnopqrstuvwxyz"), plus),
        )
    )
    assert parse("[bc]*[ab]*") == pattern(
        conc(
            mult(charclass("bc"), star),
            mult(charclass("ab"), star),
        )
    )
    assert parse("abc...") == pattern(
        conc(
            mult(charclass("a"), one),
            mult(charclass("b"), one),
            mult(charclass("c"), one),
            mult(dot, one),
            mult(dot, one),
            mult(dot, one),
        )
    )
    assert parse("\\d{4}-\\d{2}-\\d{2}") == pattern(
        conc(
            mult(charclass("0123456789"), multiplier(bound(4), bound(4))),
            mult(charclass("-"), one),
            mult(charclass("0123456789"), multiplier(bound(2), bound(2))),
            mult(charclass("-"), one),
            mult(charclass("0123456789"), multiplier(bound(2), bound(2))),
        )
    )

def test_pattern_parsing():
    assert parse("abc|def(ghi|jkl)") == pattern(
        conc(
            mult(charclass("a"), one),
            mult(charclass("b"), one),
            mult(charclass("c"), one),
        ),
        conc(
            mult(charclass("d"), one),
            mult(charclass("e"), one),
            mult(charclass("f"), one),
            mult(
                pattern(
                    conc(
                        mult(charclass("g"), one),
                        mult(charclass("h"), one),
                        mult(charclass("i"), one),
                    ),
                    conc(
                        mult(charclass("j"), one),
                        mult(charclass("k"), one),
                        mult(charclass("l"), one),
                    ),
                ), one
            ),
        )
    )

    # Accept the "non-capturing group" syntax, "(?: ... )" but give it no
    # special significance
    assert parse("(?:)") == parse("()")
    assert parse("(?:abc|def)") == parse("(abc|def)")
    parse("(:abc)") # should give no problems

    # Named groups
    assert parse("(?P<ng1>abc)") == parse("(abc)")

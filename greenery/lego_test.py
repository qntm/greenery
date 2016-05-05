# -*- coding: utf-8 -*-

if __name__ == "__main__":
	raise Exception("Test files can't be run directly. Use `python -m pytest greenery`")

from greenery.lego import conc, mult, charclass, one, emptystring, star, plus, nothing, pattern, qm, d, multiplier, bound, w, s, W, D, S, dot, nomatch, inf, zero, parse, from_fsm
from greenery import fsm

# In general the idea for unit tests is that every unit test relies only on
# functionality which has already been unit-tested. If this isn't possible, then
# additional tests are required!

################################################################################
# Equality tests. No point in comparing different lego pieces in tests unless
# this part works

def test_charclass_equality():
	assert charclass("a") == charclass("a")
	assert ~charclass("a") == ~charclass("a")
	assert ~charclass("a") != charclass("a")
	assert charclass("ab") == charclass("ba")

def test_mult_equality():
	assert mult(charclass("a"), one) == mult(charclass("a"), one)
	assert mult(charclass("a"), one) != mult(charclass("b"), one)
	assert mult(charclass("a"), one) != mult(charclass("a"), qm)
	assert mult(charclass("a"), one) != mult(charclass("a"), multiplier(bound(1), bound(2)))
	assert mult(charclass("a"), one) != charclass("a")

def test_conc_equality():
	assert conc(mult(charclass("a"), one)) == conc(mult(charclass("a"), one))
	assert conc(mult(charclass("a"), one)) != conc(mult(charclass("b"), one))
	assert conc(mult(charclass("a"), one)) != conc(mult(charclass("a"), qm))
	assert conc(mult(charclass("a"), one)) != conc(mult(charclass("a"), multiplier(bound(1), bound(2))))
	assert conc(mult(charclass("a"), one)) != emptystring

def test_pattern_equality():
	assert pattern(
		conc(mult(charclass("a"), one)),
		conc(mult(charclass("b"), one)),
	) == pattern(
		conc(mult(charclass("b"), one)),
		conc(mult(charclass("a"), one)),
	)
	assert pattern(
		conc(mult(charclass("a"), one)),
		conc(mult(charclass("a"), one)),
	) == pattern(
		conc(mult(charclass("a"), one)),
	)

################################################################################
# Parsing tests. Absolutely no cleverness is applied at parsing time, we just
# return the exact object which was just parsed. Call reduce() if you wish...

def test_charclass_parsing():
	assert charclass.match("a", 0) == (charclass("a"), 1)
	assert charclass.parse("a") == charclass("a")
	assert charclass.match("aa", 1) == (charclass("a"), 2)
	assert charclass.match("a$", 1) == (charclass("$"), 2)
	assert charclass.match(".", 0) == (dot, 1)
	try:
		charclass.match("[", 0)
		assert False
	except IndexError:
		pass
	try:
		charclass.match("a", 1)
		assert False
	except nomatch:
		pass

def test_charclass_ranges():
	# Should accept arbitrary ranges of characters in charclasses. No longer
	# limited to alphanumerics. (User beware...)
	assert parse("[z{|}~]") == parse("[z-~]")
	assert parse("[\w:;<=>?@\\[\\\\\]\\^`]") == parse("[0-z]")

def test_hex_escapes():
	# Should be able to parse e.g. "\\x40"
	assert parse("\\x00") == parse("\x00")
	assert parse("\\x40") == parse("@")
	assert parse("[\\x40]") == parse("[@]")
	assert parse("[\\x41-\\x5a]") == parse("[A-Z]")

def test_w_d_s():
	# Allow "\w", "\d" and "\s" in charclasses
	assert charclass.parse("\w") == charclass.parse("[0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz]")
	assert charclass.parse("[\w~]") == charclass.parse("[0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz~]")
	assert charclass.parse("[\da]") == charclass.parse("[0123456789a]")
	assert charclass.parse("[\s]") == charclass.parse("[\t\n\r\f\v ]")

def test_mult_parsing():
	assert mult.parse("[a-g]+") == mult(charclass("abcdefg"), plus)
	assert mult.parse("[a-g0-8$%]+") == mult(charclass("abcdefg012345678$%"), plus)
	assert mult.parse("[a-g0-8$%\\^]+") == mult(charclass("abcdefg012345678$%^"), plus)
	assert mult.match("abcde[^fg]*", 5) == (
		mult(~charclass("fg"), star),
		11
	)
	assert mult.match("abcde[^fg]*h{5}[a-z]+", 11) == (
		mult(charclass("h"), multiplier(bound(5), bound(5))),
		15
	)
	assert mult.match("abcde[^fg]*h{5}[a-z]+T{1,}", 15) == (
		mult(charclass("abcdefghijklmnopqrstuvwxyz"), plus),
		21
	)
	assert mult.match("abcde[^fg]*h{5}[a-z]+T{2,}", 21) == (
		mult(charclass("T"), multiplier(bound(2), inf)),
		26
	)

def test_conc_parsing():
	assert conc.parse("abcde[^fg]*h{5}[a-z]+") == conc(
		mult(charclass("a"), one),
		mult(charclass("b"), one),
		mult(charclass("c"), one),
		mult(charclass("d"), one),
		mult(charclass("e"), one),
		mult(~charclass("fg"), star),
		mult(charclass("h"), multiplier(bound(5), bound(5))),
		mult(charclass("abcdefghijklmnopqrstuvwxyz"), plus),
	)
	assert conc.parse("[bc]*[ab]*") == conc(
		mult(charclass("bc"), star),
		mult(charclass("ab"), star),
	)
	assert conc.parse("abc...") == conc(
		mult(charclass("a"), one),
		mult(charclass("b"), one),
		mult(charclass("c"), one),
		mult(dot, one),
		mult(dot, one),
		mult(dot, one),
	)
	assert conc.parse("\\d{4}-\\d{2}-\\d{2}") == conc(
		mult(charclass("0123456789"), multiplier(bound(4), bound(4))),
		mult(charclass("-"), one),
		mult(charclass("0123456789"), multiplier(bound(2), bound(2))),
		mult(charclass("-"), one),
		mult(charclass("0123456789"), multiplier(bound(2), bound(2))),
	)

def test_pattern_parsing():
	assert pattern.parse("abc|def(ghi|jkl)") == pattern(
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
	assert pattern.parse("(?P<ng1>abc)") == parse("(abc)")

################################################################################
# repr() tests

def test_repr():
	assert repr(~charclass("a")) == "~charclass('a')"

################################################################################
# Stringification tests

def test_charclass_str():
	assert str(w) == "\\w"
	assert str(d) == "\\d"
	assert str(s) == "\\s"
	assert str(charclass("a")) == "a"
	assert str(charclass("{")) == "\\{"
	assert str(charclass("\t")) == "\\t"
	assert str(charclass("ab")) == "[ab]"
	assert str(charclass("a{")) == "[a{]"
	assert str(charclass("a\t")) == "[\\ta]"
	assert str(charclass("a-")) == "[\\-a]"
	assert str(charclass("a[")) == "[\\[a]"
	assert str(charclass("a]")) == "[\\]a]"
	assert str(charclass("ab")) == "[ab]"
	assert str(charclass("abc")) == "[abc]"
	assert str(charclass("abcd")) == "[a-d]"
	assert str(charclass("abcdfghi")) == "[a-df-i]"
	assert str(charclass("^")) == "^"
	assert str(charclass("\\")) == "\\\\"
	assert str(charclass("a^")) == "[\\^a]"
	assert str(charclass("0123456789a")) == "[0-9a]"
	assert str(charclass("\t\v\r A")) == "[\\t\\v\\r A]"
	assert str(charclass("\n\f A")) == "[\\n\\f A]"
	assert str(charclass("\t\n\v\f\r A")) == "[\\t-\\r A]"
	assert str(charclass("0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz|")) == "[0-9A-Z_a-z|]"
	assert str(W) == "\\W"
	assert str(D) == "\\D"
	assert str(S) == "\\S"
	assert str(dot) == "."
	assert str(~charclass("")) == "."
	assert str(~charclass("a")) == "[^a]"
	assert str(~charclass("{")) == "[^{]"
	assert str(~charclass("\t")) == "[^\\t]"
	assert str(~charclass("^")) == "[^\\^]"

	# Arbitrary ranges
	assert str(parse("[\w:;<=>?@\\[\\\\\]\\^`]")) == "[0-z]"
	# TODO: what if \d is a proper subset of `chars`?

	# escape sequences are not preserved
	assert str(parse("\\x09")) == "\\t"

	# Printing ASCII control characters? You should get hex escapes
	assert str(parse("\\x00")) == "\\x00"

def test_mult_str():
	assert str(bound(2)) == "2"
	assert str(inf) == ""
	assert str(multiplier(bound(2), inf)) == "{2,}"

	a = charclass("a")
	assert str(mult(a, one)) == "a"
	assert str(mult(a, multiplier(bound(2), bound(2)))) == "a{2}"
	assert str(mult(a, multiplier(bound(3), bound(3)))) == "a{3}"
	assert str(mult(a, multiplier(bound(4), bound(4)))) == "a{4}"
	assert str(mult(a, multiplier(bound(5), bound(5)))) == "a{5}"
	assert str(mult(a, qm)) == "a?"
	assert str(mult(a, star)) == "a*"
	assert str(mult(a, plus)) == "a+"
	assert str(mult(a, multiplier(bound(2), bound(5)))) == "a{2,5}"
	assert str(mult(a, multiplier(bound(2), inf))) == "a{2,}"
	assert str(mult(d, one)) == "\\d"
	assert str(mult(d, multiplier(bound(2), bound(2)))) == "\\d{2}"
	assert str(mult(d, multiplier(bound(3), bound(3)))) == "\\d{3}"

def test_conc_str():
	assert str(conc(
		mult(charclass("a"), one),
		mult(charclass("b"), one),
		mult(charclass("c"), one),
		mult(charclass("d"), one),
		mult(charclass("e"), one),
		mult(~charclass("fg"), star),
		mult(charclass("h"), multiplier(bound(5), bound(5))),
		mult(charclass("abcdefghijklmnopqrstuvwxyz"), plus),
	)) == "abcde[^fg]*h{5}[a-z]+"

def test_pattern_str():
	assert str(pattern(
		conc(mult(charclass("a"), one)),
		conc(mult(charclass("b"), one)),
	)) == "a|b"
	assert str(pattern(
		conc(mult(charclass("a"), one)),
		conc(mult(charclass("a"), one)),
	)) == "a"
	assert str(pattern(
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
		),
	)) == "abc|def(ghi|jkl)"

def test_parse_str_round_trip():
	assert str(parse("a.b")) == "a.b" # not "a[ab]b"
	assert str(parse("\\d{4}")) == "\\d{4}"
	assert str(parse("a.b()()")) == "a.b()()"

################################################################################
# Test to_fsm() and alphabet-related functionality

def test_alphabet():
	# lego.alphabet() should include `fsm.anything_else`
	assert parse("").alphabet() == {fsm.anything_else}

def test_charclass_fsm():
	# "[^a]"
	nota = (~charclass("a")).to_fsm()
	assert nota.alphabet == {"a", fsm.anything_else}
	assert nota.accepts("b")
	assert nota.accepts(["b"])
	assert nota.accepts([fsm.anything_else])

def test_pattern_fsm():
	# "a[^a]"
	anota = pattern.parse("a[^a]").to_fsm()
	assert len(anota.states) == 3
	assert not anota.accepts("a")
	assert not anota.accepts(["a"])
	assert not anota.accepts("b")
	assert not anota.accepts(["b"])
	assert not anota.accepts("aa")
	assert not anota.accepts(["a", "a"])
	assert anota.accepts("ab")
	assert anota.accepts(["a", "b"])
	assert anota.accepts(["a", fsm.anything_else])
	assert not anota.accepts("ba")
	assert not anota.accepts("bb")

	# "0\\d"
	zeroD = pattern.parse("0\\d").to_fsm(d.chars)
	assert zeroD.accepts("01")
	assert not zeroD.accepts("10")

	# "\\d{2}"
	d2 = pattern.parse("\\d{2}").to_fsm(d.chars)
	assert not d2.accepts("")
	assert not d2.accepts("1")
	assert d2.accepts("11")
	assert not d2.accepts("111")

	# abc|def(ghi|jkl)
	conventional = pattern.parse("abc|def(ghi|jkl)").to_fsm(w.chars)
	assert not conventional.accepts("a")
	assert not conventional.accepts("ab")
	assert conventional.accepts("abc")
	assert not conventional.accepts("abcj")
	assert conventional.accepts("defghi")
	assert conventional.accepts("defjkl")

def test_fsm():
	# You should be able to to_fsm() a single lego piece without supplying a specific
	# alphabet. That should be determinable from context.
	assert parse("a.b").to_fsm().accepts("acb")

	bad = parse("0{2}|1{2}").to_fsm({"0", "1", fsm.anything_else})
	assert bad.accepts("00")
	assert bad.accepts("11")
	assert not bad.accepts("01")

	bad = parse("0{2}|1{2}").to_fsm()
	assert bad.accepts("00")
	assert bad.accepts("11")
	assert not bad.accepts("01")

def test_odd_bug():
	# Odd bug with ([bc]*c)?[ab]*
	int5A = mult(charclass("bc"), star).to_fsm({"a", "b", "c", fsm.anything_else})
	assert int5A.accepts([])
	assert int5A.accepts("")
	int5B = mult(charclass("c"), one).to_fsm({"a", "b", "c", fsm.anything_else})
	assert int5B.accepts("c")
	assert int5B.accepts(["c"])
	int5C = int5A + int5B
	assert int5C.accepts("c")
	assert int5C.accepts(["c"])

def test_bug_28():
	# Starification is broken in FSMs
	assert not parse("(ab*)").to_fsm().star().accepts("bb")
	assert not parse("(ab*)*").to_fsm().accepts("bb")

################################################################################
# Test matches(). Quite sparse at the moment

def test_wildcards_in_charclasses():
	# Allow "\w", "\d" and "\s" in charclasses
	assert parse("[\w~]*").matches("a0~")
	assert parse("[\da]*").matches("0129a")
	assert parse("[\s]+").matches(" \t \t ")

def test_block_comment_regex():
	# I went through several incorrect regexes for C block comments. Here we show
	# why the first few attempts were incorrect
	a = parse("/\\*(([^*]|\\*+[^*/])*)\\*/")
	assert a.matches("/**/")
	assert not a.matches("/***/")
	assert not a.matches("/****/")

	b = parse("/\\*(([^*]|\\*[^/])*)\\*/")
	assert b.matches("/**/")
	assert not b.matches("/***/")
	assert b.matches("/****/")

	c = parse("/\\*(([^*]|\\*+[^*/])*)\\*+/")
	assert c.matches("/**/")
	assert c.matches("/***/")
	assert c.matches("/****/")

def test_named_groups():
	a = parse("(?P<ng1>abc)")
	assert a.matches("abc")

def test_in():
	assert "a" in parse("a")
	assert "abcdsasda" in parse("\\w{4,10}")
	assert "abc" in parse("abc|def(ghi|jkl)")

################################################################################
# Test string generators

def test_charclass_gen():
	gen = charclass("xyz").strings()
	assert next(gen) == "x"
	assert next(gen) == "y"
	assert next(gen) == "z"
	try:
		next(gen)
		assert False
	except StopIteration:
		assert True

def test_mult_gen():
	# One term
	gen = mult.parse("[ab]").strings()
	assert next(gen) == "a"
	assert next(gen) == "b"
	try:
		next(gen)
		assert False
	except StopIteration:
		assert True

	# No terms
	gen = mult.parse("[ab]{0}").strings()
	assert next(gen) == ""
	try:
		next(gen)
		assert False
	except StopIteration:
		assert True

	# Many terms
	gen = mult.parse("[ab]*").strings()
	assert next(gen) == ""
	assert next(gen) == "a"
	assert next(gen) == "b"
	assert next(gen) == "aa"
	assert next(gen) == "ab"
	assert next(gen) == "ba"
	assert next(gen) == "bb"
	assert next(gen) == "aaa"

def test_conc_generator():
	gen = conc.parse("[ab][cd]").strings()
	assert next(gen) == "ac"
	assert next(gen) == "ad"
	assert next(gen) == "bc"
	assert next(gen) == "bd"
	try:
		next(gen)
		assert False
	except StopIteration:
		assert True

def test_pattern_generator():
	gen = pattern.parse("[ab]|[cde]").strings()
	assert next(gen) == "a"
	assert next(gen) == "b"
	assert next(gen) == "c"
	assert next(gen) == "d"
	assert next(gen) == "e"
	try:
		next(gen)
		assert False
	except StopIteration:
		assert True

	# more complex
	gen = pattern.parse("abc|def(ghi|jkl)").strings()
	assert next(gen) == "abc"
	assert next(gen) == "defghi"
	assert next(gen) == "defjkl"

	gen = mult.parse("[0-9a-fA-F]{3,10}").strings()
	assert next(gen) == "000"
	assert next(gen) == "001"
	assert next(gen) == "002"

def test_infinite_generation():
	# Infinite generator, flummoxes both depth-first and breadth-first searches
	gen = parse("a*b*").strings()
	assert next(gen) == ""
	assert next(gen) == "a"
	assert next(gen) == "b"
	assert next(gen) == "aa"
	assert next(gen) == "ab"
	assert next(gen) == "bb"
	assert next(gen) == "aaa"
	assert next(gen) == "aab"
	assert next(gen) == "abb"
	assert next(gen) == "bbb"
	assert next(gen) == "aaaa"

def test_wildcard_generator():
	# Generator needs to handle wildcards as well. Wildcards come last.
	gen = parse("a.b").strings(otherchar="*")
	assert next(gen) == "aab"
	assert next(gen) == "abb"
	assert next(gen) == "a*b"
	try:
		next(gen)
		assert False
	except StopIteration:
		assert True

def test_forin():
	assert [s for s in parse("abc|def(ghi|jkl)")] == ["abc", "defghi", "defjkl"]

################################################################################
# Test cardinality() and len()

def test_cardinality():
	assert charclass.parse("[]").cardinality() == 0
	assert mult.parse("[]?").cardinality() == 1
	assert mult.parse("[]{0,6}").cardinality() == 1
	assert mult.parse("[ab]{3}").cardinality() == 8
	assert mult.parse("[ab]{2,3}").cardinality() == 12
	assert len(pattern.parse("abc|def(ghi|jkl)")) == 3
	try:
		len(pattern.parse(".*"))
		assert False
	except OverflowError:
		assert True

################################################################################

def test_copy():
	x = pattern.parse("abc|def(ghi|jkl)")
	assert x.copy() == x

################################################################################
# Test from_fsm()

def test_dot():
	assert str(from_fsm(parse("a.b").to_fsm())) == "a.b" # not "a[ab]b"

def test_abstar():
	# Buggggs.
	abstar = fsm.fsm(
		alphabet = {'a', fsm.anything_else, 'b'},
		states   = {0, 1},
		initial  = 0,
		finals   = {0},
		map      = {
			0: {'a': 0, fsm.anything_else: 1, 'b': 0},
			1: {'a': 1, fsm.anything_else: 1, 'b': 1}
		}
	)
	assert str(from_fsm(abstar)) == "[ab]*"

def test_adotb():
	adotb = fsm.fsm(
		alphabet = {'a', fsm.anything_else, 'b'},
		states   = {0, 1, 2, 3, 4},
		initial  = 0,
		finals   = {4},
		map      = {
			0: {'a': 2, fsm.anything_else: 1, 'b': 1},
			1: {'a': 1, fsm.anything_else: 1, 'b': 1},
			2: {'a': 3, fsm.anything_else: 3, 'b': 3},
			3: {'a': 1, fsm.anything_else: 1, 'b': 4},
			4: {'a': 1, fsm.anything_else: 1, 'b': 1}
		}
	)
	assert str(from_fsm(adotb)) == "a.b"

def test_lego_recursion_error():
	# Catch a recursion error
	assert str(from_fsm(fsm.fsm(
		alphabet = {"0", "1"},
		states   = {0, 1, 2, 3},
		initial  = 3,
		finals   = {1},
		map      = {
			0: {"0": 1, "1": 1},
			1: {"0": 2, "1": 2},
			2: {"0": 2, "1": 2},
			3: {"0": 0, "1": 2},
		}
	))) == "0[01]"

def test_even_star_bug():
	# Bug fix. This is a(a{2})* (i.e. accepts an odd number of "a" chars in a
	# row), but when from_fsm() is called, the result is "a+". Turned out to be
	# a fault in the lego.multiplier.__mul__() routine
	elesscomplex = fsm.fsm(
		alphabet = {"a"},
		states = {0, 1},
		initial = 0,
		finals = {1},
		map = {
			0 : {"a" : 1},
			1 : {"a" : 0},
		},
	)
	assert not elesscomplex.accepts("")
	assert elesscomplex.accepts("a")
	assert not elesscomplex.accepts("aa")
	assert elesscomplex.accepts("aaa")
	elesscomplex = from_fsm(elesscomplex)
	assert str(elesscomplex) in {"a(aa)*", "(aa)*a"}
	elesscomplex = elesscomplex.to_fsm()
	assert not elesscomplex.accepts("")
	assert elesscomplex.accepts("a")
	assert not elesscomplex.accepts("aa")
	assert elesscomplex.accepts("aaa")
	gen = elesscomplex.strings()
	assert next(gen) == ["a"]
	assert next(gen) == ["a", "a", "a"]
	assert next(gen) == ["a", "a", "a", "a", "a"]
	assert next(gen) == ["a", "a", "a", "a", "a", "a", "a"]

def test_binary_3():
	# Binary numbers divisible by 3.
	# Disallows the empty string
	# Allows "0" on its own, but not leading zeroes.
	div3 = from_fsm(fsm.fsm(
		alphabet = {"0", "1"},
		states = {"initial", "zero", 0, 1, 2, None},
		initial = "initial",
		finals = {"zero", 0},
		map = {
			"initial" : {"0" : "zero", "1" : 1   },
			"zero"    : {"0" : None  , "1" : None},
			0         : {"0" : 0     , "1" : 1   },
			1         : {"0" : 2     , "1" : 0   },
			2         : {"0" : 1     , "1" : 2   },
			None      : {"0" : None  , "1" : None},
		},
	))
	assert str(div3) == "0|1(01*0|10*1)*10*"
	gen = div3.strings()
	assert next(gen) == "0"
	assert next(gen) == "11"
	assert next(gen) == "110"
	assert next(gen) == "1001"
	assert next(gen) == "1100"

def test_base_N():
	# Machine accepts only numbers in selected base (e.g. 2, 10) that are
	# divisible by N (e.g. 3, 7).
	# "0" alone is acceptable, but leading zeroes (e.g. "00", "07") are not
	base = 2
	N = 3
	assert base <= 10
	divN = from_fsm(fsm.fsm(
		alphabet = set(str(i) for i in range(base)),
		states = set(range(N)) | {"initial", "zero", None},
		initial = "initial",
		finals = {"zero", 0},
		map = dict(
			[
				("initial", dict([(str(j), j              % N) for j in range(1, base)] + [("0", "zero")])),
				("zero"   , dict([(str(j), None              ) for j in range(   base)]                  )),
				(None     , dict([(str(j), None              ) for j in range(   base)]                  )),
			] + [
				(i        , dict([(str(j), (i * base + j) % N) for j in range(   base)]                  ))
				for i in range(N)
			]
		),
	))
	gen = divN.strings()
	a = next(gen)
	assert a == "0"
	for i in range(7):
		b = next(gen)
		assert int(a, base) + N == int(b, base)
		a = b

def test_bad_alphabet():
	# You can use anything you like in your FSM alphabet, but if you try to
	# convert it to a `lego` object then the only acceptable symbols are single
	# characters or `fsm.anything_else`.
	for bad_symbol in [None, (), 0, ("a",), "", "aa", "ab", True]:
		f = fsm.fsm(
			alphabet = {bad_symbol},
			states = {0},
			initial = 0,
			finals = set(),
			map = {
				0 : {bad_symbol : 0}
			},
		)
		try:
			from_fsm(f)
			assert False
		except AssertionError as e:
			raise Exception("Accepted bad symbol: " + repr(bad_symbol))
		except Exception as e:
			pass

def test_dead_default():
	blockquote = from_fsm(fsm.fsm(
		alphabet = {"/", "*", fsm.anything_else},
		states = {0, 1, 2, 3, 4},
		initial = 0,
		finals = {4},
		map = {
				0    : {"/" : 1},
				1    : {"*" : 2},
				2    : {"/" : 2, fsm.anything_else : 2, "*" : 3},
				3    : {"/" : 4, fsm.anything_else : 2, "*" : 3},
		}
	))

################################################################################
# charclass set operations

def test_charclass_negation():
	assert ~~charclass("a") == charclass("a")
	assert charclass("a") == ~~charclass("a")

def test_charclass_union():
	# [ab] u [bc] = [abc]
	assert charclass("ab") | charclass("bc") == charclass("abc")
	# [ab] u [^bc] = [^c]
	assert charclass("ab") | ~charclass("bc") == ~charclass("c")
	# [^a] u [bc] = [^a]
	assert ~charclass("ab") | charclass("bc") == ~charclass("a")
	# [^ab] u [^bc] = [^b]
	assert ~charclass("ab") | ~charclass("bc") == ~charclass("b")

def test_charclass_intersection():
	# [ab] n [bc] = [b]
	assert charclass("ab") & charclass("bc") == charclass("b")
	# [ab] n [^bc] = [a]
	assert charclass("ab") & ~charclass("bc") == charclass("a")
	# [^ab] n [bc] = [c]
	assert ~charclass("ab") & charclass("bc") == charclass("c")
	# [^ab] n [^bc] = [^abc]
	assert ~charclass("ab") & ~charclass("bc") == ~charclass("abc")

################################################################################
# Emptiness detection

def test_empty():
	assert nothing.empty()
	assert charclass().empty()
	assert not dot.empty()
	assert not mult.parse("a{0}").empty()
	assert mult.parse("[]").empty()
	assert not mult.parse("[]?").empty()
	assert conc.parse("a[]").empty()
	assert not conc.parse("a[]?").empty()
	assert pattern().empty()
	assert not pattern.parse("a{0}").empty()
	assert not pattern.parse("[]?").empty()

################################################################################
# Test everythingbut()

def test_everythingbut():
	# Regexes are usually gibberish but we make a few claims
	a = parse("a")
	notA = a.everythingbut().to_fsm()
	assert notA.accepts("")
	assert not notA.accepts("a")
	assert notA.accepts("aa")

	# everythingbut(), called twice, should take us back to where we started.
	beer = parse("beer")
	notBeer = beer.everythingbut()
	beer2 = notBeer.everythingbut()
	assert str(beer2) == "be{2}r"

	# ".*" becomes "[]" and vice versa under this call.
	everything = parse(".*")
	assert str(everything.everythingbut()) == str(nothing)
	assert str(nothing.everythingbut()) == str(everything)

def test_isinstance_bug():
	# Problem relating to isinstance(). The class "mult" was occurring as both
	# lego.mult and as __main__.mult and apparently these count as different
	# classes for some reason, so isinstance(m, mult) was returning false.
	starfree = (parse("").everythingbut() + parse("aa") + parse("").everythingbut()).everythingbut()

################################################################################

def test_equivalence():
	assert parse("aa*").equivalent(parse("a*a"))
	assert parse("([ab]*a|[bc]*c)?b*").equivalent(parse("b*(a[ab]*|c[bc]*)?"))

################################################################################
# Test reversed()

def test_regex_reversal():
	assert reversed(parse("b")) == parse("b")
	assert reversed(parse("e*")) == parse("e*")
	assert reversed(parse("bear")) == parse("raeb")
	assert reversed(parse("beer")) == parse("reeb")
	assert reversed(parse("abc|def|ghi")) == parse("cba|fed|ihg")
	assert reversed(parse("(abc)*d")) == parse("d(cba)*")

################################################################################
# Bound and multiplier operation tests

def test_bound():
	assert min(bound(0), inf) == bound(0)
	assert min(bound(1), inf) == bound(1)
	assert qm.mandatory == bound(0)
	assert qm.optional == bound(1)

def test_multiplier_common():
	assert zero.common(zero) == zero
	assert zero.common(qm  ) == zero
	assert zero.common(one ) == zero
	assert zero.common(star) == zero
	assert zero.common(plus) == zero
	assert qm  .common(zero) == zero
	assert qm  .common(qm  ) == qm
	assert qm  .common(one ) == zero
	assert qm  .common(star) == qm
	assert qm  .common(plus) == qm
	assert one .common(zero) == zero
	assert one .common(qm  ) == zero
	assert one .common(one ) == one
	assert one .common(star) == zero
	assert one .common(plus) == one
	assert star.common(zero) == zero
	assert star.common(qm  ) == qm
	assert star.common(one ) == zero
	assert star.common(star) == star
	assert star.common(plus) == star
	assert plus.common(zero) == zero
	assert plus.common(qm  ) == qm
	assert plus.common(one ) == one
	assert plus.common(star) == star
	assert plus.common(plus) == plus

def test_multiplier_subtraction():
	# a{3,4}, a{2,5} -> a{2,3} (with a{1,1}, a{0,2} left over)
	assert multiplier.parse("{3,4}").common(multiplier.parse("{2,5}")) == multiplier.parse("{2,3}")
	assert multiplier.parse("{3,4}") - multiplier.parse("{2,3}") == one
	assert multiplier.parse("{2,5}") - multiplier.parse("{2,3}") == multiplier.parse("{0,2}")

	# a{2,}, a{1,5} -> a{1,5} (with a{1,}, a{0,0} left over)
	assert multiplier.parse("{2,}").common(multiplier.parse("{1,5}")) == multiplier.parse("{1,5}")
	assert multiplier.parse("{2,}") - multiplier.parse("{1,5}") == plus
	assert multiplier.parse("{1,5}") - multiplier.parse("{1,5}") == zero

	# a{3,}, a{2,} -> a{2,} (with a, epsilon left over)
	assert multiplier.parse("{3,}").common(multiplier.parse("{2,}")) == multiplier.parse("{2,}")
	assert multiplier.parse("{3,}") - multiplier.parse("{2,}") == one
	assert multiplier.parse("{2,}") - multiplier.parse("{2,}") == zero

	# a{3,}, a{3,} -> a{3,} (with zero, zero left over)
	assert multiplier.parse("{3,}").common(multiplier.parse("{3,}")) == multiplier.parse("{3,}")
	assert multiplier.parse("{3,}") - multiplier.parse("{3,}") == zero

def test_multiplier_union():
	assert zero | zero == zero
	assert zero | qm   == qm
	assert zero | one  == qm
	assert zero | star == star
	assert zero | plus == star
	assert qm   | zero == qm
	assert qm   | qm   == qm
	assert qm   | one  == qm
	assert qm   | star == star
	assert qm   | plus == star
	assert one  | zero == qm
	assert one  | qm   == qm
	assert one  | one  == one
	assert one  | star == star
	assert one  | plus == plus
	assert star | zero == star
	assert star | qm   == star
	assert star | one  == star
	assert star | star == star
	assert star | plus == star
	assert plus | zero == star
	assert plus | qm   == star
	assert plus | one  == plus
	assert plus | star == star
	assert plus | plus == plus
	assert not zero.canunion(multiplier(bound(2), inf))
	assert not one.canunion(multiplier(bound(3), bound(4)))
	assert not multiplier(bound(8), inf).canunion(multiplier(bound(3), bound(4)))
	try:
		zero | multiplier(bound(7), bound(8))
		assert False
	except AssertionError:
		assert False
	except Exception:
		pass

################################################################################
# Tests for some more set operations

def test_set_ops():
	assert parse("[abcd]") - parse("a") == charclass.parse("[bcd]")
	assert parse("[abcd]") ^ parse("[cdef]") == charclass.parse("[abef]")

################################################################################
# Concatenation tests (+)

def test_concatenation():
	assert charclass.parse("a") + charclass.parse("b") == conc.parse("ab")
	assert charclass.parse("a") + mult.parse("b{0,8}") == conc.parse("ab{0,8}")
	assert charclass.parse("a") + conc.parse("bc") == conc.parse("abc")
	assert charclass.parse("a") + pattern.parse("b|cd") == conc.parse("a(b|cd)")
	assert mult.parse("b{0,8}") + charclass.parse("c") == conc.parse("b{0,8}c")
	assert mult.parse("a{3,4}") + mult.parse("b?") == conc.parse("a{3,4}b?")
	assert mult.parse("a{2}") + conc.parse("bc") == conc.parse("a{2}bc")
	assert mult.parse("a{2,3}") + pattern.parse("b|cd") == conc.parse("a{2,3}(b|cd)")
	assert conc.parse("ab") + charclass.parse("c") == conc.parse("abc")
	assert conc.parse("ab") + mult.parse("c*") == conc.parse("abc*")
	assert conc.parse("") + conc.parse("") == conc.parse("")
	assert conc.parse("ab") + conc.parse("cd") == conc.parse("abcd")
	assert conc.parse("za{2,3}") + pattern.parse("b|cd") == conc.parse("za{2,3}(b|cd)")
	assert pattern.parse("a|bd") + charclass.parse("c") == conc.parse("(a|bd)c")
	assert pattern.parse("b|cd") + mult.parse("a{2,3}") == conc.parse("(b|cd)a{2,3}")
	assert pattern.parse("b|cd") + conc.parse("za{2,3}") == conc.parse("(b|cd)za{2,3}")
	assert pattern.parse("a|bc") + pattern.parse("c|de") == conc.parse("(a|bc)(c|de)")

################################################################################
# Test methods for finding common parts of regular expressions.

def test_mult_common():
	assert mult.parse("a{3,4}").common(mult.parse("a{2,5}")) == mult.parse("a{2,3}")
	assert mult.parse("a{2,}").common(mult.parse("a{1,5}")) == mult.parse("a{1,5}")
	assert mult.parse("a{3,}").common(mult.parse("a{2,}")) == mult.parse("a{2,}")

def test_conc_common():
	assert conc.parse("AAZY").common(conc.parse("BBZY"), suffix=True) == conc.parse("ZY")
	assert conc.parse("CZ").common(conc.parse("CZ"), suffix=True) == conc.parse("CZ")
	assert conc.parse("CY").common(conc.parse("CZ"), suffix=True) == conc.parse("")
	assert conc.parse("AZ").common(conc.parse("BZ"), suffix=True) == conc.parse("Z")
	assert conc.parse("AZ*").common(conc.parse("BZ"), suffix=True) == conc.parse("")
	assert conc.parse("A").common(conc.parse("B"), suffix=True) == conc.parse("")

def test_pattern_commonconc():
	assert pattern.parse("aa|aa")._commonconc() == conc.parse("aa")
	assert pattern.parse("abc|aa")._commonconc() == conc.parse("a")
	assert pattern.parse("a|bc")._commonconc() == conc.parse("")
	assert pattern.parse("cf{1,2}|cf")._commonconc() == conc.parse("cf")
	assert pattern.parse("ZA|ZB|ZC")._commonconc() == conc.parse("Z")
	assert pattern.parse("Z+A|ZB|ZZC")._commonconc() == conc.parse("Z")
	assert pattern.parse("a{2}b|a+c")._commonconc() == conc.parse("a")

def test_pattern_commonconc_suffix():
	assert pattern.parse("a|bc")._commonconc(suffix=True) == conc.parse("")
	assert pattern.parse("aa|bca")._commonconc(suffix=True) == conc.parse("a")
	assert pattern.parse("xyza|abca|a")._commonconc(suffix=True) == conc.parse("a")
	assert pattern.parse("f{2,3}c|fc")._commonconc(suffix=True) == conc.parse("fc")
	assert pattern.parse("aa")._commonconc(suffix=True) == conc.parse("aa")

################################################################################
# Inverse concatenation tests: behead() and dock()

def test_mult_dock():
	assert mult.parse("a{4,5}").dock(mult.parse("a{3}")) == mult.parse("a{1,2}")

def test_conc_dock():
	assert conc.parse("AZ").dock(conc.parse("Z")) == conc.parse("A")
	assert conc.parse("ABXY+Z").dock(conc.parse("XY+Z")) == conc.parse("AB")
	assert conc.parse("ABXY+Z").behead(conc.parse("ABXY+")) == conc.parse("Z")
	assert conc.parse("A").dock(conc.parse("")) == conc.parse("A")

	try:
		conc.parse("X{2}Y+Z").behead(conc.parse("XY+"))
		assert False
	except AssertionError:
		assert False
	except Exception:
		pass

def test_pattern_dock():
	assert pattern.parse("a|bc").dock(conc.parse("")) == pattern.parse("a|bc")
	assert pattern.parse("aa|bca").dock(conc.parse("a")) == pattern.parse("a|bc")
	assert pattern.parse("xyza|abca|a").dock(conc.parse("a")) == pattern.parse("xyz|abc|")
	assert pattern.parse("f{2,3}c|fc").dock(conc.parse("fc")) == pattern.parse("f{1,2}|")
	assert pattern.parse("aa").dock(conc.parse("aa")) == pattern.parse("")

def test_pattern_beheading():
	assert pattern.parse("aa").behead(conc.parse("a")) == pattern.parse("a")
	assert pattern.parse("abc|aa").behead(conc.parse("a")) == pattern.parse("a|bc")
	assert pattern.parse("cf{1,2}|cf").behead(conc.parse("c")) == pattern.parse("f{1,2}|f")
	assert pattern.parse("aa|aa").behead(conc.parse("aa")) == pattern.parse("")
	assert pattern.parse("abc|aa").behead(conc.parse("a")) == pattern.parse("a|bc")
	assert pattern.parse("a|bc").behead(conc.parse("")) == pattern.parse("a|bc")
	assert pattern.parse("cf{1,2}|cf").behead(conc.parse("cf")) == pattern.parse("f?|")
	assert pattern.parse("ZA|ZB|ZC").behead(conc.parse("Z")) == pattern.parse("A|B|C")
	assert pattern.parse("Z+A|ZB|ZZC").behead(conc.parse("Z")) == pattern.parse("Z*A|B|ZC")
	assert pattern.parse("a{2}b|a+c").behead(conc.parse("a")) == pattern.parse("ab|a*c")

################################################################################
# Basic concatenation reduction tests

def test_reduce_concatenations():
	assert parse("aa").reduce() == mult.parse("a{2}")
	assert parse("bb").reduce() == mult.parse("b{2}")
	assert parse("b*b").reduce() == mult.parse("b+")
	assert parse("aa{2,}").reduce() == mult.parse("a{3,}")
	assert parse("a*a{2}").reduce() == mult.parse("a{2,}")
	assert parse("aa{0,8}").reduce() == mult.parse("a{1,9}")
	assert parse("b{0,8}b").reduce() == mult.parse("b{1,9}")
	assert parse("aab").reduce() == conc.parse("a{2}b")
	assert parse("abb").reduce() == conc.parse("ab{2}")
	assert parse("abb*").reduce() == conc.parse("ab+")
	assert parse("abbc").reduce() == conc.parse("ab{2}c")
	assert parse("a?ab").reduce() == conc.parse("a{1,2}b")
	assert parse("(ac{2}|bc+)c").reduce() == conc.parse("(ac|bc*)c{2}")
	assert parse("a(a{2}b|a+c)").reduce() == conc.parse("a{2}(ab|a*c)")
	assert parse("a{2,3}(a{2}b|a+c)").reduce() == conc.parse("a{3,4}(ab|a*c)")
	assert parse("(ba{2}|ca+)a{2,3}").reduce() == conc.parse("(ba|ca*)a{3,4}")
	assert parse("za{2,3}(a{2}b|a+c)").reduce() == conc.parse("za{3,4}(ab|a*c)")
	assert parse("(ba{2}|ca+)a{2,3}z").reduce() == conc.parse("(ba|ca*)a{3,4}z")
	assert parse("(a|bc)(a|bc)").reduce() == mult.parse("(a|bc){2}")

################################################################################
# Multiplication tests

def test_charclass_multiplication():
	assert charclass("a") * one == charclass("a")
	assert charclass("a") * multiplier.parse("{1,3}") == mult.parse("a{1,3}")
	assert charclass("a") * multiplier.parse("{4,}") == mult.parse("a{4,}")

def test_mult_multiplication():
	assert mult.parse("a{2,3}") * one == mult.parse("a{2,3}")
	assert mult.parse("a{2,3}") * multiplier.parse("{4,5}") == mult.parse("a{8,15}")
	assert mult.parse("a{2,}") * multiplier.parse("{2,}") == mult.parse("a{4,}")

def test_conc_multiplication():
	assert conc.parse("ab?") * qm == mult.parse("(ab?)?")

def test_pattern_multiplication():
	assert pattern.parse("ab?|ba?") * multiplier.parse("{2,3}") == mult.parse("(ab?|ba?){2,3}")
	# TODO
	#assert pattern.parse("(ab?|ba?)") * multiplier.parse("{2,3}") == mult.parse("(ab?|ba?){2,3}")

def test_even_star_bug():
	# Defect: (a{2})* should NOT reduce to a*
	a2 = mult.parse("a{2}")
	a2star = a2 * star
	assert a2star == mult.parse("(a{2})*")

################################################################################
# Test intersection (&)

def test_mult_intersection():
	assert mult.parse("a") & mult.parse("b?") == charclass()
	assert mult.parse("a") & mult.parse("b?") == nothing
	assert mult.parse("a") & mult.parse("a?") == charclass.parse("a")
	assert mult.parse("a{2}") & mult.parse("a{2,}") == mult.parse("a{2}")
	assert mult.parse("a") & mult.parse("b") == charclass.parse("[]")
	assert mult.parse("a") & mult.parse("a") == charclass.parse("a")
	assert mult.parse("a*") & mult.parse("a") == charclass.parse("a")
	assert mult.parse("a*") & mult.parse("b*") == conc.parse("")
	assert mult.parse("a*") & mult.parse("a+") == mult.parse("a+")
	assert mult.parse("a{2}") & mult.parse("a{4}") == charclass.parse("[]")
	assert mult.parse("a{3,}") & mult.parse("a{3,}") == mult.parse("a{3,}")

def test_parse_regex_intersection():
	assert str(parse("a*") & parse("b*")) == ""
	assert str(parse("a") & parse("b")) == "[]"
	assert str(parse("\\d") & parse(".")) == "\\d"
	assert str(parse("\\d{2}") & parse("0.")) == "0\\d"
	assert str(parse("\\d{2}") & parse("19.*")) == "19"
	assert str(parse("\\d{3}") & parse("19.*")) == "19\\d"
	assert str(parse("abc...") & parse("...def")) == "abcdef"
	assert str(parse("[bc]*[ab]*") & parse("[ab]*[bc]*")) in {"([ab]*a|[bc]*c)?b*", "b*(a[ab]*|c[bc]*)?"}
	assert str(parse("\\W*")) == "\\W*"
	assert str(parse("[a-g0-8$%\\^]+")) == "[$%0-8\\^a-g]+"
	assert str(parse("[^d]{2,8}")) == "[^d]{2,8}"
	assert str(parse("\\W*") & parse("[a-g0-8$%\\^]+")) == "[$%\\^]+"
	assert str(parse("[ab]{1,2}") & parse("[^a]{1,2}")) == "b{1,2}"
	assert str(parse("[ab]?") & parse("[^a]?")) == "b?"
	assert parse("a{0,2}").matches("")
	assert parse("[ab]{0,2}").matches("")
	assert parse("[^a]{0,2}").matches("")
	assert parse("b{0,2}").matches("")
	assert str(parse("[ab]{0,2}") & parse("[^a]{0,2}")) == "b{0,2}"
	assert str(parse("[ab]{0,4}") & parse("[^a]{0,4}")) == "b{0,4}"
	assert str(parse("[abc]{0,8}") & parse("[^a]{0,8}")) == "[bc]{0,8}"
	assert str(parse("[a-g0-8$%\\^]{0,8}") & parse("[^d]{0,8}")) == "[$%0-8\\^abcefg]{0,8}"
	assert str(parse("[a-g0-8$%\\^]+") & parse("[^d]{0,8}")) == "[$%0-8\\^abcefg]{1,8}"
	assert str(parse("[a-g0-8$%\\^]+") & parse("[^d]{2,8}")) == "[$%0-8\\^abcefg]{2,8}"
	assert str(parse("\\W*") & parse("[a-g0-8$%\\^]+") & parse("[^d]{2,8}")) == "[$%\\^]{2,8}"
	assert str(parse("\\d{4}-\\d{2}-\\d{2}") & parse("19.*")) == "19\\d{2}-\\d{2}-\\d{2}"

def test_complexify():
	# Complexify!
	gen = (parse("[bc]*[ab]*") & parse("[ab]*[bc]*")).strings()
	assert next(gen) == ""
	assert next(gen) == "a"
	assert next(gen) == "b"
	assert next(gen) == "c"
	assert next(gen) == "aa"
	assert next(gen) == "ab"
	# no "ac"
	assert next(gen) == "ba"
	assert next(gen) == "bb"
	assert next(gen) == "bc"
	# no "ca"
	assert next(gen) == "cb"
	assert next(gen) == "cc"
	assert next(gen) == "aaa"

def test_silly_reduction():
	# This one is horrendous and we have to jump through some hoops to get to
	# a sensible result. Probably not a good unit test actually.
	long = \
	"(aa|bb*aa)a*|((ab|bb*ab)|(aa|bb*aa)a*b)((ab|bb*ab)|(aa|bb*aa)a*b)*" + \
	"(aa|bb*aa)a*|((ab|bb*ab)|(aa|bb*aa)a*b)((ab|bb*ab)|(aa|bb*aa)a*b)*"
	long = parse(long)
	long = reversed(long.to_fsm())
	long = reversed(from_fsm(long))
	assert str(long) == "[ab]*a[ab]"
	short = "[ab]*a?b*|[ab]*b?a*"
	assert str(parse(".*") & parse(short)) == "[ab]*"

################################################################################
# reduce() tests

def test_mult_reduction_easy():
	assert mult.parse("a").reduce() == charclass.parse("a")
	assert mult.parse("a").reduce() == charclass("a")
	assert mult.parse("a?").reduce() == mult(charclass("a"), qm)
	assert mult.parse("a{0}").reduce() == emptystring
	assert mult.parse("[]").reduce() == nothing
	assert mult.parse("[]?").reduce() == emptystring
	assert mult.parse("[]{0}").reduce() == emptystring
	assert mult.parse("[]{0,5}").reduce() == emptystring
	assert mult(pattern(), one).reduce() == nothing
	assert mult(pattern(), qm).reduce() == emptystring
	assert mult(pattern(), zero).reduce() == emptystring
	assert mult(pattern(), multiplier.parse("{0,5}")).reduce() == emptystring

def test_conc_reduction_basic():
	assert conc.parse("a").reduce() == charclass.parse("a")
	assert conc.parse("a{3,4}").reduce() == mult.parse("a{3,4}")
	assert conc.parse("ab").reduce() == conc.parse("ab")
	assert conc.parse("a[]b").reduce() == charclass.parse("[]")

def test_pattern_reduce_basic():
	assert pattern.parse("ab|cd").reduce() == pattern.parse("ab|cd")
	assert pattern.parse("a{2}b{2}").reduce() == conc.parse("a{2}b{2}")
	assert pattern.parse("a{2}").reduce() == mult.parse("a{2}")
	assert pattern.parse("a").reduce() == charclass.parse("a")

def test_empty_pattern_reduction():
	assert pattern().reduce() == charclass()

def test_empty_mult_suppression():
	assert conc.parse("[]0\d").reduce() == charclass.parse("[]")
	assert conc(
		mult(pattern(), one), # this mult can never actually match anything
		mult(charclass("0"), one),
		mult(charclass("0123456789"), one),
	).reduce() == charclass.parse("[]")

def test_empty_conc_suppression():
	assert pattern.parse("[]0\d").reduce() == charclass.parse("[]")
	assert pattern(
		conc(
			mult(pattern(), one), # this mult can never actually match anything
			mult(charclass("0"), one),
			mult(charclass("0123456789"), one),
		) # so neither can this conc
	).reduce() == charclass.parse("[]")

def test_nested_pattern_reduction():
	# a(d(ab|a*c)) -> ad(ab|a*c)
	assert conc.parse("a(d(ab|a*c))").reduce() == conc.parse("ad(ab|a*c)")

def test_mult_factor_out_qm():
	# mult contains a pattern containing an empty conc? Pull the empty
	# part out where it's external
	assert mult.parse("(a|b*|)").reduce() == mult.parse("(a|b*)?")
	assert conc.parse("(a|b*|)c").reduce() == conc.parse("(a|b*)?c")
	# This happens even if emptystring is the only thing left inside the mult
	assert mult.parse("()").reduce() == conc.parse("")
	assert mult.parse("([$%\\^]|){1}").reduce() == mult.parse("[$%\\^]?")

def test_remove_unnecessary_parens():
	# mult contains a pattern containing a single conc containing a single mult?
	# that can be reduced greatly
	assert conc.parse("(a){2}b").reduce() == conc.parse("a{2}b")
	assert conc.parse("(a?)+b").reduce() == conc.parse("a*b")
	assert mult.parse("([ab])*").reduce() == mult.parse("[ab]*")
	assert mult.parse("(c{1,2}){3,4}").reduce() == mult.parse("c{3,8}")

def test_obvious_reduction():
	assert mult.parse("(a|b)*").reduce() == mult.parse("[ab]*")

def test_mult_squoosh():
	# sequence squooshing of mults within a conc
	assert conc.parse("[0-9a-fA-F][0-9a-fA-F][0-9a-fA-F]").reduce() == mult.parse("[0-9A-Fa-f]{3}")
	assert conc.parse("[$%\\^]?[$%\\^]").reduce() == mult.parse("[$%\\^]{1,2}")
	assert conc.parse("(|(|(|(|(|(|[$%\^])[$%\^])[$%\^])[$%\^])[$%\^])[$%\^])[$%\^][$%\^]").reduce() == mult.parse("[$%\^]{2,8}")

def test_bad_reduction_bug():
	# DEFECT: "0{2}|1{2}" was erroneously reduced() to "[01]{2}"
	assert parse("0{2}|1{2}").reduce() != parse("[01]{2}")
	assert parse("0|[1-9]|ab").reduce() == pattern.parse("\d|ab")
	assert parse("0|[1-9]|a{5,7}").reduce() == pattern.parse("\d|a{5,7}")
	assert parse("0|(0|[1-9]|a{5,7})").reduce() == pattern.parse("0|(\d|a{5,7})")
	# TODO: should do better than this! Merge that 0

def test_common_prefix_pattern_reduction():
	assert pattern.parse("a{2}b|a+c").reduce() == conc.parse("a(ab|a*c)")

def test_epsilon_reduction():
	assert parse("|(ab)*|def").reduce() == pattern.parse("(ab)*|def")
	assert parse("|(ab)+|def").reduce() == pattern.parse("(ab)*|def")
	assert parse("|.+").reduce() == mult.parse(".*")
	assert parse("|a+|b+").reduce() in {pattern.parse("a+|b*"), pattern.parse("a*|b+")}

def test_charclass_intersection_2():
	assert (parse("[A-z]") & parse("[^g]")).reduce() == charclass.parse("[A-fh-z]")

def test_reduce_boom():
	# make sure recursion problem in reduce() has gone away
	assert conc.parse("([1-9]|0)").reduce() == d

def test_new_reduce():
	# The @reduce_after decorator has been removed from many methods since it
	# takes unnecessary time which the user may not wish to spend.
	# This alters the behaviour of several methods and also exposes a new
	# opportunity for conc.reduce()
	assert conc.parse("a()").reduce() == charclass.parse("a")
	assert conc.parse("a()()").reduce() == charclass.parse("a")
	assert conc.parse("a.b()()").reduce() == conc.parse("a.b")

def test_main_bug():
	assert parse("a*").reduce() == mult.parse("a*")
	assert parse("a|a*").reduce() == mult.parse("a*")
	assert parse("a{1,2}|a{3,4}|bc").reduce() == pattern.parse("a{1,4}|bc")
	assert parse("a{1,2}|bc|a{3,4}").reduce() == pattern.parse("a{1,4}|bc")
	assert parse("a{1,2}|a{3,4}|a{5,6}|bc").reduce() == pattern.parse("a{1,6}|bc")
	assert parse("a{1,2}|a{3}|a{5,6}").reduce() == conc.parse("a{1,2}(a?|a{4})")
	assert parse("a{1,2}|a{3}|a{5,6}|bc").reduce() == pattern.parse("a{1,3}|a{5,6}|bc")
	assert parse("a{1,2}|a{4}|a{5,6}").reduce() == conc.parse("a{1,2}(a{3,4})?")
	assert parse("a{1,2}|a{4}|a{5,6}|bc").reduce() == pattern.parse("a{1,2}|a{4,6}|bc")
	assert (parse("a") | parse("a*")).reduce() == mult.parse("a*")

def test_bug_28():
	# Defect in lego.to_fsm()
	assert not parse("(ab*)*").to_fsm().accepts("bb")

def test_derive():
	assert parse("a+").derive("a") == mult.parse("a*")
	assert parse("a+|b+").derive("a") == mult.parse("a*")
	assert parse("abc|ade").derive("a") == pattern.parse("bc|de")
	assert parse("abc|ade").derive("ab") == charclass.parse("c")

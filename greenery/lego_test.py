# -*- coding: utf-8 -*-

if __name__ == "__main__":
	raise Exception("Test files can't be run directly. Use `python -m pytest greenery`")

from greenery.lego import conc, mult, charclass, one, emptystring, star, plus, nothing, pattern, qm, d, multiplier, bound, w, s, W, D, S, dot, nomatch, inf, zero, parse, from_fsm, dollar, caret
from greenery import fsm

def test_new_reduce():
	# The @reduce_after decorator has been removed from many methods since it
	# takes unnecessary time which the user may not wish to spend.
	# This alters the behaviour of several methods and also exposes a new
	# opportunity for conc.reduce()
	assert conc.parse("a()").reduce() == charclass.parse("a")
	assert conc.parse("a()()").reduce() == charclass.parse("a")
	assert conc.parse("a.b()()").reduce() == conc.parse("a.b")
	assert str(parse("a.b()()")) == "a.b()()"
	assert str(parse("a.b()()").reduce()) == "a.b"

def test_conc_common():
	# "AAZY, BBZY" -> "ZY"
	assert conc.parse("AAZY").common(conc.parse("BBZY"), suffix=True) == conc.parse("ZY")
	# "CZ, CZ" -> "CZ"
	assert conc.parse("CZ").common(conc.parse("CZ"), suffix=True) == conc.parse("CZ")
	# "CY, CZ" -> ""
	assert conc.parse("CY").common(conc.parse("CZ"), suffix=True) == conc.parse("")
	# "AZ", "BZ" -> "Z"
	assert conc.parse("AZ").common(conc.parse("BZ"), suffix=True) == conc.parse("Z")
	# "AZ*", "BZ" -> ""
	assert conc.parse("AZ*").common(conc.parse("BZ"), suffix=True) == conc.parse("")
	# "A", "B" -> ""
	assert conc.parse("A").common(conc.parse("B"), suffix=True) == conc.parse("")

def test_conc_subtraction():
	# AZ - Z = A
	assert conc.parse("AZ") - conc.parse("Z") == conc.parse("A")
	# ABXY+Z - XY+Z = AB
	assert conc.parse("ABXY+Z") - conc.parse("XY+Z") == conc.parse("AB")
	# ABXY+Z.behead(ABXY+) = Z
	assert conc.parse("ABXY+Z").behead(conc.parse("ABXY+")) == conc.parse("Z")

	# X{2}Y+Z.behead(XY+) = exception
	try:
		conc.parse("X{2}Y+Z").behead(conc.parse("XY+"))
		assert False
	except AssertionError:
		assert False
	except Exception:
		pass

	# A - () = A
	assert conc.parse("A") - conc.parse("") == conc.parse("A")

def test_odd_bug():
	# Odd bug with ([bc]*c)?[ab]*
	int5A = mult(charclass("bc"), star).to_fsm({"a", "b", "c", fsm.anything_else})
	assert int5A.accepts([])
	assert int5A.accepts("")
	int5B = mult(charclass("c"), one).to_fsm({"a", "b", "c", fsm.anything_else})
	assert int5B.accepts("c")
	assert int5B.accepts(["c"])
	int5C = int5A + int5B
	assert (int5A + int5B).accepts("c")
	assert (int5A + int5B).accepts(["c"])

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

def test_empty_pattern_suppression():
	assert mult.parse("[]?").reduce() == conc.parse("")
	assert mult(pattern(), qm).reduce() == conc.parse("")

def test_empty_pattern_reduction():
	assert pattern().reduce() == charclass()

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

def test_mult_reduction_rule_order():
	# A subtlety in mult reduction.
	# ([$%\^]|){1} should become ([$%\^])? then [$%\^]?,
	# ([$%\^]|){1} should NOT become ([$%\^]|) (the pattern alone)
	assert mult.parse("([$%\\^]|){1}").reduce() == mult.parse("[$%\\^]?")

def test_nested_pattern_reduction():
	# a(d(ab|a*c)) -> ad(ab|a*c)
	assert conc.parse("a(d(ab|a*c))").reduce() == conc.parse("ad(ab|a*c)")

def test_pattern_beheading():
	# "aa".behead("a") = "a"
	assert pattern.parse("aa").behead(conc.parse("a")) == pattern.parse("a")
	# "abc|aa".behead("a") = "a|bc"
	assert pattern.parse("abc|aa").behead(conc.parse("a")) == pattern.parse("a|bc")
	# "cf{1,2}|cf".behead("c") = "f{1,2}|f" (no simplification)
	assert pattern.parse("cf{1,2}|cf").behead(conc.parse("c")) == pattern.parse("f{1,2}|f")

def test_pattern_commonconc():
	# aa, aa -> aa
	assert pattern.parse("aa|aa")._commonconc() == conc.parse("aa")
	# (aa|aa).behead(aa) = ()
	assert pattern.parse("aa|aa").behead(conc.parse("aa")) == pattern.parse("")
	# abc, aa -> a
	assert pattern.parse("abc|aa")._commonconc() == conc.parse("a")
	# (abc|aa).behead(a) = (a|bc)
	assert pattern.parse("abc|aa").behead(conc.parse("a")) == pattern.parse("a|bc")
	# a, bc -> emptystring
	assert pattern.parse("a|bc")._commonconc() == conc.parse("")
	# (a|bc).behead(emptystring) = (a|bc)
	assert pattern.parse("a|bc").behead(conc.parse("")) == pattern.parse("a|bc")
	# cf{1,2}, cf -> cf, (f?|)
	assert pattern.parse("cf{1,2}|cf")._commonconc() == conc.parse("cf")
	# (cf{1,2}|cf).behead(cf) = (f?|)
	assert pattern.parse("cf{1,2}|cf").behead(conc.parse("cf")) == pattern.parse("f?|")
	# ZA|ZB|ZC -> Z
	assert pattern.parse("ZA|ZB|ZC")._commonconc() == conc.parse("Z")
	# ZA|ZB|ZC.behead(Z) = A|B|C
	assert pattern.parse("ZA|ZB|ZC").behead(conc.parse("Z")) == pattern.parse("A|B|C")
	# Z+A|ZB|ZZC -> Z
	assert pattern.parse("Z+A|ZB|ZZC")._commonconc() == conc.parse("Z")
	# Z+A|ZB|ZZC.behead(Z) = Z*A|B|ZC
	assert pattern.parse("Z+A|ZB|ZZC").behead(conc.parse("Z")) == pattern.parse("Z*A|B|ZC")
	# a{2}b|a+c -> a
	assert pattern.parse("a{2}b|a+c")._commonconc() == conc.parse("a")
	# a{2}b|a+c.behead(a) = (ab|a*c)
	assert pattern.parse("a{2}b|a+c").behead(conc.parse("a")) == pattern.parse("ab|a*c")

def test_reduce_boom():
	# make sure recursion problem in reduce()
	# has gone away
	conc.parse("") + mult.parse("([1-9]|0)")

def test_charclass_equality():
	assert charclass("a") == charclass("a")
	assert ~charclass("a") == ~charclass("a")
	assert ~charclass("a") != charclass("a")
	assert charclass("ab") == charclass("ba")

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
	assert str(charclass("\t\v\r' \"'A")) == "[\\t\\v\\r \\\"\\'A]"
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

def test_mult_equality():
	assert mult(charclass("a"), one) == mult(charclass("a"), one)
	assert mult(charclass("a"), one) != mult(charclass("b"), one)
	assert mult(charclass("a"), one) != mult(charclass("a"), qm)
	assert mult(charclass("a"), one) != mult(charclass("a"), multiplier(bound(1), bound(2)))

def test_mult_str():
	a = charclass("a")
	assert str(mult(a, one)) == "a"
	assert str(mult(a, multiplier(bound(2), bound(2)))) == "aa"
	assert str(mult(a, multiplier(bound(3), bound(3)))) == "aaa"
	assert str(mult(a, multiplier(bound(4), bound(4)))) == "aaaa"
	assert str(mult(a, multiplier(bound(5), bound(5)))) == "a{5}"
	assert str(mult(a, qm)) == "a?"
	assert str(mult(a, star)) == "a*"
	assert str(mult(a, plus)) == "a+"
	assert str(mult(a, multiplier(bound(2), bound(5)))) == "a{2,5}"
	assert str(bound(2)) == "2"
	assert str(inf) == ""
	assert str(multiplier(bound(2), inf)) == "{2,}"
	assert str(mult(a, multiplier(bound(2), inf))) == "a{2,}"
	assert str(mult(d, one)) == "\\d"
	assert str(mult(d, multiplier(bound(2), bound(2)))) == "\\d\\d"
	assert str(mult(d, multiplier(bound(3), bound(3)))) == "\\d{3}"

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

# mult.reduce() tests

def test_mult_reduction_easy():
	# mult -> mult
	# mult -> charclass
	assert mult(charclass("a"), one).reduce() == charclass("a")
	assert mult(charclass("a"), qm).reduce() == mult(charclass("a"), qm)
	assert mult(charclass("a"), zero).reduce() == emptystring
	assert mult(nothing, one).reduce() == nothing
	assert mult(nothing, qm).reduce() == emptystring
	assert mult(nothing, zero).reduce() == emptystring
	assert mult(nothing, multiplier(bound(0), bound(5))).reduce() == emptystring
	assert mult(pattern(), one).reduce() == nothing
	assert mult(pattern(), qm).reduce() == emptystring
	assert mult(pattern(), zero).reduce() == emptystring
	assert mult(pattern(), multiplier(bound(0), bound(5))).reduce() == emptystring

def test_mult_factor_out_qm():
	# mult contains a pattern containing an empty conc? Pull the empty
	# part out where it's external
	# e.g. (a|b*|) -> (a|b*)?
	assert mult.parse("(a|b*|)").reduce() == mult.parse("(a|b*)?")
	# e.g. (a|b*|)c -> (a|b*)?c
	assert conc.parse("(a|b*|)c").reduce() == conc.parse("(a|b*)?c")
	# This happens even if emptystring is the only thing left inside the mult
	assert mult.parse("()").reduce() == conc.parse("")

def test_remove_unnecessary_parens():
	# mult contains a pattern containing a single conc containing a single mult?
	# that can be reduced greatly
	# e.g. "([ab])*" -> "[ab]*"
	assert mult.parse("([ab])*").reduce() == mult.parse("[ab]*")
	# e.g. "(c{1,2}){3,4}" -> "c{3,8}"
	assert mult.parse("(c{1,2}){3,4}").reduce() == mult.parse("c{3,8}")

def test_obvious_reduction():
	# recursive mult reduction
	# (a|b)* -> [ab]*
	assert mult.parse("(a|b)*").reduce() == mult.parse("[ab]*")

def test_mult_subtraction():
	# a{4,5} - a{3} = a{1,2}
	assert mult.parse("a{4,5}") - mult.parse("a{3}") == mult.parse("a{1,2}")

def test_conc_equality():
	assert conc(mult(charclass("a"), one)) == conc(mult(charclass("a"), one))
	assert conc(mult(charclass("a"), one)) != conc(mult(charclass("b"), one))
	assert conc(mult(charclass("a"), one)) != conc(mult(charclass("a"), qm))
	assert conc(mult(charclass("a"), one)) != conc(mult(charclass("a"), multiplier(bound(1), bound(2))))
	assert conc(mult(charclass("a"), one)) != emptystring

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

def test_conc_reduction_basic():
	assert conc.parse("a[]b").reduce() == charclass.parse("[]")
	# conc -> conc
	assert conc.parse("ab").reduce() == conc.parse("ab")
	# conc -> mult
	assert conc.parse("a{3,4}").reduce() == mult.parse("a{3,4}")
	# conc -> charclass
	assert conc.parse("a").reduce() == charclass.parse("a")

def test_mult_squoosh():
	# sequence squooshing of mults within a conc
	# e.g. "[$%\\^]?[$%\\^]" -> "[$%\\^]{1,2}"
	assert conc.parse("[$%\\^]?[$%\\^]").reduce() == mult.parse("[$%\\^]{1,2}")

def test_conc_reduce_advanced():
	# recursive conc reduction
	# (a){2}b -> a{2}b
	assert conc.parse("(a?)+b").reduce() == conc.parse("a*b")

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

def test_pattern_reduce_basic():
	# pattern -> pattern
	# (ab|cd) -> (ab|cd)
	assert pattern.parse("ab|cd").reduce() == pattern.parse("ab|cd")
	# pattern -> conc
	assert pattern.parse("a{2}b{2}").reduce() == conc.parse("a{2}b{2}")
	# pattern -> mult
	assert pattern.parse("a{2}").reduce() == mult.parse("a{2}")
	# pattern -> charclass
	assert pattern.parse("a").reduce() == charclass.parse("a")

def test_special_pattern_reduction():
	# 0|[1-9]|a{5,7} -> [0-9]|a{5,7}
	assert pattern.parse("0|[1-9]|a{5,7}").reduce() == pattern.parse("[0-9]|a{5,7}")

def test_recursive_pattern_reduction():
	assert pattern.parse("0|(0|[1-9]|a{5,7})").reduce() == pattern.parse("0|(\d|a{5,7})")
	# TODO: should do better than this! Merge that 0

def test_common_prefix_pattern_reduction():
	# a{2}b|a+c -> a(ab|a*c)
	assert pattern.parse("a{2}b|a+c").reduce() == conc.parse("a(ab|a*c)")

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

def test_charclass_multiplication():
	# a * 1 = a
	assert charclass("a") * one == charclass("a")
	# a * {1,3} = a{1,3}
	assert charclass("a") * multiplier.parse("{1,3}") == mult.parse("a{1,3}")
	# a * {4,} = a{4,}
	assert charclass("a") * multiplier.parse("{4,}") == mult.parse("a{4,}")

def test_mult_multiplication():
	# a{2,3} * 1 = a{2,3}
	assert mult.parse("a{2,3}") * one == mult.parse("a{2,3}")
	# a{2,3} * {4,5} = a{8,15}
	assert mult.parse("a{2,3}") * multiplier.parse("{4,5}") == mult.parse("a{8,15}")
	# a{2,} * {2,} = a{4,}
	assert mult.parse("a{2,}") * multiplier.parse("{2,}") == mult.parse("a{4,}")

def test_conc_multiplication():
	# ab? * {0,1} = (ab?)?
	assert conc.parse("ab?") * qm == mult.parse("(ab?)?")

def test_pattern_multiplication():
	# (ab?|ba?) * {2,3} = (ab?|ba?){2,3}
	assert pattern.parse("ab?|ba?") * multiplier.parse("{2,3}") == mult.parse("(ab?|ba?){2,3}")
	# TODO
	#assert pattern.parse("(ab?|ba?)") * multiplier.parse("{2,3}") == mult.parse("(ab?|ba?){2,3}")

def test_bound():
	assert min(bound(0), inf) == bound(0)
	assert min(bound(1), inf) == bound(1)
	assert qm.mandatory == bound(0)
	assert qm.optional == bound(1)

def test_bound_common():
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

def test_multiplier_common():
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

def test_mult_intersection():
	# a & b? = nothing
	assert mult.parse("a") & mult.parse("b?") == charclass()
	assert mult.parse("a") & mult.parse("b?") == nothing

	# a & a? = nothing
	assert mult.parse("a").reduce() == charclass.parse("a")
	assert mult.parse("a") & mult.parse("a?") == charclass.parse("a")

	# a{2} & a{2,} = a{2}
	assert mult.parse("a{2}") & mult.parse("a{2,}") == mult.parse("a{2}")

	# a & b -> no intersection.
	assert mult.parse("a") & mult.parse("b") == charclass.parse("[]")

	# a & a -> a
	assert mult.parse("a") & mult.parse("a") == charclass.parse("a")

	# a* & a -> a
	assert mult.parse("a*") & mult.parse("a") == charclass.parse("a")

	# a* & b* -> emptystring
	assert mult.parse("a*") & mult.parse("b*") == conc.parse("")

	# a* & a+ -> a+
	assert mult.parse("a*") & mult.parse("a+") == mult.parse("a+")

	# aa & aaaa -> []
	assert mult.parse("a{2}") & mult.parse("a{4}") == charclass.parse("[]")

	# a{3,4} & a{2,5} -> a{2,3}
	assert mult.parse("a{3,4}").common(mult.parse("a{2,5}")) == mult.parse("a{2,3}")

	# a{2,} & a{1,5} -> a{1,5}
	assert mult.parse("a{2,}").common(mult.parse("a{1,5}")) == mult.parse("a{1,5}")

	# a{3,}, a{2,} -> a{2,} (with a, epsilon left over)
	assert mult.parse("a{3,}").common(mult.parse("a{2,}")) == mult.parse("a{2,}")

	# a{3,}, a{3,} -> a{3,} (with inf, inf left over)
	assert mult.parse("a{3,}") & mult.parse("a{3,}") == mult.parse("a{3,}")

def test_pattern_commonconc_suffix():
	# pattern._commonconc(suffix=True) tests

	# a | bc -> emptystring
	assert pattern.parse("a|bc")._commonconc(suffix=True) == conc.parse("")
	# (a|bc) - () = (a|bc)
	assert pattern.parse("a|bc") - conc.parse("") == pattern.parse("a|bc")
	# (aa|bca) -> a
	assert pattern.parse("aa|bca")._commonconc(suffix=True) == conc.parse("a")
	# (aa|bca) - a = (a|bc)
	assert pattern.parse("aa|bca") - conc.parse("a") == pattern.parse("a|bc")
	# xyza | abca | a -> a
	assert pattern.parse("xyza|abca|a")._commonconc(suffix=True) == conc.parse("a")
	# (xyza|abca|a) - a = (xyz|abc|)
	assert pattern.parse("xyza|abca|a") - conc.parse("a") == pattern.parse("xyz|abc|")
	# f{2,3}c, fc -> fc
	assert pattern.parse("f{2,3}c|fc")._commonconc(suffix=True) == conc.parse("fc")
	# (f{2,3}c|fc) - fc = (f{1,2}|)
	assert pattern.parse("f{2,3}c|fc") - conc.parse("fc") == pattern.parse("f{1,2}|")
	# (aa) -> aa
	assert pattern.parse("aa")._commonconc(suffix=True) == conc.parse("aa")
	# (aa) - aa = ()
	assert pattern.parse("aa") - conc.parse("aa") == pattern.parse("")

def test_concatenation():

	# empty conc + empty conc
	assert conc.parse("") + conc.parse("") == conc.parse("")

	# charclass + charclass
	# a + b = ab
	assert charclass.parse("a") + charclass.parse("b") == conc.parse("ab")
	# a + a = a{2}
	assert (charclass.parse("a") + charclass.parse("a")).reduce() == mult.parse("a{2}")

	# charclass + mult
	# a + a = a{2}
	assert (charclass.parse("a") + mult.parse("a")).reduce() == mult.parse("a{2}")
	# a + a{2,} = a{3,}
	assert (charclass.parse("a") + mult.parse("a{2,}")).reduce() == mult.parse("a{3,}")
	# a + a{,8} = a{1,9}
	assert (charclass.parse("a") + mult.parse("a{0,8}")).reduce() == mult.parse("a{1,9}")
	# a + b{,8} = ab{,8}
	assert charclass.parse("a") + mult.parse("b{0,8}") == conc.parse("ab{0,8}")

	# mult + charclass
	# b + b = b{2}
	assert (mult.parse("b") + charclass.parse("b")).reduce() == mult.parse("b{2}")
	# b* + b = b+
	assert (mult.parse("b*") + charclass.parse("b")).reduce() == mult.parse("b+")
	 # b{,8} + b = b{1,9}
	assert (mult.parse("b{0,8}") + charclass.parse("b")).reduce() == mult.parse("b{1,9}")
	# b{,8} + c = b{,8}c
	assert mult.parse("b{0,8}") + charclass.parse("c") == conc.parse("b{0,8}c")

	# charclass + conc
	# a + nothing = a
	assert (charclass.parse("a") + conc.parse("")).reduce() == charclass.parse("a")
	# a + bc = abc
	assert charclass.parse("a") + conc.parse("bc") == conc.parse("abc")
	# a + ab = a{2}b
	assert (charclass.parse("a") + conc.parse("ab")).reduce() == conc.parse("a{2}b")

	# conc + charclass
	# nothing + a = a
	assert (conc.parse("") + charclass.parse("a")).reduce() == charclass.parse("a")
	# ab + c = abc
	assert conc.parse("ab") + charclass.parse("c") == conc.parse("abc")
	# ab + b = ab{2}
	assert (conc.parse("ab") + charclass.parse("b")).reduce() == conc.parse("ab{2}")

	# pattern + charclass
	# (a|bd) + c = (a|bd)c
	assert pattern.parse("a|bd") + charclass.parse("c") == conc.parse("(a|bd)c")
	# (ac{2}|bc+) + c = (ac|bc*)c{2}
	assert (pattern.parse("ac{2}|bc+") + charclass.parse("c")).reduce() == conc.parse("(ac|bc*)c{2}")

	# charclass + pattern
	# a + (b|cd) = a(b|cd)
	assert charclass.parse("a") + pattern.parse("b|cd") == conc.parse("a(b|cd)")
	# a + (a{2}b|a+c) = a{2}(ab|a*c)
	assert (charclass.parse("a") + pattern.parse("(a{2}b|a+c)")).reduce() == conc.parse("a{2}(ab|a*c)")

	# mult + mult
	# a{3,4} + b? = a{3,4}b?
	assert mult.parse("a{3,4}") + mult.parse("b?") == conc.parse("a{3,4}b?")
	# a* + a{2} = a{2,}
	assert (mult.parse("a*") + mult.parse("a{2}")).reduce() == mult.parse("a{2,}")

	# mult + conc
	# a{2} + bc = a{2}bc
	assert mult.parse("a{2}") + conc.parse("bc") == conc.parse("a{2}bc")
	# a? + ab = a{1,2}b
	assert (mult.parse("a?") + conc.parse("ab")).reduce() == conc.parse("a{1,2}b")

	# conc + mult
	# ab + c* = abc*
	assert conc.parse("ab") + mult.parse("c*") == conc.parse("abc*")
	# ab + b* = ab+
	assert (conc.parse("ab") + mult.parse("b*")).reduce() == conc.parse("ab+")

	# mult + pattern
	# a{2,3} + (b|cd) = a{2,3}(b|cd)
	assert mult.parse("a{2,3}") + pattern.parse("b|cd") == conc.parse("a{2,3}(b|cd)")
	# a{2,3} + (a{2}b|a+c) = a{3,4}(ab|a*c)
	assert (mult.parse("a{2,3}") + pattern.parse("a{2}b|a+c")).reduce() == conc.parse("a{3,4}(ab|a*c)")

	# pattern + mult
	# (b|cd) + a{2,3} = (b|cd)a{2,3}
	assert pattern.parse("b|cd") + mult.parse("a{2,3}") == conc.parse("(b|cd)a{2,3}")
	# (ba{2}|ca+) + a{2,3} = (ba|ca*)a{3,4}
	assert (pattern.parse("ba{2}|ca+") + mult.parse("a{2,3}")).reduce() == conc.parse("(ba|ca*)a{3,4}")

	# conc + conc
	# ab + cd = abcd
	assert conc.parse("ab") + conc.parse("cd") == conc.parse("abcd")
	# ab + bc = ab{2}c
	assert (conc.parse("ab") + conc.parse("bc")).reduce() == conc.parse("ab{2}c")

	# conc + pattern
	# za{2,3} + (b|cd) = za{2,3}(b|cd)
	assert conc.parse("za{2,3}") + pattern.parse("b|cd") == conc.parse("za{2,3}(b|cd)")
	# za{2,3} + (a{2}b|a+c) = za{3,4}(ab|a*c)
	assert (conc.parse("za{2,3}") + pattern.parse("a{2}b|a+c")).reduce() == conc.parse("za{3,4}(ab|a*c)")

	# pattern + conc
	# (b|cd) + za{2,3} = (b|cd)za{2,3}
	assert pattern.parse("b|cd") + conc.parse("za{2,3}") == conc.parse("(b|cd)za{2,3}")
	# (ba{2}|ca+) + a{2,3}z = (ba|ca*)a{3,4}z
	assert (pattern.parse("ba{2}|ca+") + conc.parse("a{2,3}z")).reduce() == conc.parse("(ba|ca*)a{3,4}z")

	# pattern + pattern
	# (a|bc) + (c|de) = (a|bc)(c|de)
	assert pattern.parse("a|bc") + pattern.parse("c|de") == conc.parse("(a|bc)(c|de)")
	# (a|bc) + (a|bc) = (a|bc){2}
	assert (pattern.parse("a|bc") + pattern.parse("a|bc")).reduce() == mult.parse("(a|bc){2}")

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

def test_parse_str_round_trip():
	assert str(parse("a.b")) == "a.b" # not "a[ab]b"
	assert str(parse("\\d{4}")) == "\\d{4}"

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
	assert parse("a{0,2}").to_fsm().accepts("")
	assert parse("[ab]{0,2}").to_fsm().accepts("")
	assert parse("[ab]{0,2}").to_fsm().accepts([])
	assert parse("[^a]{0,2}").to_fsm().accepts([])
	assert parse("b{0,2}").to_fsm().accepts([])
	assert str(parse("[ab]{0,2}") & parse("[^a]{0,2}")) == "b{0,2}"
	assert str(parse("[ab]{0,4}") & parse("[^a]{0,4}")) == "b{0,4}"
	assert str(parse("[abc]{0,8}") & parse("[^a]{0,8}")) == "[bc]{0,8}"
	assert str(parse("[a-g0-8$%\\^]{0,8}") & parse("[^d]{0,8}")) == "[$%0-8\\^abcefg]{0,8}"
	assert str(parse("[a-g0-8$%\\^]+") & parse("[^d]{0,8}")) == "[$%0-8\\^abcefg]{1,8}"
	assert str(parse("[a-g0-8$%\\^]+") & parse("[^d]{2,8}")) == "[$%0-8\\^abcefg]{2,8}"
	assert str(parse("\\W*") & parse("[a-g0-8$%\\^]+") & parse("[^d]{2,8}")) == "[$%\\^]{2,8}"
	assert str(parse("\\d{4}-\\d{2}-\\d{2}") & parse("19.*")) == "19\\d\\d-\\d\\d-\\d\\d"

def test_heavy_reduction():
	assert str(parse("(|(|(|(|(|(|[$%\^])[$%\^])[$%\^])[$%\^])[$%\^])[$%\^])[$%\^][$%\^]").reduce()) == "[$%\^]{2,8}"
	assert str(parse("[0-9a-fA-F][0-9a-fA-F][0-9a-fA-F]").reduce()) == "[0-9A-Fa-f]{3}"

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

def test_bad_reduction_bug():
	# DEFECT: "0{2}|1{2}" was erroneously reduced() to "[01]{2}"
	bad = parse("0{2}|1{2}").to_fsm({"0", "1", fsm.anything_else})
	assert bad.accepts("00")
	assert bad.accepts("11")
	assert not bad.accepts("01")
	assert str(parse("0|[1-9]|ab").reduce()) == "\d|ab"

def test_alphabet():
	# lego.alphabet() should include `fsm.anything_else`
	assert parse("").alphabet() == {fsm.anything_else}

def test_fsm():
	# You should be able to to_fsm() a single lego piece without supplying a specific
	# alphabet. That should be determinable from context.
	assert str(from_fsm(parse("a.b").to_fsm())) == "a.b" # not "a[ab]b"

	# A suspiciously familiar example
	bad = parse("0{2}|1{2}").to_fsm()
	assert bad.accepts("00")
	assert bad.accepts("11")
	assert not bad.accepts("01")
	assert str(parse("0|[1-9]|ab").reduce()) == "\d|ab"

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
	assert str(beer2) == "beer"

	# ".*" becomes "[]" and vice versa under this call.
	everything = parse(".*")
	assert str(everything.everythingbut()) == str(nothing)
	assert str(nothing.everythingbut()) == str(everything)

def test_epsilon_reduction():
	assert parse("|(ab)*|def").reduce() == pattern.parse("(ab)*|def")
	assert parse("|(ab)+|def").reduce() == pattern.parse("(ab)*|def")
	assert parse("|.+").reduce() == mult.parse(".*")
	assert parse("|a+|b+").reduce() in {pattern.parse("a+|b*"), pattern.parse("a*|b+")}

def test_regex_reversal():
	assert reversed(parse("b")) == parse("b")
	assert reversed(parse("e*")) == parse("e*")
	assert reversed(parse("bear")) == parse("raeb")
	assert reversed(parse("beer")) == parse("reeb")
	assert reversed(parse("abc|def|ghi")) == parse("cba|fed|ihg")
	assert reversed(parse("(abc)*d")) == parse("d(cba)*")

def test_even_star_bug():
	# Defect: (a{2})* should NOT reduce to a*
	a2 = mult.parse("a{2}")
	a2star = a2 * star
	assert a2star == mult.parse("(a{2})*")

def test_wildcards_in_charclasses():
	# Allow "\w", "\d" and "\s" in charclasses
	assert parse("[\w~]*").to_fsm().accepts("a0~")
	assert parse("[\da]*").to_fsm().accepts("0129a")
	assert parse("[\s]+").to_fsm().accepts(" \t \t ")

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
	# [ab]*
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
	# [ab][cd]
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
	# [ab]|[cde]
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

def test_isinstance_bug():
	# Problem relating to isinstance(). The class "mult" was occurring as both
	# lego.mult and as __main__.mult and apparently these count as different
	# classes for some reason, so isinstance(m, mult) was returning false.
	starfree = (parse("").everythingbut() + parse("aa") + parse("").everythingbut()).everythingbut()

def test_repr():
	assert repr(~charclass("a")) == "~charclass('a')"

def test_hex_escapes():
	# Should be able to parse e.g. "\\x40"
	assert parse("\\x00") == parse("\x00")
	assert parse("\\x40") == parse("@")
	assert parse("[\\x40]") == parse("[@]")
	assert parse("[\\x41-\\x5a]") == parse("[A-Z]")
	assert str(parse("\\x09")) == "\\t" # escape sequences are not preserved

	# Printing ASCII control characters? You should get hex escapes
	assert str(parse("\\x00")) == "\\x00"

def test_charclass_ranges():
	# Should accept arbitrary ranges of characters in charclasses. No longer
	# limited to alphanumerics. (User beware...)
	assert parse("[z{|}~]") == parse("[z-~]")
	assert str(parse("[\w:;<=>?@\\[\\\\\]\\^`]")) == "[0-z]"
	assert (parse("[A-z]") & parse("[^g]")).reduce() == parse("[A-fh-z]").reduce()

def test_non_capturing_groups():
	# Accept the "non-capturing group" syntax, "(?: ... )" but give it no
	# special significance
	assert parse("(?:)") == parse("()")
	assert parse("(?:abc|def)") == parse("(abc|def)")
	parse("(:abc)") # should give no problems

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

def test_main_bug():
	# A new reduction. a|a* -> a*
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

def test_equivalence():
	assert parse("aa*").equivalent(parse("a*a"))
	assert parse("([ab]*a|[bc]*c)?b*").equivalent(parse("b*(a[ab]*|c[bc]*)?"))

# Tests imported from `fsm` when its dependence on `lego` was severed

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

def test_lazy_quantifier():
	a = parse('a*?b+?c')
	assert a.matches('abc')
	assert a.matches('bbc')

def test_special_cases_for_charclass():
	a = parse('[- ]')
	assert a.matches('-')
	assert a.matches(' ')
	a = parse('[ -]')
	assert a.matches('-')
	assert a.matches(' ')

def test_parse_anchors():
	assert str(parse(r"\ba\b")) == r"\ba\b"
	assert str(parse(r"^a$")) == r"^a$"
	assert str(parse(r"\Aa\Z")) == r"\Aa\Z"
	assert str(parse(r"\Ga\z")) == r"\Ga\z"
	a = parse(r"^a$")
	mults = list(list(a.concs)[0].mults)
	assert mults[0] == caret
	assert mults[2] == dollar
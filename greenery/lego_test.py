# -*- coding: utf-8 -*-

if __name__ == "__main__":
	import os
	import sys
	# If you run tests in-place (instead of using py.test), ensure local version is tested!
	sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from greenery.lego import *

def test_lego():
	# "AAZY, BBZY" -> "ZY"
	assert conc(
		mult(charclass("A"), one),
		mult(charclass("A"), one),
		mult(charclass("Z"), one),
		mult(charclass("Y"), one),
	).common(
		conc(
			mult(charclass("B"), one),
			mult(charclass("B"), one),
			mult(charclass("Z"), one),
			mult(charclass("Y"), one),
		),
		suffix=True
	) == conc(
		mult(charclass("Z"), one),
		mult(charclass("Y"), one),
	)

	# "CZ, CZ" -> "CZ"
	assert conc(
		mult(charclass("C"), one),
		mult(charclass("Z"), one),
	).common(
		conc(
			mult(charclass("C"), one),
			mult(charclass("Z"), one),
		),
		suffix=True
	) == conc(
		mult(charclass("C"), one),
		mult(charclass("Z"), one),
	)

	# "CY, CZ" -> ""
	assert conc(
		mult(charclass("C"), one),
		mult(charclass("Y"), one),
	).common(
		conc(
			mult(charclass("C"), one),
			mult(charclass("Z"), one),
		),
		suffix=True
	) == emptystring

	# AZ, BZ -> Z
	assert conc(
		mult(charclass("A"), one),
		mult(charclass("Z"), one),
	).common(
		conc(
			mult(charclass("B"), one),
			mult(charclass("Z"), one),
		),
		suffix=True
	) == conc(
		mult(charclass("Z"), one)
	)

	# AZ*, BZ -> ()
	assert conc(
		mult(charclass("A"), one),
		mult(charclass("Z"), star),
	).common(
		conc(
			mult(charclass("B"), one),
			mult(charclass("Z"), one),
		),
		suffix=True
	) == emptystring

	# A, B -> no common part
	assert conc(
		mult(charclass("A"), one),
	).common(
		conc(
			mult(charclass("B"), one),
		),
		suffix=True
	) == emptystring

	# Conc subtraction

	# AZ - Z = A
	assert conc(
		mult(charclass("A"), one),
		mult(charclass("Z"), one),
	) - conc(
		mult(charclass("Z"), one)
	) == conc(
		mult(charclass("A"), one)
	)

	# ABXY+Z - XY+Z = AB
	assert conc(
		mult(charclass("A"), one),
		mult(charclass("B"), one),
		mult(charclass("X"), one),
		mult(charclass("Y"), plus),
		mult(charclass("Z"), one),
	) - conc(
		mult(charclass("X"), one),
		mult(charclass("Y"), plus),
		mult(charclass("Z"), one),
	) == conc(
		mult(charclass("A"), one),
		mult(charclass("B"), one),
	)

	# ABXY+Z.behead(ABXY+) = Z
	assert conc(
		mult(charclass("A"), one),
		mult(charclass("B"), one),
		mult(charclass("X"), one),
		mult(charclass("Y"), plus),
		mult(charclass("Z"), one),
	).behead(
		conc(
			mult(charclass("A"), one),
			mult(charclass("B"), one),
			mult(charclass("X"), one),
			mult(charclass("Y"), plus),
		)
	)== conc(
		mult(charclass("Z"), one),
	)

	# X{2}Y+Z.behead(XY+) = exception
	try:
		conc(
			mult(charclass("X"), multiplier),
			mult(charclass("Y"), plus),
			mult(charclass("Z"), one),
		).behead(
			conc(
				mult(charclass("X"), one),
				mult(charclass("Y"), plus),
			)
		)
		assert(False)
	except:
		pass

	# A - () = A
	assert conc(
		mult(charclass("A"), one),
	) - emptystring == conc(
		mult(charclass("A"), one),
	)

	# Odd bug with ([bc]*c)?[ab]*
	int5A = mult(charclass("bc"), star).fsm(set(["a", "b", "c", otherchars]))
	assert int5A.accepts("")
	int5B = mult(charclass("c"), one).fsm(set(["a", "b", "c", otherchars]))
	assert int5B.accepts("c")
	int5C = int5A + int5B
	assert (int5A + int5B).accepts("c")

	# Empty mult suppression
	assert conc(
		mult(nothing, one), # this mult can never actually match anything
		mult(charclass("0"), one),
		mult(charclass("0123456789"), one),
	).reduce() == nothing
	assert conc(
		mult(pattern(), one), # this mult can never actually match anything
		mult(charclass("0"), one),
		mult(charclass("0123456789"), one),
	).reduce() == nothing

	# Empty conc suppression in patterns.
	assert pattern(
		conc(
			mult(nothing, one), # this mult can never actually match anything
			mult(charclass("0"), one),
			mult(charclass("0123456789"), one),
		) # so neither can this conc
	).reduce() == nothing
	assert pattern(
		conc(
			mult(pattern(), one), # this mult can never actually match anything
			mult(charclass("0"), one),
			mult(charclass("0123456789"), one),
		) # so neither can this conc
	).reduce() == nothing

	# Empty pattern suppression in mults
	assert mult(nothing, qm).reduce() == emptystring
	assert mult(pattern(), qm).reduce() == emptystring

	# empty pattern behaviour
	assert pattern().reduce() == charclass()

	# pattern.fsm()

	# "a[^a]"
	anota = pattern(
		conc(
			mult(charclass("a"), one),
			mult(~charclass("a"), one),
		)
	).fsm("ab")
	assert not anota.accepts("a")
	assert not anota.accepts("b")
	assert not anota.accepts("aa")
	assert anota.accepts("ab")
	assert not anota.accepts("ba")
	assert not anota.accepts("bb")

	# "0\\d"
	zeroD = pattern(
		conc(
			mult(charclass("0"), one),
			mult(charclass("123456789"), one)
		)
	).fsm(d.chars)
	assert zeroD.accepts("01")
	assert not zeroD.accepts("10")

	# "\\d{2}"
	d2 = pattern(
		conc(
			mult(
				d, multiplier(bound(2), bound(2))
			)
		)
	).fsm(d.chars)
	assert not d2.accepts("")
	assert not d2.accepts("1")
	assert d2.accepts("11")
	assert not d2.accepts("111")

	# abc|def(ghi|jkl)
	conventional = pattern(
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
	).fsm(w.chars)
	assert not conventional.accepts("a")
	assert not conventional.accepts("ab")
	assert conventional.accepts("abc")
	assert not conventional.accepts("abcj")
	assert conventional.accepts("defghi")
	assert conventional.accepts("defjkl")

	# A subtlety in mult reduction.
	# ([$%\^]|){1} should become ([$%\^])? then [$%\^]?,
	# ([$%\^]|){1} should NOT become ([$%\^]|) (the pattern alone)
	assert mult(
		pattern(
			conc(),
			conc(
				mult(charclass("$%^"), one)
			)
		), one
	).reduce() == mult(charclass("$%^"), qm)

	# nested pattern reduction in a conc
	# a(d(ab|a*c)) -> ad(ab|a*c)
	assert conc(
		mult(charclass("a"), one),
		mult(
			pattern(
				# must contain only one conc. Otherwise, we have e.g. "a(zz|d(ab|a*c))"
				conc(
					# can contain anything
					mult(charclass("d"), one),
					mult(
						pattern(
							conc(
								mult(charclass("a"), one),
								mult(charclass("b"), one),
							),
							conc(
								mult(charclass("a"), star),
								mult(charclass("c"), one),
							),
						), one
					),
				),
			), one # must be one. Otherwise, we have e.g. "a(d(ab|a*c)){2}"
		)
	).reduce() == conc(
		mult(charclass("a"), one),
		mult(charclass("d"), one),
		mult(
			pattern(
				conc(
					mult(charclass("a"), one),
					mult(charclass("b"), one),
				),
				conc(
					mult(charclass("a"), star),
					mult(charclass("c"), one),
				),
			), one
		),
	)

	# "(aa)".behead("a") = "a"
	assert pattern(
		conc(
			mult(charclass("a"), one),
			mult(charclass("a"), one),
		),
	).behead(conc(mult(charclass("a"), one))) == pattern(
		conc(
			mult(charclass("a"), one)
		),
	)

	# (abc|aa).behead(a) = (a|bc)
	assert pattern(
		conc(
			mult(charclass("a"), one),
			mult(charclass("b"), one),
			mult(charclass("c"), one),
		),
		conc(
			mult(charclass("a"), one),
			mult(charclass("a"), one),
		),
	).behead(conc(mult(charclass("a"), one))) == pattern(
		conc(
			mult(charclass("a"), one),
		),
		conc(
			mult(charclass("b"), one),
			mult(charclass("c"), one),
		),
	)

	# (cf{1,2}|cf) - c = (f|f?)
	assert pattern(
		conc(
			mult(charclass("c"), one),
			mult(charclass("f"), multiplier(bound(1), bound(2))),
		),
		conc(
			mult(charclass("c"), one),
			mult(charclass("f"), one),
		),
	).behead(conc(mult(charclass("c"), one))) == pattern(
		conc(
			mult(charclass("f"), multiplier(bound(1), bound(2))),
		),
		conc(
			mult(charclass("f"), one),
		),
	)

	# pattern._commonconc() tests

	# aa, aa -> aa
	assert pattern(
		conc(
			mult(charclass("a"), one),
			mult(charclass("a"), one),
		),
		conc(
			mult(charclass("a"), one),
			mult(charclass("a"), one),
		),
	)._commonconc() == conc(
		mult(charclass("a"), one),
		mult(charclass("a"), one),
	)

	# (aa|aa).behead(aa) = ()
	assert pattern(
		conc(
			mult(charclass("a"), one),
			mult(charclass("a"), one),
		),
		conc(
			mult(charclass("a"), one),
			mult(charclass("a"), one),
		),
	).behead(
		conc(
			mult(charclass("a"), one),
			mult(charclass("a"), one),
		)
	) == pattern(emptystring)

	# abc, aa -> a
	assert pattern(
		conc(
			mult(charclass("a"), one),
			mult(charclass("b"), one),
			mult(charclass("c"), one),
		),
		conc(
			mult(charclass("a"), one),
			mult(charclass("a"), one),
		),
	)._commonconc() == conc(
		mult(charclass("a"), one),
	)

	# (abc|aa).behead(a) = (a|bc)
	assert pattern(
		conc(
			mult(charclass("a"), one),
			mult(charclass("b"), one),
			mult(charclass("c"), one),
		),
		conc(
			mult(charclass("a"), one),
			mult(charclass("a"), one),
		),
	).behead(
		conc(
			mult(charclass("a"), one),
		)
	) == pattern(
		conc(
			mult(charclass("a"), one),
		),
		conc(
			mult(charclass("b"), one),
			mult(charclass("c"), one),
		),
	)

	# a, bc -> emptystring
	assert pattern(
		conc(
			mult(charclass("a"), one),
		),
		conc(
			mult(charclass("b"), one),
			mult(charclass("c"), one),
		),
	)._commonconc() == emptystring

	# (a|bc).behead(emptystring) = (a|bc)
	assert pattern(
		conc(
			mult(charclass("a"), one),
		),
		conc(
			mult(charclass("b"), one),
			mult(charclass("c"), one),
		),
	).behead(emptystring) == pattern(
		conc(
			mult(charclass("a"), one),
		),
		conc(
			mult(charclass("b"), one),
			mult(charclass("c"), one),
		),
	)

	# cf{1,2}, cf -> cf, (f?|)
	assert pattern(
		conc(
			mult(charclass("c"), one),
			mult(charclass("f"), multiplier(bound(1), bound(2))),
		),
		conc(
			mult(charclass("c"), one),
			mult(charclass("f"), one),
		),
	)._commonconc() == conc(
		mult(charclass("c"), one),
		mult(charclass("f"), one),
	)

	# (cf{1,2}|cf).behead(cf) = (f?|)
	assert pattern(
		conc(
			mult(charclass("c"), one),
			mult(charclass("f"), multiplier(bound(1), bound(2))),
		),
		conc(
			mult(charclass("c"), one),
			mult(charclass("f"), one),
		),
	).behead(
		conc(
			mult(charclass("c"), one),
			mult(charclass("f"), one),
		)
	) == pattern(
		emptystring,
		conc(
			mult(charclass("f"), qm),
		),
	)

	# ZA|ZB|ZC -> Z
	assert pattern(
		conc(
			mult(charclass("Z"), one),
			mult(charclass("A"), one),
		),
		conc(
			mult(charclass("Z"), one),
			mult(charclass("B"), one),
		),
		conc(
			mult(charclass("Z"), one),
			mult(charclass("C"), one),
		),
	)._commonconc() == conc(mult(charclass("Z"), one))

	# ZA|ZB|ZC.behead(Z) = A|B|C
	assert pattern(
		conc(
			mult(charclass("Z"), one),
			mult(charclass("A"), one),
		),
		conc(
			mult(charclass("Z"), one),
			mult(charclass("B"), one),
		),
		conc(
			mult(charclass("Z"), one),
			mult(charclass("C"), one),
		),
	).behead(
		conc(mult(charclass("Z"), one))
	) == pattern(
		conc(mult(charclass("A"), one)),
		conc(mult(charclass("B"), one)),
		conc(mult(charclass("C"), one)),
	)

	# Z+A|ZB|ZZC -> Z
	assert pattern(
		conc(
			mult(charclass("Z"), plus),
			mult(charclass("A"), one),
		),
		conc(
			mult(charclass("Z"), one),
			mult(charclass("B"), one),
		),
		conc(
			mult(charclass("Z"), one),
			mult(charclass("Z"), one),
			mult(charclass("C"), one),
		),
	)._commonconc() == conc(mult(charclass("Z"), one))

	# Z+A|ZB|ZZC.behead(Z) = Z*A|B|ZC
	assert pattern(
		conc(
			mult(charclass("Z"), plus),
			mult(charclass("A"), one),
		),
		conc(
			mult(charclass("Z"), one),
			mult(charclass("B"), one),
		),
		conc(
			mult(charclass("Z"), one),
			mult(charclass("Z"), one),
			mult(charclass("C"), one),
		),
	).behead(
		conc(mult(charclass("Z"), one))
	) == pattern(
		conc(
			mult(charclass("Z"), star),
			mult(charclass("A"), one),
		),
		conc(
			mult(charclass("B"), one),
		),
		conc(
			mult(charclass("Z"), one),
			mult(charclass("C"), one),
		),
	)

	# a{2}b|a+c -> a
	assert pattern(
		conc(
			mult(charclass("a"), multiplier(bound(2), bound(2))),
			mult(charclass("b"), one),
		),
		conc(
			mult(charclass("a"), plus),
			mult(charclass("c"), one),
		)
	)._commonconc() == conc(mult(charclass("a"), one))

	# a{2}b|a+c.behead(a) = (ab|a*c)
	assert pattern(
		conc(
			mult(charclass("a"), multiplier(bound(2), bound(2))),
			mult(charclass("b"), one),
		),
		conc(
			mult(charclass("a"), plus),
			mult(charclass("c"), one),
		)
	).behead(
		conc(mult(charclass("a"), one))
	) == pattern(
		conc(
			mult(charclass("a"), one),
			mult(charclass("b"), one),
		),
		conc(
			mult(charclass("a"), star),
			mult(charclass("c"), one),
		),
	)

	# make sure recursion problem in reduce()
	# has gone away
	emptystring + mult(
		pattern(
			conc(mult(charclass("123456789"), one)),
			conc(mult(charclass("0"), one))
		),
		one
	)

	# charclass equality
	assert charclass("a") == charclass("a")
	assert ~charclass("a") == ~charclass("a")
	assert ~charclass("a") != charclass("a")
	assert charclass("ab") == charclass("ba")

	# str(charclass)
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

	# charclass parsing
	assert charclass.match("a", 0) == (charclass("a"), 1)
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

	# charclass negation
	assert ~~charclass("a") == charclass("a")
	assert charclass("a") == ~~charclass("a")

	# charclass union
	# [ab] u [bc] = [abc]
	assert charclass("ab") | charclass("bc") == charclass("abc")
	# [ab] u [^bc] = [^c]
	assert charclass("ab") | ~charclass("bc") == ~charclass("c")
	# [^a] u [bc] = [^a]
	assert ~charclass("ab") | charclass("bc") == ~charclass("a")
	# [^ab] u [^bc] = [^b]
	assert ~charclass("ab") | ~charclass("bc") == ~charclass("b")

	# charclass intersection
	# [ab] n [bc] = [b]
	assert charclass("ab") & charclass("bc") == charclass("b")
	# [ab] n [^bc] = [a]
	assert charclass("ab") & ~charclass("bc") == charclass("a")
	# [^ab] n [bc] = [c]
	assert ~charclass("ab") & charclass("bc") == charclass("c")
	# [^ab] n [^bc] = [^abc]
	assert ~charclass("ab") & ~charclass("bc") == ~charclass("abc")

	# mult equality
	assert mult(charclass("a"), one) == mult(charclass("a"), one)
	assert mult(charclass("a"), one) != mult(charclass("b"), one)
	assert mult(charclass("a"), one) != mult(charclass("a"), qm)
	assert mult(charclass("a"), one) != mult(charclass("a"), multiplier(bound(1), bound(2)))

	# str(mult) tests
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

	# mult parsing
	assert mult.match("[a-g]+", 0) == (
		mult(charclass("abcdefg"), plus),
		6
	)
	assert mult.match("[a-g0-8$%]+", 0) == (
		mult(charclass("abcdefg012345678$%"), plus),
		11
	)
	assert mult.match("[a-g0-8$%\\^]+", 0) == (
		mult(charclass("abcdefg012345678$%^"), plus),
		13
	)
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

	# mult contains a pattern containing an empty conc? Pull the empty
	# part out where it's external
	# e.g. (a|b*|) -> (a|b*)?
	assert mult(
		pattern(
			conc(mult(charclass("a"), one)),
			conc(mult(charclass("b"), star)),
			emptystring
		), one
	).reduce() == mult(
		pattern(
			conc(mult(charclass("a"), one)),
			conc(mult(charclass("b"), star)),
		), qm
	)

	# e.g. (a|b*|)c -> (a|b*)?c
	assert conc(
		mult(
			pattern(
				conc(mult(charclass("a"), one)),
				conc(mult(charclass("b"), star)),
				emptystring
			), one
		),
		mult(charclass("c"), one),
	).reduce() == conc(
		mult(
			pattern(
				conc(mult(charclass("a"), one)),
				conc(mult(charclass("b"), star)),
			), qm
		),
		mult(charclass("c"), one),
	)


	# This happens even if emptystring is the only thing left inside the mult
	assert mult(
		pattern(
			emptystring
		), one
	).reduce() == emptystring

	# mult contains a pattern containing a single conc containing a single mult?
	# that can be reduced greatly
	# e.g. "([ab])*" -> "[ab]*"
	assert mult(
		pattern(
			conc(
				mult(charclass("ab"), one)
			)
		), star
	).reduce() == mult(charclass("ab"), star)
	# e.g. "(c{1,2}){3,4}" -> "c{3,8}"
	assert mult(
		pattern(
			conc(
				mult(charclass("c"), multiplier(bound(1), bound(2)))
			)
		), multiplier(bound(3), bound(4))
	).reduce() == mult(charclass("c"), multiplier(bound(3), bound(8)))

	# recursive mult reduction
	# (a|b)* -> [ab]*
	assert mult(
		pattern(
			conc(mult(charclass("a"), one)),
			conc(mult(charclass("b"), one)),
		), star
	).reduce() == mult(charclass("ab"), star)

	# mult subtraction
	# a{4,5} - a{3} = a{1,2}
	assert mult(
		charclass("a"),
		multiplier(bound(4), bound(5))
	) - mult(
		charclass("a"),
		multiplier(bound(3), bound(3))
	) == mult(
		charclass("a"),
		multiplier(bound(1), bound(2))
	)

	# conc equality
	assert conc(mult(charclass("a"), one)) == conc(mult(charclass("a"), one))
	assert conc(mult(charclass("a"), one)) != conc(mult(charclass("b"), one))
	assert conc(mult(charclass("a"), one)) != conc(mult(charclass("a"), qm))
	assert conc(mult(charclass("a"), one)) != conc(mult(charclass("a"), multiplier(bound(1), bound(2))))
	assert conc(mult(charclass("a"), one)) != emptystring

	# str(conc) tests
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

	# conc parsing
	assert conc.match("abcde[^fg]*h{5}[a-z]+", 0) == (
		conc(
			mult(charclass("a"), one),
			mult(charclass("b"), one),
			mult(charclass("c"), one),
			mult(charclass("d"), one),
			mult(charclass("e"), one),
			mult(~charclass("fg"), star),
			mult(charclass("h"), multiplier(bound(5), bound(5))),
			mult(charclass("abcdefghijklmnopqrstuvwxyz"), plus),
		), 21
	)
	assert conc.match("[bc]*[ab]*", 0) == (
		conc(
			mult(charclass("bc"), star),
			mult(charclass("ab"), star),
		),
		10
	)
	assert conc.match("abc...", 0) == (
		conc(
			mult(charclass("a"), one),
			mult(charclass("b"), one),
			mult(charclass("c"), one),
			mult(dot, one),
			mult(dot, one),
			mult(dot, one),
		),
		6
	)
	assert conc.match("\\d{4}-\\d{2}-\\d{2}", 0) == (
		conc(
			mult(charclass("0123456789"), multiplier(bound(4), bound(4))),
			mult(charclass("-"), one),
			mult(charclass("0123456789"), multiplier(bound(2), bound(2))),
			mult(charclass("-"), one),
			mult(charclass("0123456789"), multiplier(bound(2), bound(2))),
		),
		17
	)

	# conc.reduce()
	assert conc(
		mult(charclass("a"), one),
		mult(charclass(), one),
		mult(charclass("b"), one),
	).reduce() == nothing
	# conc -> conc
	assert conc(
		mult(charclass("a"), one),
		mult(charclass("b"), one),
	).reduce() == conc(
		mult(charclass("a"), one),
		mult(charclass("b"), one),
	)
	# conc -> mult
	assert conc(
		mult(charclass("a"), multiplier(bound(3), bound(4))),
	).reduce() == mult(charclass("a"), multiplier(bound(3), bound(4)))
	# conc -> charclass
	assert conc(
		mult(charclass("a"), one),
	).reduce() == charclass("a")

	# sequence squooshing of mults within a conc
	# e.g. "[$%\\^]?[$%\\^]" -> "[$%\\^]{1,2}"
	assert conc(
		mult(charclass("a"), one),
		mult(charclass("$%^"), qm),
		mult(charclass("$%^"), one),
		mult(charclass("b"), one),
	).reduce() == conc(
		mult(charclass("a"), one),
		mult(charclass("$%^"), multiplier(bound(1), bound(2))),
		mult(charclass("b"), one)
	)

	# recursive conc reduction
	# (a){2}b -> a{2}b
	assert conc(
		mult(
			pattern(
				conc(
					mult(charclass("a"), qm)
				)
			), plus
		),
		mult(charclass("b"), one)
	).reduce() == conc(
		mult(charclass("a"), star),
		mult(charclass("b"), one)
	).reduce()

	# pattern equality
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

	# str(pattern)
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

	# pattern.reduce() tests

	# pattern -> pattern
	# (ab|cd) -> (ab|cd)
	assert pattern(
		conc(
			mult(charclass("a"), multiplier(bound(2), bound(2))),
			mult(charclass("b"), multiplier(bound(2), bound(2))),
		),
		conc(
			mult(charclass("c"), multiplier(bound(2), bound(2))),
			mult(charclass("d"), multiplier(bound(2), bound(2))),
		),
	).reduce() == pattern(
		conc(
			mult(charclass("a"), multiplier(bound(2), bound(2))),
			mult(charclass("b"), multiplier(bound(2), bound(2))),
		),
		conc(
			mult(charclass("c"), multiplier(bound(2), bound(2))),
			mult(charclass("d"), multiplier(bound(2), bound(2))),
		),
	)

	# pattern -> conc
	assert pattern(
		conc(
			mult(charclass("a"), multiplier(bound(2), bound(2))),
			mult(charclass("b"), multiplier(bound(2), bound(2))),
		),
	).reduce() == conc(
		mult(charclass("a"), multiplier(bound(2), bound(2))),
		mult(charclass("b"), multiplier(bound(2), bound(2))),
	)

	# pattern -> mult
	assert pattern(
		conc(
			mult(charclass("a"), multiplier(bound(2), bound(2))),
		),
	).reduce() == mult(charclass("a"), multiplier(bound(2), bound(2)))

	# pattern -> charclass
	assert pattern(
		conc(
			mult(charclass("a"), one),
		),
	).reduce() == charclass("a")

	# special pattern reduction technique.
	# 0|[1-9]|a{5,7} -> [0-9]|a{5,7}
	assert pattern(
		conc(mult(charclass("0"), one)),
		conc(mult(charclass("123456789"), one)),
		conc(mult(charclass("a"), multiplier(bound(5), bound(7)))),
	).reduce() == pattern(
		conc(mult(charclass("0123456789"), one)),
		conc(mult(charclass("a"), multiplier(bound(5), bound(7)))),
	)

	# recursive pattern reduction
	assert pattern(
		conc(mult(charclass("0"), one)),
		conc(
			mult(
				pattern(
					conc(mult(charclass("0"), one)),
					conc(mult(charclass("123456789"), one)),
					conc(mult(charclass("a"), multiplier(bound(5), bound(7)))),
				), one
			)
		)
	).reduce() == pattern(
		conc(mult(charclass("0"), one)),
		conc(
			mult(
				pattern(
					conc(mult(charclass("0123456789"), one)),
					conc(mult(charclass("a"), multiplier(bound(5), bound(7)))),
				), one
			)
		)
	)

	# common prefix reduction of pattern
	# a{2}b|a+c -> a{2}(ab|a*c)
	assert pattern(
		conc(
			mult(charclass("a"), multiplier(bound(2), bound(2))),
			mult(charclass("b"), one),
		),
		conc(
			mult(charclass("a"), plus),
			mult(charclass("c"), one),
		)
	).reduce() == conc(
		mult(charclass("a"), one),
		mult(
			pattern(
				conc(
					mult(charclass("a"), one),
					mult(charclass("b"), one),
				),
				conc(
					mult(charclass("a"), star),
					mult(charclass("c"), one),
				),
			), one
		)
	)

	# pattern parsing
	assert pattern.match("abc|def(ghi|jkl)", 0) == (
		pattern(
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
		), 16
	)

	# charclass multiplication
	# a * 1 = a
	assert charclass("a") * one == charclass("a")
	# a * {1,3} = a{1,3}
	assert charclass("a") * multiplier(bound(1), bound(3)) == mult(charclass("a"), multiplier(bound(1), bound(3)))
	# a * {4,} = a{4,}
	assert charclass("a") * multiplier(bound(4), inf) == mult(charclass("a"), multiplier(bound(4), inf))

	# mult multiplication
	# a{2,3} * 1 = a{2,3}
	assert mult(
		charclass("a"), multiplier(bound(2), bound(3))
	) * one == mult(charclass("a"), multiplier(bound(2), bound(3)))
	# a{2,3} * {4,5} = a{8,15}
	assert mult(
		charclass("a"), multiplier(bound(2), bound(3))
	) * multiplier(bound(4), bound(5)) == mult(charclass("a"), multiplier(bound(8), bound(15)))
	# a{2,} * {2,} = a{4,}
	assert mult(
		charclass("a"), multiplier(bound(2), inf)
	) * multiplier(bound(2), inf) == mult(charclass("a"), multiplier(bound(4), inf))

	# conc multiplication
	# ab? * {0,1} = (ab?)?
	assert conc(
		mult(charclass("a"), one),
		mult(charclass("b"), qm),
	) * qm == mult(
		pattern(
			conc(
				mult(charclass("a"), one),
				mult(charclass("b"), qm),
			),
		), qm
	)

	# pattern multiplication
	# (ab?|ba?) * {2,3} = (ab?|ba?){2,3}
	assert pattern(
		conc(
			mult(charclass("a"), one),
			mult(charclass("b"), qm),
		),
		conc(
			mult(charclass("b"), one),
			mult(charclass("a"), qm),
		),
	) * multiplier(bound(2), bound(3)) == mult(
		pattern(
			conc(
				mult(charclass("a"), one),
				mult(charclass("b"), qm),
			),
			conc(
				mult(charclass("b"), one),
				mult(charclass("a"), qm),
			),
		), multiplier(bound(2), bound(3))
	)

	# bound class tests

	assert min(bound(0), inf) == bound(0)
	assert min(bound(1), inf) == bound(1)
	assert qm.mandatory == bound(0)
	assert qm.optional == bound(1)

	# multiplier intersection operator tests
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

	# a{3,4}, a{2,5} -> a{2,3} (with a{1,1}, a{0,2} left over)
	assert multiplier(bound(3), bound(4)).common(multiplier(bound(2), bound(5))) == multiplier(bound(2), bound(3))
	assert multiplier(bound(3), bound(4)) - multiplier(bound(2), bound(3)) == one
	assert multiplier(bound(2), bound(5)) - multiplier(bound(2), bound(3)) == multiplier(bound(0), bound(2))

	# a{2,}, a{1,5} -> a{1,5} (with a{1,}, a{0,0} left over)
	assert multiplier(bound(2), inf).common(multiplier(bound(1), bound(5))) == multiplier(bound(1), bound(5))
	assert multiplier(bound(2), inf) - multiplier(bound(1), bound(5)) == plus
	assert multiplier(bound(1), bound(5)) - multiplier(bound(1), bound(5)) == zero

	# a{3,}, a{2,} -> a{2,} (with a, epsilon left over)
	assert multiplier(bound(3), inf).common(multiplier(bound(2), inf)) == multiplier(bound(2), inf)
	assert multiplier(bound(3), inf) - multiplier(bound(2), inf) == one
	assert multiplier(bound(2), inf) - multiplier(bound(2), inf) == zero

	# a{3,}, a{3,} -> a{3,} (with zero, zero left over)
	assert multiplier(bound(3), inf).common(multiplier(bound(3), inf)) == multiplier(bound(3), inf)
	assert multiplier(bound(3), inf) - multiplier(bound(3), inf) == zero

	# mult intersection ("&") tests
	# a & b? = nothing
	assert mult(charclass("a"), one) & mult(charclass("b"), qm) == charclass()
	assert mult(charclass("a"), one) & mult(charclass("b"), qm) == nothing
	# a & a? = nothing
	assert mult(charclass('a'), one).reduce() == charclass("a")
	assert mult(charclass("a"), one) & mult(charclass("a"), qm) == charclass("a")
	# a{2} & a{2,} = a{2}
	assert mult(charclass("a"), multiplier(bound(2), bound(2))) \
	& mult(charclass("a"), multiplier(bound(2), inf)) \
	== mult(charclass("a"), multiplier(bound(2), bound(2)))
	# a & b -> no intersection.
	assert mult(charclass("a"), one) & mult(charclass("b"), one) == nothing
	# a & a -> a
	assert mult(charclass("a"), one) & mult(charclass("a"), one) == charclass("a")
	# a* & a -> no intersection
	assert mult(charclass("a"), star) & mult(charclass("a"), one) == charclass("a")
	# a* & b* -> emptystring
	assert mult(charclass("a"), star) & mult(charclass("b"), star) == emptystring
	# a* & a+ -> a+
	assert mult(charclass("a"), star) & mult(charclass("a"), plus) == mult(charclass("a"), plus)
	# aa & aaaa -> []
	assert mult(charclass("a"), multiplier(bound(2), bound(2))) \
	& mult(charclass("a"), multiplier(bound(4), bound(4))) \
	== nothing

	# a{3,4} & a{2,5} -> a{2,3}
	assert mult(
		charclass("a"), multiplier(bound(3), bound(4))
	).common(mult(
		charclass("a"), multiplier(bound(2), bound(5))
	)) == mult(charclass("a"), multiplier(bound(2), bound(3)))

	# a{2,} & a{1,5} -> a{1,5}
	assert mult(
		charclass("a"), multiplier(bound(2), inf)
	).common(mult(
		charclass("a"), multiplier(bound(1), bound(5))
	)) == mult(charclass("a"), multiplier(bound(1), bound(5)))

	# a{3,}, a{2,} -> a{2,} (with a, epsilon left over)
	assert mult(
		charclass("a"), multiplier(bound(3), inf)
	).common(mult(
		charclass("a"), multiplier(bound(2), inf)
	)) == mult(charclass("a"), multiplier(bound(2), inf))

	# a{3,}, a{3,} -> a{3,} (with inf, inf left over)
	assert mult(
		charclass("a"), multiplier(bound(3), inf)
	) & mult(
		charclass("a"), multiplier(bound(3), inf)
	) == mult(charclass("a"), multiplier(bound(3), inf))

	# pattern._commonconc(suffix=True) tests

	# a | bc -> emptystring
	assert pattern(
		conc(
			mult(charclass("a"), one),
		),
		conc(
			mult(charclass("b"), one),
			mult(charclass("c"), one),
		),
	)._commonconc(suffix=True) == emptystring

	# (a|bc) - () = (a|bc)
	assert pattern(
		conc(
			mult(charclass("a"), one),
		),
		conc(
			mult(charclass("b"), one),
			mult(charclass("c"), one),
		),
	) - emptystring == pattern(
		conc(
			mult(charclass("a"), one),
		),
		conc(
			mult(charclass("b"), one),
			mult(charclass("c"), one),
		),
	)

	# (aa|bca) -> a
	assert pattern(
		conc(
			mult(charclass("a"), one),
			mult(charclass("a"), one),
		),
		conc(
			mult(charclass("b"), one),
			mult(charclass("c"), one),
			mult(charclass("a"), one),
		),
	)._commonconc(suffix=True) == conc(mult(charclass("a"), one))

	# (aa|bca) - a = (a|bc)
	assert pattern(
		conc(
			mult(charclass("a"), one),
			mult(charclass("a"), one),
		),
		conc(
			mult(charclass("b"), one),
			mult(charclass("c"), one),
			mult(charclass("a"), one),
		),
	) - conc(mult(charclass("a"), one)) == pattern(
		conc(
			mult(charclass("a"), one),
		),
		conc(
			mult(charclass("b"), one),
			mult(charclass("c"), one),
		),
	)

	# xyza | abca | a -> a
	assert pattern(
		conc(
			mult(charclass("x"), one),
			mult(charclass("y"), one),
			mult(charclass("z"), one),
			mult(charclass("a"), one),
		),
		conc(
			mult(charclass("a"), one),
			mult(charclass("b"), one),
			mult(charclass("c"), one),
			mult(charclass("a"), one),
		),
		conc(
			mult(charclass("a"), one),
		),
	)._commonconc(suffix=True) == conc(mult(charclass("a"), one))

	# (xyza|abca|a) - a = (xyz|abc|)
	assert pattern(
		conc(
			mult(charclass("x"), one),
			mult(charclass("y"), one),
			mult(charclass("z"), one),
			mult(charclass("a"), one),
		),
		conc(
			mult(charclass("a"), one),
			mult(charclass("b"), one),
			mult(charclass("c"), one),
			mult(charclass("a"), one),
		),
		conc(
			mult(charclass("a"), one),
		),
	) - conc(mult(charclass("a"), one)) == pattern(
		emptystring,
		conc(
			mult(charclass("x"), one),
			mult(charclass("y"), one),
			mult(charclass("z"), one),
		),
		conc(
			mult(charclass("a"), one),
			mult(charclass("b"), one),
			mult(charclass("c"), one),
		),
	)

	# f{2,3}c, fc -> fc
	assert pattern(
		conc(
			mult(charclass("f"), multiplier(bound(2), bound(3))),
			mult(charclass("c"), one),
		),
		conc(
			mult(charclass("f"), one),
			mult(charclass("c"), one),
		),
	)._commonconc(suffix=True) == conc(
		mult(charclass("f"), one),
		mult(charclass("c"), one),
	)

	# (f{2,3}c|fc) - fc = (f{1,2}|)
	assert pattern(
		conc(
			mult(charclass("f"), multiplier(bound(2), bound(3))),
			mult(charclass("c"), one),
		),
		conc(
			mult(charclass("f"), one),
			mult(charclass("c"), one),
		),
	) - conc(
		mult(charclass("f"), one),
		mult(charclass("c"), one),
	) == pattern(
		emptystring,
		conc(
			mult(charclass("f"), multiplier(bound(1), bound(2))),
		),
	)

	# (aa) -> aa
	assert pattern(
		conc(
			mult(charclass("a"), one),
			mult(charclass("a"), one),
		),
	)._commonconc(suffix=True) == conc(
		mult(charclass("a"), one),
		mult(charclass("a"), one),
	)

	# (aa) - aa = ()
	assert pattern(
		conc(
			mult(charclass("a"), one),
			mult(charclass("a"), one),
		),
	) - conc(
		mult(charclass("a"), one),
		mult(charclass("a"), one),
	) == pattern(emptystring)

	# concatenation tests (__add__())

	# empty conc + empty conc
	assert emptystring + emptystring == emptystring

	# charclass + charclass
	# a + b = ab
	assert charclass("a") + charclass("b") == conc(
		mult(charclass("a"), one),
		mult(charclass("b"), one),
	)
	# a + a = a{2}
	assert charclass("a") + charclass("a") == mult(charclass("a"), multiplier(bound(2), bound(2)))

	# charclass + mult
	# a + a = a{2}
	assert charclass("a") + mult(charclass("a"), one) == mult(charclass("a"), multiplier(bound(2), bound(2)))
	# a + a{2,} = a{3,}
	assert charclass("a") + mult(charclass("a"), multiplier(bound(2), inf)) == mult(charclass("a"), multiplier(bound(3), inf))
	# a + a{,8} = a{1,9}
	assert charclass("a") + mult(charclass("a"), multiplier(bound(0), bound(8))) == mult(charclass("a"), multiplier(bound(1), bound(9)))
	# a + b{,8} = ab{,8}
	assert charclass("a") + mult(charclass("b"), multiplier(bound(0), bound(8))) == conc(
		mult(charclass("a"), one),
		mult(charclass("b"), multiplier(bound(0), bound(8))),
	)

	# mult + charclass
	# b + b = b{2}
	assert mult(charclass("b"), one) + charclass("b") == mult(charclass("b"), multiplier(bound(2), bound(2)))
	# b* + b = b+
	assert mult(charclass("b"), star) + charclass("b") == mult(charclass("b"), plus)
	 # b{,8} + b = b{1,9}
	assert mult(charclass("b"), multiplier(bound(0), bound(8))) + charclass("b") == mult(charclass("b"), multiplier(bound(1), bound(9)))
	# b{,8} + c = b{,8}c
	assert mult(charclass("b"), multiplier(bound(0), bound(8))) + charclass("c") == conc(
		mult(charclass("b"), multiplier(bound(0), bound(8))),
		mult(charclass("c"), one),
	)

	# charclass + conc
	# a + nothing = a
	assert charclass("a") + emptystring == charclass("a")
	# a + bc = abc
	assert charclass("a") + conc(
		mult(charclass("b"), one),
		mult(charclass("c"), one),
	) == conc(
		mult(charclass("a"), one),
		mult(charclass("b"), one),
		mult(charclass("c"), one),
	)
	# a + ab = a{2}b
	assert charclass("a") + conc(
		mult(charclass("a"), one),
		mult(charclass("b"), one),
	) == conc(
		mult(charclass("a"), multiplier(bound(2), bound(2))),
		mult(charclass("b"), one),
	)

	# conc + charclass
	# nothing + a = a
	assert emptystring + charclass("a") == charclass("a")
	# ab + c = abc
	assert conc(
		mult(charclass("a"), one),
		mult(charclass("b"), one),
	) + charclass("c") == conc(
		mult(charclass("a"), one),
		mult(charclass("b"), one),
		mult(charclass("c"), one),
	)
	# ab + b = ab{2}
	assert conc(
		mult(charclass("a"), one),
		mult(charclass("b"), one),
	) + charclass("b") == conc(
		mult(charclass("a"), one),
		mult(charclass("b"), multiplier(bound(2), bound(2))),
	)

	# pattern + charclass
	# (a|bd) + c = (a|bd)c
	assert pattern(
		conc(
			mult(charclass("a"), one),
		),
		conc(
			mult(charclass("b"), one),
			mult(charclass("d"), one),
		),
	) + charclass("c") == conc(
		mult(
			pattern(
				conc(
					mult(charclass("a"), one),
				),
				conc(
					mult(charclass("b"), one),
					mult(charclass("d"), one),
				),
			), one
		),
		mult(charclass("c"), one),
	)
	# (ac{2}|bc+) + c = (ac|bc*)c{2}
	assert pattern(
		conc(
			mult(charclass("a"), one),
			mult(charclass("c"), multiplier(bound(2), bound(2))),
		),
		conc(
			mult(charclass("b"), one),
			mult(charclass("c"), plus),
		),
	) + charclass("c") == conc(
		mult(
			pattern(
				conc(
					mult(charclass("a"), one),
					mult(charclass("c"), one),
				),
				conc(
					mult(charclass("b"), one),
					mult(charclass("c"), star),
				),
			), one
		),
		mult(charclass("c"), multiplier(bound(2), bound(2))),
	)

	# charclass + pattern
	# a + (b|cd) = a(b|cd)
	assert charclass("a") + pattern(
		conc(
			mult(charclass("b"), one),
		),
		conc(
			mult(charclass("c"), one),
			mult(charclass("d"), one),
		),
	) == conc(
		mult(charclass("a"), one),
		mult(
			pattern(
				conc(
					mult(charclass("b"), one),
				),
				conc(
					mult(charclass("c"), one),
					mult(charclass("d"), one),
				),
			), one
		)
	)
	# a + (a{2}b|a+c) = a{2}(ab|a*c)
	assert charclass("a") + pattern(
		conc(
			mult(charclass("a"), multiplier(bound(2), bound(2))),
			mult(charclass("b"), one),
		),
		conc(
			mult(charclass("a"), plus),
			mult(charclass("c"), one),
		),
	) == conc(
		mult(charclass("a"), multiplier(bound(2), bound(2))),
		mult(
			pattern(
				conc(
					mult(charclass("a"), one),
					mult(charclass("b"), one),
				),
				conc(
					mult(charclass("a"), star),
					mult(charclass("c"), one),
				),
			), one
		),
	)

	# mult + mult
	# a{3,4} + b? = a{3,4}b?
	assert mult(charclass("a"), multiplier(bound(3), bound(4))) + mult(charclass("b"), qm) == conc(
		mult(charclass("a"), multiplier(bound(3), bound(4))),
		mult(charclass("b"), qm),
	)
	# a* + a{2} = a{2,}
	assert mult(charclass("a"), star) + mult(charclass("a"), multiplier(bound(2), bound(2))) == mult(charclass("a"), multiplier(bound(2), inf))

	# mult + conc
	# a{2} + bc = a{2}bc
	assert mult(charclass("a"), multiplier(bound(2), bound(2))) + conc(
		mult(charclass("b"), one),
		mult(charclass("c"), one),
	) == conc(
		mult(charclass("a"), multiplier(bound(2), bound(2))),
		mult(charclass("b"), one),
		mult(charclass("c"), one),
	)
	# a? + ab = a{1,2}b
	assert mult(charclass("a"), qm) + conc(
		mult(charclass("a"), one),
		mult(charclass("b"), one),
	) == conc(
		mult(charclass("a"), multiplier(bound(1), bound(2))),
		mult(charclass("b"), one),
	)

	# conc + mult
	# ab + c* = abc*
	assert conc(
		mult(charclass("a"), one),
		mult(charclass("b"), one),
	) + mult(charclass("c"), star) == conc(
		mult(charclass("a"), one),
		mult(charclass("b"), one),
		mult(charclass("c"), star),
	)
	# ab + b* = ab+
	assert conc(
		mult(charclass("a"), one),
		mult(charclass("b"), one),
	) + mult(charclass("b"), star) == conc(
		mult(charclass("a"), one),
		mult(charclass("b"), plus),
	)

	# mult + pattern
	# a{2,3} + (b|cd) = a{2,3}(b|cd)
	assert mult(charclass("a"), multiplier(bound(2), bound(3))) + pattern(
		conc(
			mult(charclass("b"), one),
		),
		conc(
			mult(charclass("c"), one),
			mult(charclass("d"), one),
		),
	) == conc(
		mult(charclass("a"), multiplier(bound(2), bound(3))),
		mult(
			pattern(
				conc(
					mult(charclass("b"), one),
				),
				conc(
					mult(charclass("c"), one),
					mult(charclass("d"), one),
				),
			), one
		)
	)
	# a{2,3} + (a{2}b|a+c) = a{3,4}(ab|a*c)
	assert mult(charclass("a"), multiplier(bound(2), bound(3))) + pattern(
		conc(
			mult(charclass("a"), multiplier(bound(2), bound(2))),
			mult(charclass("b"), one),
		),
		conc(
			mult(charclass("a"), plus),
			mult(charclass("c"), one),
		),
	) == conc(
		mult(charclass("a"), multiplier(bound(3), bound(4))),
		mult(
			pattern(
				conc(
					mult(charclass("a"), one),
					mult(charclass("b"), one),
				),
				conc(
					mult(charclass("a"), star),
					mult(charclass("c"), one),
				),
			), one
		),
	)

	# pattern + mult
	# (b|cd) + a{2,3} = (b|cd)a{2,3}
	assert pattern(
		conc(
			mult(charclass("b"), one),
		),
		conc(
			mult(charclass("c"), one),
			mult(charclass("d"), one),
		),
	) + mult(charclass("a"), multiplier(bound(2), bound(3))) == conc(
		mult(
			pattern(
				conc(
					mult(charclass("b"), one),
				),
				conc(
					mult(charclass("c"), one),
					mult(charclass("d"), one),
				),
			), one
		),
		mult(charclass("a"), multiplier(bound(2), bound(3))),
	)
	# (ba{2}|ca+) + a{2,3} = (ba|ca*)a{3,4}
	assert pattern(
		conc(
			mult(charclass("b"), one),
			mult(charclass("a"), multiplier(bound(2), bound(2))),
		),
		conc(
			mult(charclass("c"), one),
			mult(charclass("a"), plus),
		),
	) + mult(charclass("a"), multiplier(bound(2), bound(3))) == conc(
		mult(
			pattern(
				conc(
					mult(charclass("b"), one),
					mult(charclass("a"), one),
				),
				conc(
					mult(charclass("c"), one),
					mult(charclass("a"), star),
				),
			), one
		),
		mult(charclass("a"), multiplier(bound(3), bound(4))),
	)

	# conc + conc
	# ab + cd = abcd
	assert conc(
		mult(charclass("a"), one),
		mult(charclass("b"), one),
	) + conc(
		mult(charclass("c"), one),
		mult(charclass("d"), one),
	) == conc(
		mult(charclass("a"), one),
		mult(charclass("b"), one),
		mult(charclass("c"), one),
		mult(charclass("d"), one),
	)
	# ab + bc = ab{2}c
	assert conc(
		mult(charclass("a"), one),
		mult(charclass("b"), one),
	) + conc(
		mult(charclass("b"), one),
		mult(charclass("c"), one),
	) == conc(
		mult(charclass("a"), one),
		mult(charclass("b"), multiplier(bound(2), bound(2))),
		mult(charclass("c"), one),
	)

	# conc + pattern
	# za{2,3} + (b|cd) = za{2,3}(b|cd)
	assert conc(
		mult(charclass("z"), one),
		mult(charclass("a"), multiplier(bound(2), bound(3))),
	) + pattern(
		conc(
			mult(charclass("b"), one),
		),
		conc(
			mult(charclass("c"), one),
			mult(charclass("d"), one),
		),
	) == conc(
		mult(charclass("z"), one),
		mult(charclass("a"), multiplier(bound(2), bound(3))),
		mult(
			pattern(
				conc(
					mult(charclass("b"), one),
				),
				conc(
					mult(charclass("c"), one),
					mult(charclass("d"), one),
				),
			), one,
		)
	)
	# za{2,3} + (a{2}b|a+c) = za{3,4}(ab|a*c)
	assert conc(
		mult(charclass("z"), one),
		mult(charclass("a"), multiplier(bound(2), bound(3))),
	) + pattern(
		conc(
			mult(charclass("a"), multiplier(bound(2), bound(2))),
			mult(charclass("b"), one),
		),
		conc(
			mult(charclass("a"), plus),
			mult(charclass("c"), one),
		),
	) == conc(
		mult(charclass("z"), one),
		mult(charclass("a"), multiplier(bound(3), bound(4))),
		mult(
			pattern(
				conc(
					mult(charclass("a"), one),
					mult(charclass("b"), one),
				),
				conc(
					mult(charclass("a"), star),
					mult(charclass("c"), one),
				),
			), one
		),
	)

	# pattern + conc
	# (b|cd) + za{2,3} = (b|cd)za{2,3}
	assert pattern(
		conc(
			mult(charclass("b"), one),
		),
		conc(
			mult(charclass("c"), one),
			mult(charclass("d"), one),
		),
	) + conc(
		mult(charclass("z"), one),
		mult(charclass("a"), multiplier(bound(2), bound(3))),
	) == conc(
		mult(
			pattern(
				conc(
					mult(charclass("b"), one),
				),
				conc(
					mult(charclass("c"), one),
					mult(charclass("d"), one),
				),
			), one
		),
		mult(charclass("z"), one),
		mult(charclass("a"), multiplier(bound(2), bound(3))),
	)
	# (ba{2}|ca+) + a{2,3}z = (ba|ca*)a{3,4}z
	assert pattern(
		conc(
			mult(charclass("b"), one),
			mult(charclass("a"), multiplier(bound(2), bound(2))),
		),
		conc(
			mult(charclass("c"), one),
			mult(charclass("a"), plus),
		),
	) + conc(
		mult(charclass("a"), multiplier(bound(2), bound(3))),
		mult(charclass("z"), one),
	) == conc(
		mult(
			pattern(
				conc(
					mult(charclass("b"), one),
					mult(charclass("a"), one),
				),
				conc(
					mult(charclass("c"), one),
					mult(charclass("a"), star),
				),
			), one
		),
		mult(charclass("a"), multiplier(bound(3), bound(4))),
		mult(charclass("z"), one),
	)

	# pattern + pattern
	# (a|bc) + (c|de) = (a|bc)(c|de)
	assert pattern(
		conc(
			mult(charclass("a"), one),
		),
		conc(
			mult(charclass("b"), one),
			mult(charclass("c"), one),
		),
	) + pattern(
		conc(
			mult(charclass("c"), one),
		),
		conc(
			mult(charclass("d"), one),
			mult(charclass("e"), one),
		),
	) == conc(
		mult(
			pattern(
				conc(
					mult(charclass("a"), one),
				),
				conc(
					mult(charclass("b"), one),
					mult(charclass("c"), one),
				),
			), one
		),
		mult(
			pattern(
				conc(
					mult(charclass("c"), one),
				),
				conc(
					mult(charclass("d"), one),
					mult(charclass("e"), one),
				),
			), one
		),
	)
	# (a|bc) + (a|bc) = (a|b){2}
	assert pattern(
		conc(
			mult(charclass("a"), one),
		),
		conc(
			mult(charclass("b"), one),
			mult(charclass("c"), one),
		),
	) + pattern(
		conc(
			mult(charclass("a"), one),
		),
		conc(
			mult(charclass("b"), one),
			mult(charclass("c"), one),
		),
	) == mult(
		pattern(
			conc(
				mult(charclass("a"), one),
			),
			conc(
				mult(charclass("b"), one),
				mult(charclass("c"), one),
			),
		), multiplier(bound(2), bound(2))
	)

	assert nothing.empty()
	assert charclass().empty()
	assert not dot.empty()
	assert not mult(charclass("a"), zero).empty()
	assert mult(charclass(), one).empty()
	assert not mult(charclass(), qm).empty()
	assert conc(mult(charclass("a"), one), mult(charclass(), one)).empty()
	assert not conc(mult(charclass("a"), one), mult(charclass(), qm)).empty()
	assert pattern().empty()
	assert not pattern(conc(mult(charclass("a"), zero))).empty()
	assert not pattern(conc(mult(charclass(), qm))).empty()

	assert str(parse("a.b")) == "a.b" # not "a[ab]b"
	assert str(parse("\\d{4}")) == "\\d{4}"

	# Intersection tests
	assert str(parse("a*") & parse("b*")) == ""
	assert str(parse("a") & parse("b")) == "[]"
	assert str(parse("\\d") & parse(".")) == "\\d"
	assert str(parse("\\d{2}") & parse("0.")) == "0\\d"
	assert str(parse("\\d{2}") & parse("19.*")) == "19"
	assert str(parse("\\d{3}") & parse("19.*")) == "19\\d"
	assert str(parse("abc...") & parse("...def")) == "abcdef"
	assert str(parse("[bc]*[ab]*") & parse("[ab]*[bc]*")) in set(["([ab]*a|[bc]*c)?b*", "b*(a[ab]*|c[bc]*)?"])
	assert str(parse("\\W*") & parse("[a-g0-8$%\\^]+") & parse("[^d]{2,8}")) == "[$%\\^]{2,8}"
	assert str(parse("\\d{4}-\\d{2}-\\d{2}") & parse("19.*")) == "19\\d\\d-\\d\\d-\\d\\d"

	# Reduction tests
	assert str(parse("(|(|(|(|(|(|[$%\^])[$%\^])[$%\^])[$%\^])[$%\^])[$%\^])[$%\^][$%\^]")) == "[$%\^]{2,8}"
	assert str(parse("[0-9a-fA-F][0-9a-fA-F][0-9a-fA-F]")) == "[0-9A-Fa-f]{3}"

	# This one is horrendous and we have to jump through some hoops to get to
	# a sensible result. Probably not a good unit test actually.
	long = \
	"(aa|bb*aa)a*|((ab|bb*ab)|(aa|bb*aa)a*b)((ab|bb*ab)|(aa|bb*aa)a*b)*" + \
	"(aa|bb*aa)a*|((ab|bb*ab)|(aa|bb*aa)a*b)((ab|bb*ab)|(aa|bb*aa)a*b)*"
	long = parse(long)
	long = reversed(long.fsm())
	long = reversed(long.lego())
	assert str(long) == "[ab]*a[ab]"
	short = "[ab]*a?b*|[ab]*b?a*"
	assert str(parse(".*") & parse(short).reduce()) == "[ab]*"

	# DEFECT: "0{2}|1{2}" was erroneously reduced() to "[01]{2}"
	bad = parse("0{2}|1{2}").fsm(set(["0", "1", otherchars]))
	assert bad.accepts("00")
	assert bad.accepts("11")
	assert not bad.accepts("01")
	assert str(parse("0|[1-9]|ab")) == "\d|ab"

	# lego.alphabet() should include "otherchars"
	assert parse("").alphabet() == set([otherchars])

	# You should be able to fsm() a single lego piece without supplying a specific
	# alphabet. That should be determinable from context.
	assert str(parse("a.b").fsm().lego()) == "a.b" # not "a[ab]b"

	# A suspiciously familiar example
	bad = parse("0{2}|1{2}").fsm()
	assert bad.accepts("00")
	assert bad.accepts("11")
	assert not bad.accepts("01")
	assert str(parse("0|[1-9]|ab")) == "\d|ab"

	# everythingbut().
	# Regexes are usually gibberish but we make a few claims
	a = parse("a")
	notA = a.everythingbut().fsm()
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

	# epsilon reduction in patterns.
	assert parse("|(ab)*|def").reduce() == parse("(ab)*|def")
	assert parse("|(ab)+|def").reduce() == parse("(ab)*|def")
	assert parse("|.+").reduce() == parse(".*")
	assert parse("|a+|b+") in set([parse("a+|b*"), parse("a*|b+")])

	# Regex reversal
	assert reversed(parse("b")) == parse("b")
	assert reversed(parse("e*")) == parse("e*")
	assert reversed(parse("bear")) == parse("raeb")
	assert reversed(parse("beer")) == parse("reeb")
	assert reversed(parse("abc|def|ghi")) == parse("cba|fed|ihg")
	assert reversed(parse("(abc)*d")) == parse("d(cba)*")

	# Defect: (a{2})* should NOT reduce to a*
	a2 = mult(charclass("a"), multiplier(bound(2), bound(2)))
	a2star = a2 * star
	assert a2star == mult(pattern(conc(a2)), star)

	# Allow "\w", "\d" and "\s" in charclasses
	assert parse("[\w~]*").fsm().accepts("a0~")
	assert parse("[\da]*").fsm().accepts("0129a")
	assert parse("[\s]+").fsm().accepts(" \t \t ")

	# String generators: charclass
	gen = charclass("xyz").strings()
	assert next(gen) == "x"
	assert next(gen) == "y"
	assert next(gen) == "z"
	try:
		next(gen)
		assert False
	except StopIteration:
		assert True

	# Generators: mult
	# One term
	gen = mult(charclass("ab"), one).strings()
	assert next(gen) == "a"
	assert next(gen) == "b"
	try:
		next(gen)
		assert False
	except StopIteration:
		assert True

	# No terms
	gen = mult(charclass("ab"), zero).strings()
	assert next(gen) == ""
	try:
		next(gen)
		assert False
	except StopIteration:
		assert True

	# Many terms
	# [ab]*
	gen = mult(charclass("ab"), star).strings()
	assert next(gen) == ""
	assert next(gen) == "a"
	assert next(gen) == "b"
	assert next(gen) == "aa"
	assert next(gen) == "ab"
	assert next(gen) == "ba"
	assert next(gen) == "bb"
	assert next(gen) == "aaa"

	# Generators: conc
	# [ab][cd]
	gen = conc(mult(charclass("ab"), one), mult(charclass("cd"), one)).strings()
	assert next(gen) == "ac"
	assert next(gen) == "ad"
	assert next(gen) == "bc"
	assert next(gen) == "bd"
	try:
		next(gen)
		assert False
	except StopIteration:
		assert True

	# Generators: pattern
	# [ab]|[cd]
	gen = pattern(
		conc(mult(charclass("ab"), one)),
		conc(mult(charclass("cde"), one)),
	).strings()
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
	gen = parse("abc|def(ghi|jkl)").strings()
	assert next(gen) == "abc"
	assert next(gen) == "defghi"
	assert next(gen) == "defjkl"

	gen = parse("[0-9a-fA-F]{3,10}").strings()
	assert next(gen) == "000"
	assert next(gen) == "001"
	assert next(gen) == "002"

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

	# Generator needs to handle wildcards as well
	gen = parse("a.b").strings(otherchar="*")
	assert next(gen) == "a*b"
	assert next(gen) == "aab"
	assert next(gen) == "abb"
	try:
		next(gen)
		assert False
	except StopIteration:
		assert True

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

	# Problem relating to isinstance(). The class "mult" was occurring as both
	# lego.mult and as __main__.mult and apparently these count as different
	# classes for some reason, so isinstance(m, mult) was returning false.
	starfree = (parse("").everythingbut() + parse("aa") + parse("").everythingbut()).everythingbut()

	assert repr(~charclass("a")) == "~charclass('a')"

	# Should be able to parse e.g. "\\x40"
	assert parse("\\x00") == parse("\x00")
	assert parse("\\x40") == parse("@")
	assert parse("[\\x40]") == parse("[@]")
	assert parse("[\\x41-\\x5a]") == parse("[A-Z]")
	assert str(parse("\\x09")) == "\\t" # escape sequences are not preserved

	# Printing ASCII control characters? You should get hex escapes
	assert str(parse("\\x00")) == "\\x00"

	# Should accept arbitrary ranges of characters in charclasses. No longer
	# limited to alphanumerics. (User beware...)
	assert parse("[z{|}~]") == parse("[z-~]")
	assert str(parse("[\w:;<=>?@\\[\\\\\]\\^`]")) == "[0-z]"
	assert parse("[A-z]") & parse("[^g]") == parse("[A-fh-z]")

	# Accept the "non-capturing group" syntax, "(?: ... )" but give it no
	# special significance
	assert parse("(?:)") == parse("()")
	assert parse("(?:abc|def)") == parse("(abc|def)")
	parse("(:abc)") # should give no problems

if __name__ == "__main__":
	test_lego()

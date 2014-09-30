# -*- coding: utf-8 -*-

if __name__ == "__main__":
	import os
	import sys
	# If you run tests in-place (instead of using py.test), ensure local version is tested!
	sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from greenery.fsm import *

def test_fsm():
	# Buggggs.
	abstar = fsm(
		alphabet = set(['a', None, 'b']),
		states	 = set([0, 1]),
		initial	 = 0,
		finals	 = set([0]),
		map	 = {
			0: {'a': 0, None: 1, 'b': 0},
			1: {'a': 1, None: 1, 'b': 1}
		}
	)
	assert str(abstar.lego()) == "[ab]*"

	adotb = fsm(
		alphabet = set(['a', None, 'b']),
		states	 = set([0, 1, 2, 3, 4]),
		initial	 = 0,
		finals	 = set([4]),
		map	 = {
			0: {'a': 2, None: 1, 'b': 1},
			1: {'a': 1, None: 1, 'b': 1},
			2: {'a': 3, None: 3, 'b': 3},
			3: {'a': 1, None: 1, 'b': 4},
			4: {'a': 1, None: 1, 'b': 1}
		}
	)
	assert str(adotb.lego()) == "a.b"

	from greenery.lego import otherchars

	# Odd bug with fsm.__add__(), exposed by "[bc]*c"
	int5A = fsm(
		alphabet = set(["a", "b", "c", otherchars]),
		states   = set([0, 1]),
		initial  = 1,
		finals   = set([1]),
		map      = {
			0: {otherchars: 0, "a": 0, "b": 0, "c": 0},
			1: {otherchars: 0, "a": 0, "b": 1, "c": 1},
		}
	)
	assert int5A.accepts("")

	int5B = fsm(
		alphabet = set(["a", "b", "c", otherchars]),
		states   = set([0, 1, 2]),
		initial  = 1,
		finals   = set([0]),
		map      = {
			0: {otherchars: 2, "a": 2, "b": 2, "c": 2},
			1: {otherchars: 2, "a": 2, "b": 2, "c": 0},
			2: {otherchars: 2, "a": 2, "b": 2, "c": 2},
		}
	)
	assert int5B.accepts("c")

	int5C = int5A + int5B
	assert int5C.accepts("c")
	# assert int5C.initial == 0

	# fsm.lego()

	# Catch a recursion error
	assert str(fsm(
		alphabet = set(["0", "1"]),
		states   = set([0, 1, 2, 3]),
		initial  = 3,
		finals   = set([1]),
		map      = {
			0: {"0": 1, "1": 1},
			1: {"0": 2, "1": 2},
			2: {"0": 2, "1": 2},
			3: {"0": 0, "1": 2},
		}
	).lego()) == "0[01]"

	# built-ins testing
	assert not null("a").accepts("a")
	assert epsilon("a").accepts("")
	assert not epsilon("a").accepts("a")

	a = fsm(
		alphabet = set(["a", "b"]),
		states   = set([0, 1, "ob"]),
		initial  = 0,
		finals   = set([1]),
		map      = {
			0    : {"a" : 1   , "b" : "ob"},
			1    : {"a" : "ob", "b" : "ob"},
			"ob" : {"a" : "ob", "b" : "ob"},
		},
	)
	assert not a.accepts("")
	assert a.accepts("a")
	assert not a.accepts("b")

	b = fsm(
		alphabet = set(["a", "b"]),
		states   = set([0, 1, "ob"]),
		initial  = 0,
		finals   = set([1]),
		map      = {
			0    : {"a" : "ob", "b" : 1   },
			1    : {"a" : "ob", "b" : "ob"},
			"ob" : {"a" : "ob", "b" : "ob"},
		},
	)
	assert not b.accepts("")
	assert not b.accepts("a")
	assert b.accepts("b")

	# concatenation simple test
	concAA = a + a
	assert not concAA.accepts("")
	assert not concAA.accepts("a")
	assert concAA.accepts("aa")
	assert not concAA.accepts("aaa")

	concAA = epsilon(set(["a", "b"])) + a + a
	assert not concAA.accepts("")
	assert not concAA.accepts("a")
	assert concAA.accepts("aa")
	assert not concAA.accepts("aaa")

	concAB = a + b
	assert not concAB.accepts("")
	assert not concAB.accepts("a")
	assert not concAB.accepts("b")
	assert not concAB.accepts("aa")
	assert concAB.accepts("ab")
	assert not concAB.accepts("ba")
	assert not concAB.accepts("bb")

	# alternation simple test
	altA = a | null(set(["a", "b"]))
	assert not altA.accepts("")
	assert altA.accepts("a")

	altAB = a | b
	assert not altAB.accepts("")
	assert altAB.accepts("a")
	assert altAB.accepts("b")
	assert not altAB.accepts("aa")
	assert not altAB.accepts("ab")
	assert not altAB.accepts("ba")
	assert not altAB.accepts("bb")

	# fsmstar simple test
	starA = a.star()
	assert starA.accepts("")
	assert starA.accepts("a")
	assert not starA.accepts("b")
	assert starA.accepts("aaaaaaaaa")

	# multiplication simple test
	twoA = a * 2
	assert not twoA.accepts("")
	assert not twoA.accepts("a")
	assert twoA.accepts("aa")
	assert not twoA.accepts("aaa")

	zeroA = a * 0
	assert zeroA.accepts("")
	assert not zeroA.accepts("a")

	# intersection simple test
	intAB = a & b
	assert not intAB.accepts("")
	assert not intAB.accepts("a")
	assert not intAB.accepts("b")

	everythingbutA = a.everythingbut()
	assert everythingbutA.accepts("")
	assert not everythingbutA.accepts("a")
	assert everythingbutA.accepts("b")
	assert everythingbutA.accepts("aa")
	assert everythingbutA.accepts("ab")

	# this is "0*1" in heavy disguise. crawl should resolve this duplication
	# Notice how states 2 and 3 behave identically. When resolved together,
	# states 1 and 2&3 also behave identically, so they, too should be resolved
	# (this is impossible to spot before 2 and 3 have been combined).
	merged = fsm(
		alphabet = set(["0", "1"]),
		states   = set([1, 2, 3, 4, "oblivion"]),
		initial  = 1,
		finals   = set([4]),
		map      = {
			1          : {"0" : 2         , "1" : 4         },
			2          : {"0" : 3         , "1" : 4         },
			3          : {"0" : 3         , "1" : 4         },
			4          : {"0" : "oblivion", "1" : "oblivion"},
			"oblivion" : {"0" : "oblivion", "1" : "oblivion"},
		}
	).reduce()
	assert len(merged.states) == 3

	# this is (a*ba)*
	starred = fsm(
		alphabet = set(["a", "b"]),
		states   = set([0, 1, 2, "oblivion"]),
		initial  = 0,
		finals   = set([2]),
		map      = {
			0          : {"a" : 0         , "b" : 1         },
			1          : {"a" : 2         , "b" : "oblivion"},
			2          : {"a" : "oblivion", "b" : "oblivion"},
			"oblivion" : {"a" : "oblivion", "b" : "oblivion"},
		}
	).star()
	assert starred.alphabet == frozenset(["a", "b"])
	assert starred.accepts("")
	assert not starred.accepts("a")
	assert not starred.accepts("b")
	assert not starred.accepts("aa")
	assert starred.accepts("ba")
	assert starred.accepts("aba")
	assert starred.accepts("aaba")
	assert not starred.accepts("aabb")
	assert starred.accepts("abababa")

	# reduce() behaviour test
	# FSM accepts no strings but has 3 states, needs only 1
	asdf = fsm(
		alphabet = set([None]),
		states   = set([0, 1, 2]),
		initial  = 0,
		finals   = set([1]),
		map = {
			0 : {None : 2},
			1 : {None : 2},
			2 : {None : 2},
		},
	)
	asdf = asdf.reduce()
	assert len(asdf.states) == 1

	# FSM reversal
	abc = fsm(
		alphabet = set(["a", "b", "c"]),
		states   = set([0, 1, 2, 3, None]),
		initial  = 0,
		finals   = set([3]),
		map = {
			0    : {"a" : 1   , "b" : None, "c" : None},
			1    : {"a" : None, "b" : 2   , "c" : None},
			2    : {"a" : None, "b" : None, "c" : 3   },
			3    : {"a" : None, "b" : None, "c" : None},
			None : {"a" : None, "b" : None, "c" : None},
		},
	)
	cba = reversed(abc)
	assert cba.accepts("cba")

	# This is (a|b)*a(a|b)
	brzozowski = fsm(
		alphabet = set(["a", "b"]),
		states = set(["A", "B", "C", "D", "E"]),
		initial = "A",
		finals = set(["C", "E"]),
		map = {
			"A" : {"a" : "B", "b" : "D"},
			"B" : {"a" : "C", "b" : "E"},
			"C" : {"a" : "C", "b" : "E"},
			"D" : {"a" : "B", "b" : "D"},
			"E" : {"a" : "B", "b" : "D"},
		},
	)
	assert brzozowski.accepts("aa")
	assert brzozowski.accepts("ab")
	assert brzozowski.accepts("aab")
	assert brzozowski.accepts("bab")
	assert brzozowski.accepts("abbbbbbbab")
	assert not brzozowski.accepts("")
	assert not brzozowski.accepts("a")
	assert not brzozowski.accepts("b")
	assert not brzozowski.accepts("ba")
	assert not brzozowski.accepts("bb")
	assert not brzozowski.accepts("bbbbbbbbbbbb")

	# So this is (a|b)a(a|b)*
	b2 = reversed(brzozowski)
	assert b2.accepts("aa")
	assert b2.accepts("ba")
	assert b2.accepts("baa")
	assert b2.accepts("bab")
	assert b2.accepts("babbbbbbba")
	assert not b2.accepts("")
	assert not b2.accepts("a")
	assert not b2.accepts("b")
	assert not b2.accepts("ab")
	assert not b2.accepts("bb")
	assert not b2.accepts("bbbbbbbbbbbb")

	# Test string generator functionality.
	gen = b2.strings()
	assert next(gen) == ["a", "a"]
	assert next(gen) == ["b", "a"]
	assert next(gen) == ["a", "a", "a"]
	assert next(gen) == ["a", "a", "b"]
	assert next(gen) == ["b", "a", "a"]
	assert next(gen) == ["b", "a", "b"]
	assert next(gen) == ["a", "a", "a", "a"]

	# epsilon reversed is epsilon
	assert reversed(epsilon("a")).accepts("")

	# Bug fix. This is a(a{2})* (i.e. accepts an odd number of "a" chars in a
	# row), but when .lego() is called, the result is "a+". Turned out to be
	# a fault in the lego.multiplier.__mul__() routine
	elesscomplex = fsm(
		alphabet = set(["a"]),
		states = set([0, 1]),
		initial = 0,
		finals = set([1]),
		map = {
			0 : {"a" : 1},
			1 : {"a" : 0},
		},
	)
	assert not elesscomplex.accepts("")
	assert elesscomplex.accepts("a")
	assert not elesscomplex.accepts("aa")
	assert elesscomplex.accepts("aaa")
	elesscomplex = elesscomplex.lego()
	assert str(elesscomplex) in set(["a(aa)*", "(aa)*a"])
	elesscomplex = elesscomplex.fsm()
	assert not elesscomplex.accepts("")
	assert elesscomplex.accepts("a")
	assert not elesscomplex.accepts("aa")
	assert elesscomplex.accepts("aaa")
	gen = elesscomplex.strings()
	assert next(gen) == ["a"]
	assert next(gen) == ["a", "a", "a"]
	assert next(gen) == ["a", "a", "a", "a", "a"]
	assert next(gen) == ["a", "a", "a", "a", "a", "a", "a"]

	# Binary numbers divisible by 3.
	# Disallows the empty string
	# Allows "0" on its own, but not leading zeroes.
	div3 = fsm(
		alphabet = set(["0", "1"]),
		states = set(["initial", "zero", 0, 1, 2, None]),
		initial = "initial",
		finals = set(["zero", 0]),
		map = {
			"initial" : {"0" : "zero", "1" : 1   },
			"zero"    : {"0" : None  , "1" : None},
			0         : {"0" : 0     , "1" : 1   },
			1         : {"0" : 2     , "1" : 0   },
			2         : {"0" : 1     , "1" : 2   },
			None      : {"0" : None  , "1" : None},
		},
	)
	assert not div3.accepts("")
	assert div3.accepts("0")
	assert not div3.accepts("1")
	assert not div3.accepts("00")
	assert not div3.accepts("01")
	assert not div3.accepts("10")
	assert div3.accepts("11")
	assert not div3.accepts("000")
	assert not div3.accepts("001")
	assert not div3.accepts("010")
	assert not div3.accepts("011")
	assert not div3.accepts("100")
	assert not div3.accepts("101")
	assert div3.accepts("110")
	assert not div3.accepts("111")
	assert not div3.accepts("0000")
	assert not div3.accepts("0001")
	assert not div3.accepts("0010")
	assert not div3.accepts("0011")
	assert not div3.accepts("0100")
	assert not div3.accepts("0101")
	assert not div3.accepts("0110")
	assert not div3.accepts("0111")
	assert not div3.accepts("1000")
	assert div3.accepts("1001")
	div3 = div3.lego()
	assert str(div3) == "0|1(01*0|10*1)*10*"
	gen = div3.strings()
	assert next(gen) == "0"
	assert next(gen) == "11"
	assert next(gen) == "110"
	assert next(gen) == "1001"
	assert next(gen) == "1100"

	# Machine accepts only numbers in selected base (e.g. 2, 10) that are
	# divisible by N (e.g. 3, 7).
	# "0" alone is acceptable, but leading zeroes (e.g. "00", "07") are not
	base = 2
	N = 3
	assert base <= 10
	divN = fsm(
		alphabet = set(str(i) for i in range(base)),
		states = set(range(N)) | set(["initial", "zero", None]),
		initial = "initial",
		finals = set(["zero", 0]),
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
	)
	gen = divN.lego().strings()
	a = next(gen)
	assert a == "0"
	for i in range(7):
		b = next(gen)
		assert int(a, base) + N == int(b, base)
		a = b

if __name__ == "__main__":
	test_fsm()

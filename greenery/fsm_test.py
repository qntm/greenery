# -*- coding: utf-8 -*-

if __name__ == "__main__":
	raise Exception("Test files can't be run directly. Use `python -m pytest greenery`")

import pytest
from greenery.fsm import fsm, null, epsilon, anything_else

def test_addbug():
	# Odd bug with fsm.__add__(), exposed by "[bc]*c"
	int5A = fsm(
		alphabet = {"a", "b", "c", anything_else},
		states   = {0, 1},
		initial  = 1,
		finals   = {1},
		map      = {
			0: {anything_else: 0, "a": 0, "b": 0, "c": 0},
			1: {anything_else: 0, "a": 0, "b": 1, "c": 1},
		}
	)
	assert int5A.accepts("")

	int5B = fsm(
		alphabet = {"a", "b", "c", anything_else},
		states   = {0, 1, 2},
		initial  = 1,
		finals   = {0},
		map      = {
			0: {anything_else: 2, "a": 2, "b": 2, "c": 2},
			1: {anything_else: 2, "a": 2, "b": 2, "c": 0},
			2: {anything_else: 2, "a": 2, "b": 2, "c": 2},
		}
	)
	assert int5B.accepts("c")

	int5C = int5A + int5B
	assert int5C.accepts("c")
	# assert int5C.initial == 0

def test_builtins():
	assert not null("a").accepts("a")
	assert epsilon("a").accepts("")
	assert not epsilon("a").accepts("a")

@pytest.fixture
def a():
	a = fsm(
		alphabet = {"a", "b"},
		states   = {0, 1, "ob"},
		initial  = 0,
		finals   = {1},
		map      = {
			0    : {"a" : 1   , "b" : "ob"},
			1    : {"a" : "ob", "b" : "ob"},
			"ob" : {"a" : "ob", "b" : "ob"},
		},
	)
	return a

def test_a(a):
	assert not a.accepts("")
	assert a.accepts("a")
	assert not a.accepts("b")

@pytest.fixture
def b():
	b = fsm(
		alphabet = {"a", "b"},
		states   = {0, 1, "ob"},
		initial  = 0,
		finals   = {1},
		map      = {
			0    : {"a" : "ob", "b" : 1   },
			1    : {"a" : "ob", "b" : "ob"},
			"ob" : {"a" : "ob", "b" : "ob"},
		},
	)
	return b

def test_b(b):
	assert not b.accepts("")
	assert not b.accepts("a")
	assert b.accepts("b")

def test_concatenation_aa(a):
	concAA = a + a
	assert not concAA.accepts("")
	assert not concAA.accepts("a")
	assert concAA.accepts("aa")
	assert not concAA.accepts("aaa")

	concAA = epsilon({"a", "b"}) + a + a
	assert not concAA.accepts("")
	assert not concAA.accepts("a")
	assert concAA.accepts("aa")
	assert not concAA.accepts("aaa")

def test_concatenation_ab(a, b):
	concAB = a + b
	assert not concAB.accepts("")
	assert not concAB.accepts("a")
	assert not concAB.accepts("b")
	assert not concAB.accepts("aa")
	assert concAB.accepts("ab")
	assert not concAB.accepts("ba")
	assert not concAB.accepts("bb")

def test_alternation_a(a):
	altA = a | null({"a", "b"})
	assert not altA.accepts("")
	assert altA.accepts("a")

def test_alternation_ab(a, b):
	altAB = a | b
	assert not altAB.accepts("")
	assert altAB.accepts("a")
	assert altAB.accepts("b")
	assert not altAB.accepts("aa")
	assert not altAB.accepts("ab")
	assert not altAB.accepts("ba")
	assert not altAB.accepts("bb")

def test_star(a):
	starA = a.star()
	assert starA.accepts("")
	assert starA.accepts("a")
	assert not starA.accepts("b")
	assert starA.accepts("aaaaaaaaa")

def test_multiply_0(a):
	zeroA = a * 0
	assert zeroA.accepts("")
	assert not zeroA.accepts("a")

def test_multiply_1(a):
	oneA = a * 1
	assert not oneA.accepts("")
	assert oneA.accepts("a")
	assert not oneA.accepts("aa")

def test_multiply_2(a):
	twoA = a * 2
	assert not twoA.accepts("")
	assert not twoA.accepts("a")
	assert twoA.accepts("aa")
	assert not twoA.accepts("aaa")

def test_multiply_7(a):
	sevenA = a * 7
	assert not sevenA.accepts("aaaaaa")
	assert sevenA.accepts("aaaaaaa")
	assert not sevenA.accepts("aaaaaaaa")

def test_optional_mul(a, b):
	unit = a + b
	# accepts "ab"

	optional = (epsilon(a.alphabet) | unit)
	# accepts "(ab)?
	assert optional.accepts([])
	assert not optional.accepts(["a"])
	assert not optional.accepts(["b"])
	assert optional.accepts(["a", "b"])
	assert not optional.accepts(["a", "a"])

	optional = optional * 2
	# accepts "(ab)?(ab)?"
	assert optional.accepts([])
	assert not optional.accepts(["a"])
	assert not optional.accepts(["b"])
	assert not optional.accepts(["a", "a"])
	assert optional.accepts(["a", "b"])
	assert not optional.accepts(["b", "a"])
	assert not optional.accepts(["b", "b"])
	assert not optional.accepts(["a", "a", "a"])
	assert optional.accepts(["a", "b", "a", "b"])

def test_intersection_ab(a, b):
	intAB = a & b
	assert not intAB.accepts("")
	assert not intAB.accepts("a")
	assert not intAB.accepts("b")

def test_negation(a):
	everythingbutA = a.everythingbut()
	assert everythingbutA.accepts("")
	assert not everythingbutA.accepts("a")
	assert everythingbutA.accepts("b")
	assert everythingbutA.accepts("aa")
	assert everythingbutA.accepts("ab")

def test_crawl_reduction():
	# this is "0*1" in heavy disguise. crawl should resolve this duplication
	# Notice how states 2 and 3 behave identically. When resolved together,
	# states 1 and 2&3 also behave identically, so they, too should be resolved
	# (this is impossible to spot before 2 and 3 have been combined).
	# Finally, the oblivion state should be omitted.
	merged = fsm(
		alphabet = {"0", "1"},
		states   = {1, 2, 3, 4, "oblivion"},
		initial  = 1,
		finals   = {4},
		map      = {
			1          : {"0" : 2         , "1" : 4         },
			2          : {"0" : 3         , "1" : 4         },
			3          : {"0" : 3         , "1" : 4         },
			4          : {"0" : "oblivion", "1" : "oblivion"},
			"oblivion" : {"0" : "oblivion", "1" : "oblivion"},
		}
	).reduce()
	assert len(merged.states) == 2

def test_star_advanced():
	# This is (a*ba)*. Naively connecting the final states to the initial state
	# gives the incorrect result here.
	starred = fsm(
		alphabet = {"a", "b"},
		states   = {0, 1, 2, "oblivion"},
		initial  = 0,
		finals   = {2},
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

def test_reduce():
	# FSM accepts no strings but has 3 states, needs only 1
	asdf = fsm(
		alphabet = {None},
		states   = {0, 1, 2},
		initial  = 0,
		finals   = {1},
		map = {
			0 : {None : 2},
			1 : {None : 2},
			2 : {None : 2},
		},
	)
	asdf = asdf.reduce()
	assert len(asdf.states) == 1

def test_reverse_abc():
	abc = fsm(
		alphabet = {"a", "b", "c"},
		states   = {0, 1, 2, 3, None},
		initial  = 0,
		finals   = {3},
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

def test_reverse_brzozowski():
	# This is (a|b)*a(a|b)
	brzozowski = fsm(
		alphabet = {"a", "b"},
		states = {"A", "B", "C", "D", "E"},
		initial = "A",
		finals = {"C", "E"},
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

def test_reverse_epsilon():
	# epsilon reversed is epsilon
	assert reversed(epsilon("a")).accepts("")

def test_binary_3():
	# Binary numbers divisible by 3.
	# Disallows the empty string
	# Allows "0" on its own, but not leading zeroes.
	div3 = fsm(
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

def test_invalid_fsms():
	# initial state 1 is not a state
	try:
		fsm(
			alphabet = {},
			states = {},
			initial = 1,
			finals = set(),
			map = {}
		)
		assert False
	except AssertionError:
		assert False
	except Exception:
		pass

	# final state 2 not a state
	try:
		fsm(
			alphabet = {},
			states = {1},
			initial = 1,
			finals = {2},
			map = {}
		)
		assert False
	except AssertionError:
		assert False
	except Exception:
		pass

	# invalid transition for state 1, symbol "a"
	try:
		fsm(
			alphabet = {"a"},
			states = {1},
			initial = 1,
			finals = set(),
			map = {
				1 : {"a" : 2}
			}
		)
		assert False
	except AssertionError:
		assert False
	except Exception:
		pass

def test_bad_multiplier(a):
	try:
		x = a * -1
		assert False
	except AssertionError:
		assert False
	except Exception:
		pass

def test_anything_else_acceptance():
	a = fsm(
		alphabet = {"a", "b", "c", anything_else},
		states = {1},
		initial = 1,
		finals = {1},
		map = {
			1 : {"a" : 1, "b" : 1, "c" : 1, anything_else : 1}
		},
	)
	assert a.accepts("d")

def test_difference(a, b):
	aorb = fsm(
		alphabet = {"a", "b"},
		states = {0, 1, None},
		initial = 0,
		finals = {1},
		map = { 
			0    : {"a" : 1   , "b" : 1   },
			1    : {"a" : None, "b" : None},
			None : {"a" : None, "b" : None},
		},
	)

	assert list((a ^ a).strings()) == []
	assert list((b ^ b).strings()) == []
	assert list((a ^ b).strings()) == [["a"], ["b"]]
	assert list((aorb ^ a).strings()) == [["b"]]

def test_empty(a, b):
	assert not a.empty()
	assert not b.empty()

	assert fsm(
		alphabet = {},
		states = {0, 1},
		initial = 0,
		finals = {1},
		map = {0:{}, 1:{}},
	).empty()

	assert not fsm(
		alphabet = {},
		states = {0},
		initial = 0,
		finals = {0},
		map = {0:{}},
	).empty()

	assert fsm(
		alphabet = {"a", "b"},
		states = {0, 1, None, 2},
		initial = 0,
		finals = {2},
		map = { 
			0    : {"a" : 1   , "b" : 1   },
			1    : {"a" : None, "b" : None},
			None : {"a" : None, "b" : None},
			2    : {"a" : None, "b" : None},
		},
	).empty()

def test_equivalent(a, b):
	assert (a | b).equivalent(b | a)

def test_dead_default():
	'''
		You may now omit a transition, or even an entire state, from the map. This
		affects every usage of `fsm.map`.
	'''
	blockquote = fsm(
		alphabet = {"/", "*", anything_else},
		states = {0, 1, 2, 3, 4, 5},
		initial = 0,
		finals = {4},
		map = {
				0 : {"/" : 1},
				1 : {"*" : 2},
				2 : {"/" : 2, anything_else : 2, "*" : 3},
				3 : {"/" : 4, anything_else : 2, "*" : 3},
		}
	)
	assert blockquote.accepts(["/", "*", "whatever", "*", "/"])
	assert not blockquote.accepts(["*", "*", "whatever", "*", "/"])
	str(blockquote) # test stringification
	blockquote | blockquote
	blockquote & blockquote
	blockquote ^ blockquote
	reversed(blockquote)
	assert not blockquote.everythingbut().accepts(["/", "*", "whatever", "*", "/"])
	assert blockquote.everythingbut().accepts(["*"]) # deliberately seek oblivion
	assert blockquote.islive(3)
	assert blockquote.islive(4)
	assert not blockquote.islive(5)
	gen = blockquote.strings()
	assert next(gen) == ["/", "*", "*", "/"]

def test_alphabet_unions():
	# Thanks to sparse maps it should now be possible to compute the union of FSMs
	# with disagreeing alphabets!
	a = fsm(
		alphabet = {"a"},
		states   = {0, 1},
		initial  = 0,
		finals   = {1},
		map      = {
			0    : {"a" : 1},
		},
	)

	b = fsm(
		alphabet = {"b"},
		states   = {0, 1},
		initial  = 0,
		finals   = {1},
		map      = {
			0    : {"b" : 1},
		},
	)

	assert (a | b).accepts(["a"])
	assert (a | b).accepts(["b"])
	assert (a & b).empty()
	assert (a + b).accepts(["a", "b"])
	assert (a ^ b).accepts(["a"])
	assert (a ^ b).accepts(["b"])

def test_repr():
	assert repr(anything_else) == "anything_else"
	assert str(anything_else) == "anything_else"

def test_new_set_methods(a, b):
	# A whole bunch of new methods were added to the FSM module to enable FSMs to
	# function exactly as if they were sets of strings (symbol lists), see:
	# https://docs.python.org/3/library/stdtypes.html#set-types-set-frozenset
	# But do they work?
	assert len(a) == 1
	assert len((a | b) * 4) == 16
	try:
		len(a.star())
		assert False
	except OverflowError:
		pass

	# "in"
	assert "a" in a
	assert not "a" in b
	assert "a" not in b

	# List comprehension!
	four = (a | b) * 2
	for string in four:
		assert string == ["a", "a"]
		break
	assert [s for s in four] == [["a", "a"], ["a", "b"], ["b", "a"], ["b", "b"]]

	# set.union() imitation
	assert fsm.union(a, b) == a.union(b)
	assert len(fsm.union()) == 0
	assert fsm.intersection(a, b) == a.intersection(b)

	# This takes a little explaining. In general, `a & b & c` is equivalent to
	# `EVERYTHING & a & b & c` where `EVERYTHING` is an FSM accepting every
	# possible string. Similarly `a` is equivalent to `EVERYTHING & a`, and the
	# intersection of no sets at all is... `EVERYTHING`.
	# However, since we compute the union of alphabets, and there are no
	# alphabets, the union is the empty set. So the only string which `EVERYTHING`
	# actually recognises is the empty string, [] (or "" if you prefer).
	int_none = fsm.intersection()
	assert len(int_none) == 1
	assert [] in int_none

	assert (a | b).difference(a) == fsm.difference((a | b), a) == (a | b) - a == b
	assert (a | b).difference(a, b) == fsm.difference((a | b), a, b) == (a | b) - a - b == null("ab")
	assert a.symmetric_difference(b) == fsm.symmetric_difference(a, b) == a ^ b
	assert a.isdisjoint(b)
	assert a <= (a | b)
	assert a < (a | b)
	assert a != (a | b)
	assert (a | b) > a
	assert (a | b) >= a

	assert list(a.concatenate(a, a).strings()) == [["a", "a", "a"]]
	assert list(a.concatenate().strings()) == [["a"]]
	assert list(fsm.concatenate(b, a, b).strings()) == [["b", "a", "b"]]
	assert list(fsm.concatenate().strings()) == []
	assert not a.copy() is a

def test_oblivion_crawl(a):
	# When crawling a new FSM, we should avoid generating an oblivion state.
	# `abc` has no oblivion state... all the results should not as well!
	abc = fsm(
		alphabet = {"a", "b", "c"},
		states = {0, 1, 2, 3},
		initial = 0,
		finals = {3},
		map = {
			0 : {"a" : 1},
			1 : {"b" : 2},
			2 : {"c" : 3},
		}
	)
	assert len((abc + abc).states) == 7
	assert len(abc.star().states) == 3
	assert len((abc * 3).states) == 10
	assert len(reversed(abc).states) == 4
	assert len((abc | abc).states) == 4
	assert len((abc & abc).states) == 4
	assert len((abc ^ abc).states) == 1
	assert len((abc - abc).states) == 1

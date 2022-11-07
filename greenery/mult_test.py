# -*- coding: utf-8 -*-

from .fsm import ANYTHING_ELSE
from .charclass import Charclass, DIGIT
from .bound import Bound, INF
from .multiplier import Multiplier, ONE, QM, STAR, PLUS
from .rxelems import Mult

def test_mult_equality():
    assert Mult(Charclass("a"), ONE) == Mult(Charclass("a"), ONE)
    assert Mult(Charclass("a"), ONE) != Mult(Charclass("b"), ONE)
    assert Mult(Charclass("a"), ONE) != Mult(Charclass("a"), QM)
    assert Mult(Charclass("a"), ONE) != Mult(Charclass("a"), Multiplier(Bound(1), Bound(2)))
    assert Mult(Charclass("a"), ONE) != Charclass("a")

def test_mult_str():
    a = Charclass("a")
    assert str(Mult(a, ONE)) == "a"
    assert str(Mult(a, Multiplier(Bound(2), Bound(2)))) == "a{2}"
    assert str(Mult(a, Multiplier(Bound(3), Bound(3)))) == "a{3}"
    assert str(Mult(a, Multiplier(Bound(4), Bound(4)))) == "a{4}"
    assert str(Mult(a, Multiplier(Bound(5), Bound(5)))) == "a{5}"
    assert str(Mult(a, QM)) == "a?"
    assert str(Mult(a, STAR)) == "a*"
    assert str(Mult(a, PLUS)) == "a+"
    assert str(Mult(a, Multiplier(Bound(2), Bound(5)))) == "a{2,5}"
    assert str(Mult(a, Multiplier(Bound(2), INF))) == "a{2,}"
    assert str(Mult(DIGIT, ONE)) == "\\d"
    assert str(Mult(DIGIT, Multiplier(Bound(2), Bound(2)))) == "\\d{2}"
    assert str(Mult(DIGIT, Multiplier(Bound(3), Bound(3)))) == "\\d{3}"

def test_odd_bug():
    # Odd bug with ([bc]*c)?[ab]*
    int5A = Mult(Charclass("bc"), STAR).to_fsm({"a", "b", "c", ANYTHING_ELSE})
    assert int5A.accepts([])
    assert int5A.accepts("")
    int5B = Mult(Charclass("c"), ONE).to_fsm({"a", "b", "c", ANYTHING_ELSE})
    assert int5B.accepts("c")
    assert int5B.accepts(["c"])
    int5C = int5A + int5B
    assert int5C.accepts("c")
    assert int5C.accepts(["c"])

def test_mult_common():
    assert Mult(Charclass("a"), Multiplier(Bound(3), Bound(4))) \
        .common(Mult(Charclass("a"), Multiplier(Bound(2), Bound(5)))) == \
        Mult(Charclass("a"), Multiplier(Bound(2), Bound(3)))
    assert Mult(Charclass("a"), Multiplier(Bound(2), INF)) \
        .common(Mult(Charclass("a"), Multiplier(Bound(1), Bound(5)))) == \
        Mult(Charclass("a"), Multiplier(Bound(1), Bound(5)))
    assert Mult(Charclass("a"), Multiplier(Bound(3), INF)) \
        .common(Mult(Charclass("a"), Multiplier(Bound(2), INF))) == \
        Mult(Charclass("a"), Multiplier(Bound(2), INF))

def test_mult_dock():
    assert Mult(Charclass("a"), Multiplier(Bound(4), Bound(5))) \
        .dock(Mult(Charclass("a"), Multiplier(Bound(3), Bound(3)))) == \
        Mult(Charclass("a"), Multiplier(Bound(1), Bound(2)))

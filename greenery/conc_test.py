# -*- coding: utf-8 -*-

from .bound import Bound
from .multiplier import Multiplier, ZERO, QM, ONE, STAR, PLUS
from .charclass import Charclass
from .rxelems import Conc, Mult, EMPTYSTRING, Multiplicand

def test_conc_equality():
    a = Conc(Mult(Multiplicand(Charclass("a")), ONE))
    assert a == Conc(Mult(Multiplicand(Charclass("a")), ONE))
    assert a != Conc(Mult(Multiplicand(Charclass("b")), ONE))
    assert a != Conc(Mult(Multiplicand(Charclass("a")), QM))
    assert a != Conc(Mult(
        Multiplicand(Charclass("a")),
        Multiplier(Bound(1), Bound(2)))
    )
    assert a != EMPTYSTRING

def test_conc_str():
    assert str(Conc(
        Mult(Multiplicand(Charclass("a")), ONE),
        Mult(Multiplicand(Charclass("b")), ONE),
        Mult(Multiplicand(Charclass("c")), ONE),
        Mult(Multiplicand(Charclass("d")), ONE),
        Mult(Multiplicand(Charclass("e")), ONE),
        Mult(Multiplicand(~Charclass("fg")), STAR),
        Mult(Multiplicand(Charclass("h")), Multiplier(Bound(5), Bound(5))),
        Mult(Multiplicand(Charclass("abcdefghijklmnopqrstuvwxyz")), PLUS),
    )) == "abcde[^fg]*h{5}[a-z]+"

def test_conc_common():
    a = Mult(Multiplicand(Charclass("A")), ONE)
    b = Mult(Multiplicand(Charclass("B")), ONE)
    c = Mult(Multiplicand(Charclass("C")), ONE)
    y = Mult(Multiplicand(Charclass("y")), ONE)
    z = Mult(Multiplicand(Charclass("Z")), ONE)
    zstar = Mult(Multiplicand(Charclass("Z")), STAR)

    assert Conc(a, a, z, y).common(Conc(b, b, z, y), suffix=True) == Conc(z, y)
    assert Conc(c, z).common(Conc(c, z), suffix=True) == Conc(c, z)
    assert Conc(c, y).common(Conc(c, z), suffix=True) == Conc()
    assert Conc(a, z).common(Conc(b, z), suffix=True) == Conc(z)
    assert Conc(a, zstar).common(Conc(b, z), suffix=True) == Conc()
    assert Conc(a).common(Conc(b), suffix=True) == Conc()

def test_conc_dock():
    a = Mult(Multiplicand(Charclass("A")), ONE)
    b = Mult(Multiplicand(Charclass("B")), ONE)
    x = Mult(Multiplicand(Charclass("X")), ONE)
    x2 = Mult(Multiplicand(Charclass("X")), Multiplier(Bound(2), Bound(2)))
    yplus = Mult(Multiplicand(Charclass("y")), PLUS)
    z = Mult(Multiplicand(Charclass("Z")), ONE)

    assert Conc(a, z).dock(Conc(z)) == Conc(a)
    assert Conc(a, b, x, yplus, z).dock(Conc(x, yplus, z)) == Conc(a, b)
    assert Conc(a, b, x, yplus, z).behead(Conc(a, b, x, yplus)) == Conc(z)
    assert Conc(a).dock(Conc()) == Conc(a)

    try:
        Conc(x2, yplus, z).behead(Conc(x, yplus))
        assert False
    except AssertionError:
        assert False
    except Exception:
        pass

def test_mult_reduction_easy():
    assert Conc(Mult(
        Multiplicand(Charclass("a")),
        ZERO
    )).reduce() == Conc()

from __future__ import annotations

import pytest

from .bound import Bound
from .charclass import Charclass
from .multiplier import ONE, PLUS, QM, STAR, ZERO, Multiplier
from .rxelems import Conc, Mult


def test_conc_equality() -> None:
    a = Conc(Mult(Charclass((("a", "a"),)), ONE))
    assert a == Conc(Mult(Charclass((("a", "a"),)), ONE))
    assert a != Conc(Mult(Charclass((("b", "b"),)), ONE))
    assert a != Conc(Mult(Charclass((("a", "a"),)), QM))
    assert a != Conc(Mult(Charclass((("a", "a"),)), Multiplier(Bound(1), Bound(2))))
    assert a != Conc()


def test_conc_str() -> None:
    assert (
        str(
            Conc(
                Mult(Charclass((("a", "a"),)), ONE),
                Mult(Charclass((("b", "b"),)), ONE),
                Mult(Charclass((("c", "c"),)), ONE),
                Mult(Charclass((("d", "d"),)), ONE),
                Mult(Charclass((("e", "e"),)), ONE),
                Mult(~Charclass((("f", "f"), ("g", "g"))), STAR),
                Mult(Charclass((("h", "h"),)), Multiplier(Bound(5), Bound(5))),
                Mult(Charclass(
                    tuple((char, char) for char in "abcdefghijklmnopqrstuvwxyz")
                ), PLUS),
            )
        )
        == "abcde[^fg]*h{5}[a-z]+"
    )


def test_conc_common() -> None:
    a = Mult(Charclass((("A", "A"),)), ONE)
    b = Mult(Charclass((("B", "B"),)), ONE)
    c = Mult(Charclass((("C", "C"),)), ONE)
    y = Mult(Charclass((("y", "y"),)), ONE)
    z = Mult(Charclass((("Z", "Z"),)), ONE)
    zstar = Mult(Charclass((("Z", "Z"),)), STAR)

    assert Conc(a, a, z, y).common(Conc(b, b, z, y), suffix=True) == Conc(z, y)
    assert Conc(c, z).common(Conc(c, z), suffix=True) == Conc(c, z)
    assert Conc(c, y).common(Conc(c, z), suffix=True) == Conc()
    assert Conc(a, z).common(Conc(b, z), suffix=True) == Conc(z)
    assert Conc(a, zstar).common(Conc(b, z), suffix=True) == Conc()
    assert Conc(a).common(Conc(b), suffix=True) == Conc()


def test_conc_dock() -> None:
    a = Mult(Charclass((("A", "A"),)), ONE)
    b = Mult(Charclass((("B", "B"),)), ONE)
    x = Mult(Charclass((("X", "X"),)), ONE)
    x_twice = Mult(Charclass((("X", "X"),)), Multiplier(Bound(2), Bound(2)))
    yplus = Mult(Charclass((("y", "y"),)), PLUS)
    z = Mult(Charclass((("Z", "Z"),)), ONE)

    assert Conc(a, z).dock(Conc(z)) == Conc(a)
    assert Conc(a, b, x, yplus, z).dock(Conc(x, yplus, z)) == Conc(a, b)
    assert Conc(a, b, x, yplus, z).behead(Conc(a, b, x, yplus)) == Conc(z)
    assert Conc(a).dock(Conc()) == Conc(a)

    with pytest.raises(ArithmeticError, match="Can't subtract"):
        Conc(x_twice, yplus, z).behead(Conc(x, yplus))


def test_mult_reduction_easy() -> None:
    assert Conc(Mult(Charclass((("a", "a"),)), ZERO)).reduce() == Conc()

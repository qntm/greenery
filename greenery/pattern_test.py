# -*- coding: utf-8 -*-

from .charclass import Charclass
from .multiplier import ONE, ZERO
from .rxelems import Pattern, Conc, Mult, Multiplicand
from .parse import parse


def test_pattern_equality():
    assert Pattern(
        Conc(Mult(Multiplicand(Charclass("a")), ONE)),
        Conc(Mult(Multiplicand(Charclass("b")), ONE)),
    ) == Pattern(
        Conc(Mult(Multiplicand(Charclass("b")), ONE)),
        Conc(Mult(Multiplicand(Charclass("a")), ONE)),
    )
    assert Pattern(
        Conc(Mult(Multiplicand(Charclass("a")), ONE)),
        Conc(Mult(Multiplicand(Charclass("a")), ONE)),
    ) == Pattern(
        Conc(Mult(Multiplicand(Charclass("a")), ONE)),
    )


def test_pattern_str():
    assert str(Pattern(
        Conc(Mult(Multiplicand(Charclass("a")), ONE)),
        Conc(Mult(Multiplicand(Charclass("b")), ONE)),
    )) == "a|b"
    assert str(Pattern(
        Conc(Mult(Multiplicand(Charclass("a")), ONE)),
        Conc(Mult(Multiplicand(Charclass("a")), ONE)),
    )) == "a"
    assert str(Pattern(
        Conc(
            Mult(Multiplicand(Charclass("a")), ONE),
            Mult(Multiplicand(Charclass("b")), ONE),
            Mult(Multiplicand(Charclass("c")), ONE),
        ),
        Conc(
            Mult(Multiplicand(Charclass("d")), ONE),
            Mult(Multiplicand(Charclass("e")), ONE),
            Mult(Multiplicand(Charclass("f")), ONE),
            Mult(
                Multiplicand(
                    Pattern(
                        Conc(
                            Mult(Multiplicand(Charclass("g")), ONE),
                            Mult(Multiplicand(Charclass("h")), ONE),
                            Mult(Multiplicand(Charclass("i")), ONE),
                        ),
                        Conc(
                            Mult(Multiplicand(Charclass("j")), ONE),
                            Mult(Multiplicand(Charclass("k")), ONE),
                            Mult(Multiplicand(Charclass("l")), ONE),
                        ),
                    )
                ),
                ONE,
            ),
        ),
    )) == "abc|def(ghi|jkl)"


def test_empty():
    assert Pattern().empty()


def test_mult_reduction_easy():
    assert Pattern(Conc()).reduce() == Pattern(Conc())
    assert Pattern(
        Conc(
            Mult(
                Multiplicand(
                    Charclass("a")
                ),
                ZERO,
            )
        )
    ).reduce() == Pattern(
        Conc()
    )
    assert str(
        Pattern(
            Conc(
                Mult(
                    Multiplicand(
                        Charclass("a")
                    ),
                    ZERO,
                )
            )
        ).reduce()
    ) == ""


def test_empty_pattern_reduction():
    assert str(Pattern().reduce()) == "[]"


def test_empty_conc_suppression():
    assert str(Pattern(
        Conc(
            # this `Mult` can never actually match anything
            Mult(Multiplicand(Pattern()), ONE),
            Mult(Multiplicand(Charclass("0")), ONE),
            Mult(Multiplicand(Charclass("0123456789")), ONE),
        )  # so neither can this `Conc`
    ).reduce()) == "[]"


def test_pattern_dock():
    a = Mult(Multiplicand(Charclass("a")), ONE)
    c = Mult(Multiplicand(Charclass("c")), ONE)
    f = Mult(Multiplicand(Charclass("f")), ONE)

    assert parse("a|bc").dock(Conc()) == parse("a|bc")
    assert parse("aa|bca").dock(Conc(a)) == parse("a|bc")
    assert parse("xyza|abca|a").dock(Conc(a)) == parse("xyz|abc|")
    assert parse("f{2,3}c|fc").dock(Conc(f, c)) == parse("f{1,2}|")
    assert parse("aa").dock(Conc(a, a)) == parse("")


def test_pattern_beheading():
    a = Mult(Multiplicand(Charclass("a")), ONE)
    c = Mult(Multiplicand(Charclass("c")), ONE)
    f = Mult(Multiplicand(Charclass("f")), ONE)
    z = Mult(Multiplicand(Charclass("Z")), ONE)

    assert parse("aa").behead(Conc(a)) == parse("a")
    assert parse("abc|aa").behead(Conc(a)) == parse("a|bc")
    assert parse("cf{1,2}|cf").behead(Conc(c)) == parse("f{1,2}|f")
    assert parse("aa|aa").behead(Conc(a, a)) == parse("")
    assert parse("abc|aa").behead(Conc(a)) == parse("a|bc")
    assert parse("a|bc").behead(Conc()) == parse("a|bc")
    assert parse("cf{1,2}|cf").behead(Conc(c, f)) == parse("f?|")
    assert parse("ZA|ZB|ZC").behead(Conc(z)) == parse("A|B|C")
    assert parse("Z+A|ZB|ZZC").behead(Conc(z)) == parse("Z*A|B|ZC")
    assert parse("a{2}b|a+c").behead(Conc(a)) == parse("ab|a*c")

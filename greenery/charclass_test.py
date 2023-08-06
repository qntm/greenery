from __future__ import annotations

import pytest

from .charclass import (
    DIGIT,
    DOT,
    NONDIGITCHAR,
    NONSPACECHAR,
    NONWORDCHAR,
    NULLCHARCLASS,
    SPACECHAR,
    WORDCHAR,
    Charclass,
    repartition,
    add_ord_range,
)


def test_add_ord_range_0() -> None:
    assert add_ord_range([], False, (1, 2), False) == ([(1, 2)], False)


def test_add_ord_range_1A() -> None:
    assert add_ord_range(
        [(1, 1), (3, 4), (10, 11), (13, 17)],
        False,
        (7, 7),
        False
    ) == ([(1, 1), (3, 4), (7, 7), (10, 11), (13, 17)], False)

def test_add_ord_range_1B() -> None:
    assert add_ord_range([(5, 16)], False, (1, 1), False) == ([(1, 1), (5, 16)], False)
    assert add_ord_range([(5, 16)], False, (1, 2), False) == ([(1, 2), (5, 16)], False)
    assert add_ord_range([(5, 16)], False, (1, 3), False) == ([(1, 3), (5, 16)], False)
    assert add_ord_range([(5, 16)], False, (1, 4), False) == ([(1, 16)], False)
    assert add_ord_range([(5, 16)], False, (1, 5), False) == ([(1, 16)], False)
    assert add_ord_range([(5, 16)], False, (1, 16), False) == ([(1, 16)], False)
    assert add_ord_range([(5, 16)], False, (1, 17), False) == ([(1, 17)], False)
    assert add_ord_range([(5, 16)], False, (1, 18), False) == ([(1, 18)], False)
    assert add_ord_range([(5, 16)], False, (4, 4), False) == ([(4, 16)], False)
    assert add_ord_range([(5, 16)], False, (5, 5), False) == ([(5, 16)], False)
    assert add_ord_range([(5, 16)], False, (5, 18), False) == ([(5, 18)], False)
    assert add_ord_range([(5, 16)], False, (7, 8), False) == ([(5, 16)], False)
    assert add_ord_range([(5, 16)], False, (10, 20), False) == ([(5, 20)], False)
    assert add_ord_range([(5, 16)], False, (16, 20), False) == ([(5, 20)], False)
    assert add_ord_range([(5, 16)], False, (17, 20), False) == ([(5, 20)], False)
    assert add_ord_range([(5, 16)], False, (18, 20), False) == ([(5, 16), (18, 20)], False)


def test_add_ord_range_2() -> None:
    assert add_ord_range([(1, 2), (11, 12)], False, (5, 6), False) == ([(1, 2), (5, 6), (11, 12)], False)
    assert add_ord_range([(1, 2), (11, 12)], False, (3, 6), False) == ([(1, 6), (11, 12)], False)
    assert add_ord_range([(1, 2), (11, 12)], False, (2, 6), False) == ([(1, 6), (11, 12)], False)
    assert add_ord_range([(1, 2), (11, 12)], False, (5, 9), False) == ([(1, 2), (5, 9), (11, 12)], False)
    assert add_ord_range([(1, 2), (11, 12)], False, (5, 10), False) == ([(1, 2), (5, 12)], False)
    assert add_ord_range([(1, 2), (11, 12)], False, (-2, -1), False) == ([(-2, -1), (1, 2), (11, 12)], False)
    assert add_ord_range([(1, 2), (11, 12)], False, (0, 20), False) == ([(0, 20)], False)


def test_add_ord_range_0_n() -> None:
    assert add_ord_range([], False, (1, 2), True) == ([(1, 2)], True)


def test_add_ord_range_1A_n() -> None:
    assert add_ord_range(
        [(1, 1), (3, 4), (10, 11), (13, 17)],
        False,
        (7, 7),
        True
    ) == ([(7, 7)], True)

def test_add_ord_range_1B_n() -> None:
    assert add_ord_range([(5, 16)], False, (1, 1), True) == ([(1, 1)], True)
    assert add_ord_range([(5, 16)], False, (1, 2), True) == ([(1, 2)], True)
    assert add_ord_range([(5, 16)], False, (1, 3), True) == ([(1, 3)], True)
    assert add_ord_range([(5, 16)], False, (1, 4), True) == ([(1, 4)], True)
    assert add_ord_range([(5, 16)], False, (1, 5), True) == ([(1, 4)], True)
    assert add_ord_range([(5, 16)], False, (1, 16), True) == ([(1, 4)], True)
    assert add_ord_range([(5, 16)], False, (1, 17), True) == ([(1, 4), (17, 17)], True)
    assert add_ord_range([(5, 16)], False, (1, 18), True) == ([(1, 4), (17, 18)], True)
    assert add_ord_range([(5, 16)], False, (4, 4), True) == ([(4, 4)], True)
    assert add_ord_range([(5, 16)], False, (5, 5), True) == ([], True)
    assert add_ord_range([(5, 16)], False, (5, 18), True) == ([(17, 18)], True)
    assert add_ord_range([(5, 16)], False, (7, 8), True) == ([], True)
    assert add_ord_range([(5, 16)], False, (10, 20), True) == ([(17, 20)], True)
    assert add_ord_range([(5, 16)], False, (16, 20), True) == ([(17, 20)], True)
    assert add_ord_range([(5, 16)], False, (17, 20), True) == ([(17, 20)], True)
    assert add_ord_range([(5, 16)], False, (18, 20), True) == ([(18, 20)], True)


def test_add_ord_range_2_n() -> None:
    assert add_ord_range([(1, 2), (11, 12)], False, (5, 6), True) == ([(5, 6)], True)
    assert add_ord_range([(1, 2), (11, 12)], False, (3, 6), True) == ([(3, 6)], True)
    assert add_ord_range([(1, 2), (11, 12)], False, (2, 6), True) == ([(3, 6)], True)
    assert add_ord_range([(1, 2), (11, 12)], False, (5, 9), True) == ([(5, 9)], True)
    assert add_ord_range([(1, 2), (11, 12)], False, (5, 10), True) == ([(5, 10)], True)
    assert add_ord_range([(1, 2), (11, 12)], False, (-2, -1), True) == ([], True)
    assert add_ord_range([(1, 2), (11, 12)], False, (0, 20), True) == ([(0, 0), (3, 10), (13, 20)], True)


def test_charclass_equality() -> None:
    assert Charclass((("a", "a"),)) == Charclass((("a", "a"),))
    assert ~Charclass((("a", "a"),)) == ~Charclass((("a", "a"),))
    assert ~Charclass((("a", "a"),)) != Charclass((("a", "a"),))
    assert Charclass((("a", "a"), ("b", "b"))) == Charclass((("b", "b"), ("a", "a")))


def test_charclass_ctor() -> None:
    with pytest.raises(TypeError):
        Charclass(frozenset({"a", "aa"}))

    assert not Charclass((("a", "a"), ("b", "b"))).negated
    assert not Charclass((("a", "a"), ("b", "b")), negated=False).negated
    assert Charclass((("a", "a"), ("b", "b")), negated=True).negated


def test_repr() -> None:
    assert repr(~Charclass((("a", "a"),))) == "~Charclass((('a', 'a'),))"


def test_issubset() -> None:
    assert Charclass((("a", "a"),)).issubset(Charclass((("a", "a"),)))
    assert not Charclass((("a", "a"),)).issubset(Charclass((("b", "b"),)))
    assert Charclass((("a", "a"),)).issubset(Charclass((("a", "b"),)))
    assert Charclass((("a", "a"),)).issubset(~Charclass((("b", "b"),)))
    assert not (~Charclass((("a", "a"),))).issubset(Charclass((("b", "b"),)))
    assert ~Charclass((("a", "a"),)).issubset(DOT)


def test_charclass_str() -> None:
    assert str(WORDCHAR) == "\\w"
    assert str(DIGIT) == "\\d"
    assert str(SPACECHAR) == "\\s"
    assert str(Charclass((("a", "a"),))) == "a"
    assert str(Charclass((("{", "{"),))) == "\\{"
    assert str(Charclass((("\t", "\t"),))) == "\\t"
    assert str(Charclass((("a", "a"), ("b", "b")))) == "[ab]"
    assert str(Charclass((("a", "a"), ("{", "{")))) == "[a{]"
    assert str(Charclass((("a", "a"), ("\t", "\t")))) == "[\\ta]"
    assert str(Charclass((("a", "a"), ("-", "-")))) == "[\\-a]"
    assert str(Charclass((("a", "a"), ("[", "[")))) == "[\\[a]"
    assert str(Charclass((("a", "a"), ("]", "]")))) == "[\\]a]"
    assert str(Charclass((("a", "a"), ("b", "b")))) == "[ab]"
    assert str(Charclass((("a", "a"), ("b", "b"), ("c", "c")))) == "[abc]"
    assert str(Charclass((("a", "a"), ("b", "b"), ("c", "c"), ("d", "d")))) == "[a-d]"
    assert str(Charclass((
        ("a", "a"),
        ("b", "b"),
        ("c", "c"),
        ("d", "d"),
        ("f", "f"),
        ("g", "g"),
        ("h", "h"),
        ("i", "i"),
    ))) == "[a-df-i]"
    assert str(Charclass((("^", "^"),))) == "^"
    assert str(Charclass((("\\", "\\"),))) == "\\\\"
    assert str(Charclass((("a", "a"), ("^", "^")))) == "[\\^a]"
    assert str(Charclass(
        tuple((char, char) for char in "0123456789a")
    )) == "[0-9a]"
    assert str(Charclass((
        ("\t", "\t"),
        ("\v", "\v"),
        ("\r", "\r"),
        (" ", " "),
        ("A", "A"),
    ))) == "[\\t\\v\\r A]"
    assert str(Charclass((
        ("\n", "\n"),
        ("\f", "\f"),
        (" ", " "),
        ("A", "A"),
    ))) == "[\\n\\f A]"
    assert str(Charclass((
        ("\t", "\t"),
        ("\n", "\n"),
        ("\v", "\v"),
        ("\f", "\f"),
        ("\r", "\r"),
        (" ", " "),
        ("A", "A"),
    ))) == "[\\t-\\r A]"
    assert str(Charclass(
        tuple((char, char) for char in "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz|")
    )) == "[0-9A-Z_a-z|]"
    assert str(NONWORDCHAR) == "\\W"
    assert str(NONDIGITCHAR) == "\\D"
    assert str(NONSPACECHAR) == "\\S"
    assert str(DOT) == "."
    assert str(~Charclass(())) == "."
    assert str(~Charclass((("a", "a"),))) == "[^a]"
    assert str(~Charclass((("{", "{"),))) == "[^{]"
    assert str(~Charclass((("\t", "\t"),))) == "[^\\t]"
    assert str(~Charclass((("^", "^"),))) == "[^\\^]"


def test_charclass_negation() -> None:
    assert ~~Charclass((("a", "a"),)) == Charclass((("a", "a"),))
    assert Charclass((("a", "a"),)) == ~~Charclass((("a", "a"),))


def test_empty() -> None:
    assert NULLCHARCLASS.empty()
    assert not DOT.empty()


def test_repartition_elementary() -> None:
    assert repartition([
        Charclass((("a", "a"),))
    ]) == {
        Charclass((("a", "a"),)): [
            Charclass((("a", "a"),))
        ],
    }
    assert repartition([
        Charclass((("a", "a"),)),
        ~Charclass((("a", "a"),))
    ]) == {
        Charclass((("a", "a"),)): [
            Charclass((("a", "a"),))
        ],
        ~Charclass((("a", "a"),)): [
            ~Charclass((("a", "a"),))
        ],
    }


def test_repartition_basic() -> None:
    assert repartition([
        Charclass((("a", "a"),)),
        Charclass((("a", "a"), ("b", "b"), ("c", "c")))
    ]) == {
        Charclass((("a", "a"),)): [
            Charclass((("a", "a"),)),
        ],
        Charclass((("a", "a"), ("b", "b"), ("c", "c"))): [
            Charclass((("a", "a"),)),
            Charclass((("b", "b"), ("c", "c"))),
        ],
    }


def test_repartition_negation() -> None:
    assert repartition([
        Charclass((("a", "a"), ("b", "b"))),
        Charclass((("a", "a"),)),
        ~Charclass((("a", "a"), ("b", "b")))
    ]) == {
        Charclass((("a", "a"), ("b", "b"))): [
            Charclass((("a", "a"),)),
            Charclass((("b", "b"),)),
        ],
        Charclass((("a", "a"),)): [
            Charclass((("a", "a"),)),
        ],
        ~Charclass((("a", "a"), ("b", "b"))): [
            ~Charclass((("a", "a"), ("b", "b"))),
        ],
    }
    assert repartition([
        Charclass((("a", "a"), ("b", "b"))),
        Charclass((("a", "a"), ("b", "b"), ("c", "c"))),
        ~Charclass((("a", "a"), ("b", "b")))
    ]) == {
        Charclass((("a", "a"), ("b", "b"))): [
            Charclass((("a", "a"), ("b", "b"))),
        ],
        Charclass((("a", "a"), ("b", "b"), ("c", "c"))): [
            Charclass((("a", "a"), ("b", "b"))),
            Charclass((("c", "c"),)),
        ],
        ~Charclass((("a", "a"), ("b", "b"))): [
            Charclass((("c", "c"),)),
            ~Charclass((("a", "a"), ("b", "b"), ("c", "c"))),
        ],
    }
    assert repartition([
        ~Charclass((("a", "a"),)),
        ~Charclass((("a", "a"), ("b", "b"))),
        ~Charclass((("a", "a"), ("b", "b"), ("c", "c"))),
    ]) == {
        ~Charclass((("a", "a"),)): [
            Charclass((("b", "b"),)),
            Charclass((("c", "c"),)),
            ~Charclass((("a", "a"), ("b", "b"), ("c", "c"))),
        ],
        ~Charclass((("a", "a"), ("b", "b"))): [
            Charclass((("c", "c"),)),
            ~Charclass((("a", "a"), ("b", "b"), ("c", "c"))),
        ],
        ~Charclass((("a", "a"), ("b", "b"), ("c", "c"))): [
            ~Charclass((("a", "a"), ("b", "b"), ("c", "c"))),
        ],
    }


def test_repartition_advanced() -> None:
    assert repartition(
        [
            Charclass((("a", "a"),)),
            Charclass((("b", "b"), ("c", "c"), ("d", "d"), ("e", "e"), ("f", "f"))),
            ~Charclass((("a", "a"), ("b", "b"), ("c", "c"), ("d", "d"), ("e", "e"), ("f", "f"))),
            Charclass((("a", "a"), ("b", "b"), ("c", "c"), ("d", "d"))),
            ~Charclass((("a", "a"), ("b", "b"), ("c", "c"), ("d", "d"))),
        ]
    ) == {
        Charclass((("a", "a"),)): [
            Charclass((("a", "a"),))
        ],
        Charclass((("b", "b"), ("c", "c"), ("d", "d"), ("e", "e"), ("f", "f"))): [
            Charclass((("b", "b"), ("c", "c"), ("d", "d"))),
            Charclass((("e", "e"), ("f", "f"))),
        ],
        ~Charclass((("a", "a"), ("b", "b"), ("c", "c"), ("d", "d"), ("e", "e"), ("f", "f"))): [
            ~Charclass((("a", "a"), ("b", "b"), ("c", "c"), ("d", "d"), ("e", "e"), ("f", "f"))),
        ],
        Charclass((("a", "a"), ("b", "b"), ("c", "c"), ("d", "d"))): [
            Charclass((("a", "a"),)),
            Charclass((("b", "b"), ("c", "c"), ("d", "d"))),
        ],
        ~Charclass((("a", "a"), ("b", "b"), ("c", "c"), ("d", "d"))): [
            Charclass((("e", "e"), ("f", "f"))),
            ~Charclass((("a", "a"), ("b", "b"), ("c", "c"), ("d", "d"), ("e", "e"), ("f", "f"))),
        ],
    }
    assert repartition([WORDCHAR, DIGIT, DOT, NONDIGITCHAR, NULLCHARCLASS]) == {
        WORDCHAR: [
            DIGIT,
            Charclass(
                tuple((char, char) for char in "ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz")
            ),
        ],
        DIGIT: [DIGIT],
        DOT: [
            DIGIT,
            Charclass(
                tuple((char, char) for char in "ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz")
            ),
            NONWORDCHAR,
        ],
        NONDIGITCHAR: [
            Charclass(
                tuple((char, char) for char in "ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz")
            ),
            NONWORDCHAR,
        ],
        # Yup, there's nothing here!
        # This should be impossible or at least cause no problems in practice
        NULLCHARCLASS: [],
    }

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
)


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


def test_charclass_union() -> None:
    # [ab] ∪ [bc] = [abc]
    assert Charclass((("a", "a"), ("b", "b"))) | Charclass((("b", "b"), ("c", "c"))) == Charclass((("a", "a"), ("b", "b"), ("c", "c")))
    # [ab] ∪ [^bc] = [^c]
    assert Charclass((("a", "a"), ("b", "b"))) | ~Charclass((("b", "b"), ("c", "c"))) == ~Charclass((("c", "c"),))
    # [^a] ∪ [bc] = [^a]
    assert ~Charclass((("a", "a"), ("b", "b"))) | Charclass((("b", "b"), ("c", "c"))) == ~Charclass((("a", "a"),))
    # [^ab] ∪ [^bc] = [^b]
    assert ~Charclass((("a", "a"), ("b", "b"))) | ~Charclass((("b", "b"), ("c", "c"))) == ~Charclass((("b", "b"),))

    assert Charclass.union() == NULLCHARCLASS

    assert Charclass.union(
        Charclass((("a", "a"), ("b", "b"))),
        Charclass((("a", "a"),)),
        Charclass((("c", "c"), ("d", "d"))),
    ) == Charclass((("a", "a"), ("b", "b"), ("c", "c"), ("d", "d")))

    assert Charclass.union(
        Charclass((("a", "a"), ("b", "b"))),
        ~Charclass((("a", "a"), ("b", "b"), ("c", "c"))),
    ) == ~Charclass((("c", "c"),))


def test_charclass_intersection() -> None:
    # [ab] ∩ [bc] = [b]
    assert Charclass((("a", "a"), ("b", "b"))) & Charclass((("b", "b"), ("c", "c"))) == Charclass((("b", "b"),))
    # [ab] ∩ [^bc] = [a]
    assert Charclass((("a", "a"), ("b", "b"))) & ~Charclass((("b", "b"), ("c", "c"))) == Charclass((("a", "a"),))
    # [^ab] ∩ [bc] = [c]
    assert ~Charclass((("a", "a"), ("b", "b"))) & Charclass((("b", "b"), ("c", "c"))) == Charclass((("c", "c"),))
    # [^ab] ∩ [^bc] = [^abc]
    assert ~Charclass((("a", "a"), ("b", "b"))) & ~Charclass((("b", "b"), ("c", "c"))) == ~Charclass((("a", "a"), ("b", "b"), ("c", "c")))

    assert Charclass.intersection(
        Charclass((("a", "a"), ("b", "b"))),
        Charclass((("b", "b"), ("c", "c"), ("d", "d"))),
        Charclass((("a", "a"), ("b", "b"), ("c", "c"), ("d", "d"), ("e", "e"))),
    ) == Charclass((("b", "b"),))

    assert Charclass.intersection() == ~NULLCHARCLASS
    assert NULLCHARCLASS & NULLCHARCLASS == NULLCHARCLASS


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
